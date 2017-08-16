#include "SutraDataSource.h"
//#include "SutraReader.h"
//#include "SutraReader2.h"
//#include "SutraReader21.h"
#include "SutraReader22.h"
#include "mvUtil.h"
#include <fstream.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <vtkIntArray.h>
#include <vtkPolyData.h>
#include "mvExternalMeshVector.h"
#include "mvExternalMesh.h"


SutraDataSource::SutraDataSource() : mvDataSource()
{
	m_FileType = 0;
	m_In1 = 0;
	m_In2 = 0;
	m_NumberOfNodes = 0;
	m_NumberOfElements = 0;
	m_Incidence = 0;
	m_UseExternalFile = 0;
	m_GridType = MV_UNSTRUCTED_GRID;
	m_LayeredMesh = FALSE;
}

SutraDataSource::~SutraDataSource()
{
	ReleaseMemory();
	if (m_In1)
	{
		if (m_In1->is_open())
		{
			m_In1->close();
		}
		delete m_In1;
	}
	if (m_In2)
	{
		if (m_In2->is_open())
		{
			m_In2->close();
		}
		delete m_In2;
	}
}

void SutraDataSource::GetDefaultModelFeatureColor(int i, float *rgba)
{
	float m_Rgba[MAXIMUM_NUMBER_OF_SUTRA_MODEL_FEATURE_TYPES][4] = {
		{0.5, 1.0, 0.5, 1},		// Fluid source/sink -- light green
		{1.0, 1.0, 0.5, 1},		// Energy or solute source/sink  --  yellow
		{0.5, 1.0, 1.0, 1},		// Specified Pressure -- light blue
		{1.0, 0.5, 0.5, 1}		// Specified conc or temp --  pink
	};

	rgba[0] = m_Rgba[i][0];
	rgba[1] = m_Rgba[i][1];
	rgba[2] = m_Rgba[i][2];
	rgba[3] = m_Rgba[i][3];
}

char *SutraDataSource::LoadData(char *dataFileList)
{

	// Parse the data file list;
	char inpFile[256];
	char code[4];
	char timeUnits[4];
	char *pList = dataFileList;
	char *errmsg;
	char externalFile[256];
	int ISTEADYFLOW, ISTEADYTRANSPORT;
	ParseDataFileList(pList, m_NodFile);
	ParseDataFileList(pList, m_EleFile);
	ParseDataFileList(pList, inpFile);

	int ExtFileLength;
	if (m_UseExternalFile)
	{
		ParseDataFileList(pList, externalFile);
		ExtFileLength = strlen(externalFile);
	}
	else
	{
		ExtFileLength = 0;
	}


	ParseDataFileList(pList, code);
	if (*pList != 0)
	{
		ParseDataFileList(pList, timeUnits);
		m_TimeUnits = atoi(timeUnits);
	}
	else
	{
		m_TimeUnits = 0;
	}
	m_FileType = atoi(code);

	// check that the external data file exists.
	if (ExtFileLength > 0)
	{
		ifstream in(externalFile, ios::in|ios::nocreate);
		if (!in.is_open())
		{
			return "Unable to open the external data file.";
		}
	    in.close();
	}

	if (m_FileType == -1)	// ascii data
	{
		bool inputread = false;
		ISTEADYFLOW = 1;
		ISTEADYTRANSPORT = 1;

		// Read the nod file
		m_In1 = new ifstream;
		m_In1->open(m_NodFile, ios::in|ios::nocreate);
		if (!m_In1->is_open())
		{
			return "Unable to open nod file.";
		}
		// Note that ReadNodFile will allocate memory for m_TimePointLabels, 
		// m_DataTypeLabels, and m_ScalarGridCoordinates
		errmsg = ReadNodFile(*m_In1, m_ScalarGridDim, m_NumberOfNodes, m_NumberOfElements,
				m_NumberOfTimePoints, m_IceSatFraction,  m_TimePointLabels, m_NumberOfScalarDataTypes, 
				m_DataTypeLabels, m_ScalarGridCoordinates, m_TimeUnits);
		if (errmsg)
		{
			return errmsg;
		}
		if (m_ScalarGridDim[1] == 0)
		{
			m_GridType = MV_UNSTRUCTED_GRID;
		}
		else
		{
			m_GridType = MV_STRUCTURED_GRID_ALL_ACTIVE;
		}
		m_NumberOfNodalScalarDataTypes = m_NumberOfScalarDataTypes;

		// Read the ele file if used
		if (strlen(m_EleFile) > 0)
		{
			m_In2 = new ifstream;
			m_In2->open(m_EleFile, ios::in|ios::nocreate);
			if (!m_In2->is_open())
			{
				return "Unable to open ele file.";
			}
			if (m_NumberOfElements == 0)
			{
				m_VectorGridCoordinates = new float[3*m_NumberOfNodes];
			}
			else
			{
				m_VectorGridCoordinates = new float[3*m_NumberOfElements];
			}
			errmsg = ReadEleFile(*m_In2, m_ScalarGridDim, m_NumberOfTimePoints,
								m_TimePointLabels, m_VectorGridCoordinates,
								inputread, ISTEADYFLOW, ISTEADYTRANSPORT, 
								m_NumberOfElementTimePoints, m_NumberOfElements, m_TimeUnits);

			if (errmsg)
			{
				return errmsg;
			}
			m_VectorGridDim[0] = m_ScalarGridDim[0]-1;
			m_VectorGridDim[1] = m_ScalarGridDim[1]-1;
			if (m_ScalarGridDim[2] == 1) // 2D case
			{
				m_VectorGridDim[2] = 1;
			}
			else
			{
				m_VectorGridDim[2] = m_ScalarGridDim[2]-1;
			}
			m_VectorArray = new float[3*m_NumberOfElements];
		}

		int ScalarArraySize = (m_NumberOfNodes + m_NumberOfElements) 
			* (m_NumberOfScalarDataTypes+ADDITIONAL_ELEMENT_DATA_SETS+ADDITIONAL_NODE_DATA_SETS);
		m_ScalarArray = new float[ScalarArraySize];


		// Read the inp file if used
		m_NumberOfModelFeatureTypes = 0;

		int len = strlen(inpFile);
		if (len > 0)
		{
			inputread = true;
			int ierror, istart, ibousz;
			istart = 0;
			
			int ElementScalarArraySize = m_NumberOfElements*ADDITIONAL_ELEMENT_DATA_SETS;
			float *ElementScalarArray = new float[ElementScalarArraySize];
			
			int NodeScalarArraySize = m_NumberOfNodes;
			float *NodeScalarArray = new float[NodeScalarArraySize];

//			sutra_2d3d_(&ierror, &istart, &ibousz, 0 /*not used*/, 
//				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, inpFile, len);
//			sutra_2d3d2_(&ierror, &istart, &ibousz, 0 /*not used*/, 
//				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT,  
//				ElementScalarArray, &ElementScalarArraySize, m_Incidence, NodeScalarArray,
//				&NodeScalarArraySize, inpFile, len);
//			sutra_21_(&ierror, &istart, &ibousz, 0 /*not used*/, 
//				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT,  
//				ElementScalarArray, &ElementScalarArraySize, m_Incidence, NodeScalarArray,
//				&NodeScalarArraySize, &m_MeshInfo[0], inpFile, len);
			SUTRA_22_(&ierror, &istart, &ibousz, 0 /*not used*/, 
				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT,  
				ElementScalarArray, &ElementScalarArraySize, m_Incidence, NodeScalarArray,
				&NodeScalarArraySize, &m_MeshInfo[0], inpFile, len);
			if (ierror)
			{
				delete [] ElementScalarArray;
				delete [] NodeScalarArray;
				if (ierror == -1)
				{
					return "Unable to open the inp file";
				}
				else
				{
					return "Error encountered while reading the inp file";
				}
			}
			m_ModelFeatureArray = new int[ibousz];

			if (m_Incidence != 0)
			{
				delete [] m_Incidence;
				m_Incidence = 0;
			}
			m_Incidence = new int[m_NumberOfElements*8];
			
			istart = 1;
//			sutra_2d3d_(&ierror, &istart, &ibousz, m_ModelFeatureArray, 
//				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, inpFile, len);
//			sutra_2d3d2_(&ierror, &istart, &ibousz, m_ModelFeatureArray, 
//				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, 
//				ElementScalarArray, &ElementScalarArraySize, m_Incidence,NodeScalarArray,
//				&NodeScalarArraySize, inpFile, len);
//			sutra_21_(&ierror, &istart, &ibousz, m_ModelFeatureArray, 
//				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, 
//				ElementScalarArray, &ElementScalarArraySize, m_Incidence,NodeScalarArray,
//				&NodeScalarArraySize, &m_MeshInfo[0], inpFile, len);
			SUTRA_22_(&ierror, &istart, &ibousz, m_ModelFeatureArray, 
				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, 
				ElementScalarArray, &ElementScalarArraySize, m_Incidence,NodeScalarArray,
				&NodeScalarArraySize, &m_MeshInfo[0], inpFile, len);
			if (ierror)
			{
				delete [] ElementScalarArray;
				delete [] NodeScalarArray;
				return "Error encountered while reading the inp file";
			}


			int *ElementCount= new int[m_NumberOfNodes];
			
			int NodeCount;
			int Node;
			int NumberOfElementDataSets = 0;
			int NumberOfNodeDataSets = 1;
			if (m_ScalarGridDim[2] == 1)   // 2D case
			{ 
				NodeCount = 4;
				NumberOfElementDataSets = ADDITIONAL_2D_ELEMENT_DATA_SETS-VECTOR_DATA_SETS;
			}
			else
			{
				NodeCount = 8;
				NumberOfElementDataSets = ADDITIONAL_ELEMENT_DATA_SETS-VECTOR_DATA_SETS;
			}

			// Element Count stores the number of elements that each node is a part of.
			for (int i=0; i<m_NumberOfNodes; i++)
			{
				ElementCount[i] = 0;
			}

			int Offset = (m_NumberOfNodes + m_NumberOfElements) 
				* m_NumberOfNodalScalarDataTypes;

			// initialize node data for data that is originally stored by element (SUTRA Data set 15).
			for (int DataSetIndex = 0; DataSetIndex < NumberOfElementDataSets; DataSetIndex++)
			{
				for (i=0; i<m_NumberOfNodes; i++)
				{
					if (i+Offset >= ScalarArraySize)
					{
						return "Error in initializing array.";
					}
					m_ScalarArray[i+Offset] = 0;
				}
				Offset += m_NumberOfNodes + m_NumberOfElements;
			}

			// initialize node data for data that is originally stored by node (SUTRA Data set 14).
			Offset += m_NumberOfNodes;
			for (DataSetIndex = 0; DataSetIndex < NumberOfNodeDataSets; DataSetIndex++)
			{
				for (i=0; i<m_NumberOfElements; i++)
				{
					if (i+Offset >= ScalarArraySize)
					{
						return "Error in initializing array.";
					}
					m_ScalarArray[i+Offset] = 0;
				}
				Offset += m_NumberOfNodes + m_NumberOfElements;
			}

			Offset = (m_NumberOfNodes + m_NumberOfElements) 
				* m_NumberOfNodalScalarDataTypes + m_NumberOfNodes;

			for (DataSetIndex = 0; DataSetIndex < NumberOfElementDataSets; DataSetIndex++)
			{
				int NodeI = -1;
				for (int j=0; j<m_NumberOfElements; j++)
				{
					if (j+Offset >= ScalarArraySize)
					{
						return "Error in setting array.";
					}
					m_ScalarArray[j+Offset] = ElementScalarArray[j + DataSetIndex*m_NumberOfElements];
					for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
					{
						NodeI += 1;
						Node = m_Incidence[NodeI];
						if ((Node < 0) || (Node >= m_NumberOfNodes))
						{
							delete [] ElementScalarArray;
							return "Error encountered while reading the inp file";
						}
						if (Node+Offset-m_NumberOfNodes >= ScalarArraySize)
						{
							return "Error in setting array.";
						}
						if (Node+Offset-m_NumberOfNodes < 0)
						{
							return "Error in setting array.";
						}
						if (j + DataSetIndex*m_NumberOfElements >= ElementScalarArraySize)
						{
							return "Error in setting array.";
						}

						
						m_ScalarArray[Node+Offset-m_NumberOfNodes] += ElementScalarArray[j + DataSetIndex*m_NumberOfElements];
						if (DataSetIndex == 0)
						{
							ElementCount[Node] += 1;
						}
					}
				}
				Offset += m_NumberOfNodes + m_NumberOfElements;
			}
			
			Offset = (m_NumberOfNodes + m_NumberOfElements) 
				* m_NumberOfNodalScalarDataTypes;

			for (DataSetIndex = 0; DataSetIndex < NumberOfElementDataSets; DataSetIndex++)
			{
				for (int k=0; k<m_NumberOfNodes; k++)
				{
					if (ElementCount[k] <= 0)
					{
						delete [] ElementScalarArray;
						return "Error encountered while reading the inp file";
					}
					if (k+Offset >= ScalarArraySize)
					{
						return "Error in setting array.";
					}
					m_ScalarArray[k+Offset] = 
						m_ScalarArray[k+Offset] / ElementCount[k];
				}
				Offset += m_NumberOfNodes + m_NumberOfElements;
			}

			for (DataSetIndex = 0; DataSetIndex < NumberOfNodeDataSets; DataSetIndex++)
			{
				for (int j=0; j<m_NumberOfNodes; j++)
				{
					if (j+Offset >= ScalarArraySize)
					{
						return "Error in setting array.";
					}
					if (j + DataSetIndex*m_NumberOfNodes >= NodeScalarArraySize)
					{
						return "Error in setting array.";
					}
					m_ScalarArray[j+Offset] = NodeScalarArray[j + DataSetIndex*m_NumberOfNodes];
				}
				Offset += m_NumberOfNodes + m_NumberOfElements;
			}

			Offset = (m_NumberOfNodes + m_NumberOfElements) 
				* (m_NumberOfNodalScalarDataTypes+NumberOfElementDataSets)
				+ m_NumberOfNodes;
			for (DataSetIndex = 0; DataSetIndex < NumberOfNodeDataSets; DataSetIndex++)
			{
				int NodeI = -1;
				for (int j=0; j<m_NumberOfElements; j++)
				{
					if (j+Offset >= ScalarArraySize)
					{
						return "Error in setting array.";
					}
					if (Node + DataSetIndex*m_NumberOfElements >= NodeScalarArraySize)
					{
						return "Error in setting array.";
					}
					for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
					{
						NodeI +=1;
						Node = m_Incidence[NodeI];
						m_ScalarArray[j+Offset] += 
							NodeScalarArray[Node + DataSetIndex*m_NumberOfElements];
					}
					m_ScalarArray[j+Offset] /= NodeCount;
				}
				Offset += m_NumberOfNodes + m_NumberOfElements;
			}
			
			delete [] ElementScalarArray;
			delete [] NodeScalarArray;

			delete [] ElementCount;
				
			len = m_NumberOfModelFeatureTypes*40;
			m_ModelFeatureLabels = new char[len];
			memset(m_ModelFeatureLabels, ' ', len);
//			sutra_labels_(m_ModelFeatureLabels, len);
//			sutra_labels2_(m_ModelFeatureLabels, len);
//			sutra_labels21_(m_ModelFeatureLabels, len);
			SUTRA_LABELS22_(m_ModelFeatureLabels, len);

			m_NumberOfScalarDataTypes = m_NumberOfScalarDataTypes +
				NumberOfElementDataSets + NumberOfNodeDataSets + VECTOR_DATA_SETS;
		}

	}
	else	// binary data
	{
		m_In1 = new ifstream;
		m_In1->open(m_NodFile, ios::in|ios::binary|ios::nocreate);
		if (!m_In1->is_open())
		{
			return "Unable to open binary data file.";
		}
		errmsg = LoadBinaryFile();
		if (errmsg)
		{
			return errmsg;
		}
	}
		
	m_ScalarArrayOffset = 0;

	m_DataFileList = new char[strlen(dataFileList) + 1];
	strcpy(m_DataFileList, dataFileList);

	return 0;
}

void SutraDataSource::AdvanceOneTimePoint()
{
	SetTimePointTo(-1);
}

void SutraDataSource::SetTimePointTo(int timePointIndex)
{
	char line[1000];
	int i, j;
	float x, y, z;
    float *Scalars;
	int NodalDataSetsToRead;
	float TotalSat;
	if (timePointIndex != -1)
	{
		// Start from beginning of data stream
		if (m_In1->is_open())
		{
			m_In1->close();
		}
		if (m_FileType == -1)			// ascii data
		{
			m_In1->open(m_NodFile, ios::in|ios::nocreate);
			// skip header material
			for (i=0; i<m_NumberOfTimePoints+12; i++)
			{
				m_In1->getline(line, 1000);
			}
			// skip to starting time step
			for (i=0; i<timePointIndex; i++)
			{
				for (j=0; j<m_NumberOfNodes + 5; j++)
				{
					m_In1->getline(line, 1000);
				}
			}
			if (m_VectorArray != 0)
			{
				if (m_In2->is_open())
				{
					m_In2->close();
				}
				m_In2->open(m_EleFile, ios::in|ios::nocreate);
				// Skip header
				for (i=0; i<m_NumberOfElementTimePoints+12; i++)
				{
					m_In2->getline(line, 1000);
				}
				// Skip to starting time step
				if (m_NumberOfTimePoints == m_NumberOfElementTimePoints)
				{
					for (i=0; (i<timePointIndex)&&(i<m_NumberOfElementTimePoints-1); i++)
					{
						for (j=0; j<m_NumberOfElements + 5; j++)
						{
							m_In2->getline(line, 1000);
						}
					}
				}
				else
				{
					for (i=0; (i<timePointIndex-1)&&(i<m_NumberOfElementTimePoints-1); i++)
					{
						for (j=0; j<m_NumberOfElements + 5; j++)
						{
							m_In2->getline(line, 1000);
						}
					}
				}
			}
		}
		else		// binary data
		{
			m_In1->open(m_NodFile, ios::in|ios::nocreate|ios::binary);
			// skip over the header
			char aString[20];
			int n[3];
			m_In1->read((unsigned char *) aString, 20*sizeof(char));
			// skip over the grid info
			m_In1->read((unsigned char *) &n, 3*sizeof(int));
			if (m_version >= 2.0)
			{
				// skip the number of nodes 
				m_In1->read((unsigned char *) &n, sizeof(int));
				// skip the number of elements
				m_In1->read((unsigned char *) &n, sizeof(int));
				// skip the number of nodal data types 
				m_In1->read((unsigned char *) &n, sizeof(int));
				// skip the number of data types 
				m_In1->read((unsigned char *) &n, sizeof(int));
			}
			else
			{
				// skip the number of data types 
				m_In1->read((unsigned char *) &n, sizeof(int));
			}
			// skip over data type labels
			for (i=0; i<m_NumberOfScalarDataTypes; i++)
			{
				m_In1->read((unsigned char *) aString, 20*sizeof(char));
			}
			// skip over the time point labels
			m_In1->read((unsigned char *) &n, sizeof(int));
			for (i=0; i<m_NumberOfTimePoints; i++)
			{

				if (m_version < 2)
				{
					m_In1->read((unsigned char *) aString, 20*sizeof(char));
				}
				else
				{
					m_In1->read((unsigned char *) aString, TIME_LABEL_SIZE*sizeof(char));
				}
			}
			// skip over the scalar grid coordinates and velocity grid coordinates
			float *array = new float[m_NumberOfNodes*3];
			m_In1->read((unsigned char *) array, 3*m_NumberOfNodes*sizeof(float));
			m_In1->read((unsigned char *) &n, sizeof(int));
			if (m_VectorArray != 0)
			{
				m_In1->read((unsigned char *) array, 3*m_NumberOfElements*sizeof(float));
			}
			delete [] array;
			if (m_version >= 2)
			{
				if (m_ScalarGridDim[2] == 1)
				{
					//2D
					int* intarray = new int[4*m_NumberOfElements];
					m_In1->read((unsigned char *) intarray, 4*m_NumberOfElements*sizeof(int));
					delete [] intarray;
				}
				else
				{
					//3D
					int* intarray = new int[8*m_NumberOfElements];
					m_In1->read((unsigned char *) intarray, 8*m_NumberOfElements*sizeof(int));
					delete [] intarray;
				}
			}

			// skip over model features
			int nfeatures = 0;
			int ibousz = 0;
			m_In1->read((unsigned char *) &nfeatures, sizeof(int));
			if (nfeatures > 0)
			{
				char *flabels = new char[40*nfeatures];
				m_In1->read((unsigned char *) flabels, 40*nfeatures*sizeof(char));
				delete [] flabels;
				m_In1->read((unsigned char *) &ibousz, sizeof(int));
				if (ibousz > 0)
				{
					int *ibnode = new int[ibousz];
					m_In1->read((unsigned char *) ibnode, ibousz*sizeof(int));
					delete [] ibnode;
				}
			}

			// skip data for elements
			int count = 0;

			if (m_version >= 2)
			{
				m_In1->read((unsigned char *) &count, sizeof(int));
			}

			Scalars = new float[count*(m_NumberOfNodes + m_NumberOfElements)];

			if (timePointIndex == 0)
			{
				for (j = 0; j<count; j++)
				{
					m_In1->read((unsigned char *) Scalars, 
						(m_NumberOfNodes+m_NumberOfElements)*sizeof(float));
				}
			}

			delete [] Scalars;

			if (m_version >= 2)
			{
				// skip mesh info
				int dummy;
				m_In1->read((unsigned char *) &dummy, sizeof(int));
				m_In1->read((unsigned char *) &dummy, sizeof(int));
				m_In1->read((unsigned char *) &dummy, sizeof(int));
			}

			// skip data
			for (i=0; i<timePointIndex; i++)
			{
				if (m_version >= 2)
				{
					m_In1->read((unsigned char *) m_ScalarArray, 
						m_NumberOfNodalScalarDataTypes
						*(m_NumberOfNodes + m_NumberOfElements)*sizeof(float));
				}
				else
				{
					m_In1->read((unsigned char *) m_ScalarArray, 
						m_NumberOfNodalScalarDataTypes
						*m_NumberOfNodes*sizeof(float));
				}

				if (m_VectorArray != 0)
				{
					m_In1->read((unsigned char *) m_VectorArray, 3*m_NumberOfElements*sizeof(float));
				}
			}

		}
	}

	// read data
	if (m_FileType == -1)		// ascii data
	{
		if (m_IceSatFraction)
		{
			NodalDataSetsToRead = m_NumberOfNodalScalarDataTypes-2;
		}
		else
		{
			NodalDataSetsToRead = m_NumberOfNodalScalarDataTypes;
		}
		char FirstNumber[1000];
		for (j=0; j< 5; j++)
		{
			m_In1->getline(line, 1000);
		}
		if (m_ScalarGridDim[2] == 1)   // 2D case
		{
			for (i=0; i<m_NumberOfNodes; i++)
			{
				m_In1->getline(line, 1000);
				mvUtil::ExtractFirstString(line, FirstNumber);
				x = mvUtil::Fortran_atof(FirstNumber);

				mvUtil::ExtractFirstString(line, FirstNumber);
				y = mvUtil::Fortran_atof(FirstNumber);



				for (j=0; j<NodalDataSetsToRead; j++)
				{
					mvUtil::ExtractFirstString(line, FirstNumber);
					m_ScalarArray[i + j*(m_NumberOfNodes+m_NumberOfElements)] = mvUtil::Fortran_atof(FirstNumber);
//					(*m_In1) >> m_ScalarArray[i + j*(m_NumberOfNodes+m_NumberOfElements)];
				}
				if (m_IceSatFraction)
				{
					TotalSat = m_ScalarArray[i + (NodalDataSetsToRead-1)*(m_NumberOfNodes+m_NumberOfElements)]
						     + m_ScalarArray[i + (NodalDataSetsToRead-2)*(m_NumberOfNodes+m_NumberOfElements)];
					m_ScalarArray[i + NodalDataSetsToRead*(m_NumberOfNodes+m_NumberOfElements)] = TotalSat;
					if (TotalSat == 0)
					{
						m_ScalarArray[i + (NodalDataSetsToRead+1)*(m_NumberOfNodes+m_NumberOfElements)] = 0;
					}
					else
					{
						m_ScalarArray[i + (NodalDataSetsToRead+1)*(m_NumberOfNodes+m_NumberOfElements)] = 
							m_ScalarArray[i + (NodalDataSetsToRead-1)*(m_NumberOfNodes+m_NumberOfElements)]/TotalSat;
					}
				}



/*				(*m_In1) >> x >> y;
				for (j=0; j<m_NumberOfNodalScalarDataTypes; j++)
				{
					(*m_In1) >> m_ScalarArray[i + j*(m_NumberOfNodes+m_NumberOfElements)];
				}
				m_In1->getline(line, 1000);
*/
			}
		}
		else
		{
			for (i=0; i<m_NumberOfNodes; i++)
			{
				(*m_In1) >> x >> y >> z;

				
				for (j=0; j<NodalDataSetsToRead; j++)
				{
					(*m_In1) >> m_ScalarArray[i + j*(m_NumberOfNodes+m_NumberOfElements)];
				}

				if (m_IceSatFraction)
				{
					TotalSat = m_ScalarArray[i + (NodalDataSetsToRead-1)*(m_NumberOfNodes+m_NumberOfElements)]
					  	     + m_ScalarArray[i + (NodalDataSetsToRead-2)*(m_NumberOfNodes+m_NumberOfElements)];
					m_ScalarArray[i + NodalDataSetsToRead*(m_NumberOfNodes+m_NumberOfElements)] = TotalSat;
					if (TotalSat == 0)
					{
						m_ScalarArray[i + (NodalDataSetsToRead+1)*(m_NumberOfNodes+m_NumberOfElements)] = 0;
					}
					else
					{
						m_ScalarArray[i + (NodalDataSetsToRead+1)*(m_NumberOfNodes+m_NumberOfElements)] = 
							m_ScalarArray[i + (NodalDataSetsToRead-1)*(m_NumberOfNodes+m_NumberOfElements)]/TotalSat;
					}
				}

				m_In1->getline(line, 1000);
			}
		}

		// Calculate average values in each elements for nodal data.
		if (m_Incidence != 0)
		{
			int NodeCount;
			int Node;
			if (m_ScalarGridDim[2] == 1)   // 2D case
			{ 
				NodeCount = 4;
			}
			else
			{
				NodeCount = 8;
			}

			int Offset = -m_NumberOfElements;
			
			for (int k = 0; k<m_NumberOfNodalScalarDataTypes; k++)
			{
				Offset += (m_NumberOfNodes + m_NumberOfElements);
	
				for (int i=0; i<m_NumberOfElements; i++)
				{
					m_ScalarArray[i+Offset] = 0;
				}

				int NodeI = -1;
				for (int j=0; j<m_NumberOfElements; j++)
				{
					for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
					{
						NodeI += 1;
						Node = m_Incidence[NodeI];
						if ((Node < 0) || (Node >= m_NumberOfNodes))
						{
							return;
						}
						m_ScalarArray[j+Offset] += m_ScalarArray[Node+Offset-m_NumberOfNodes];
	
					}
					m_ScalarArray[j+Offset] /= NodeCount;	
				}
			}
		}
	
	
	}
	else  		// binary data
	{
		if (m_version >= 2)
		{
		m_In1->read((unsigned char *) m_ScalarArray, 
			m_NumberOfNodalScalarDataTypes
			*(m_NumberOfNodes + m_NumberOfElements)*sizeof(float));
		}
		else
		{
			Scalars = m_ScalarArray;
			for (i = 0; i < m_NumberOfNodalScalarDataTypes; i++)
			{
				m_In1->read((unsigned char *) Scalars, 
					m_NumberOfNodes*sizeof(float));
			    Scalars += m_NumberOfNodes;
				for (j = 0; j < m_NumberOfElements; j++)
				{
					Scalars[j] = 0;
				}
				Scalars += m_NumberOfElements;
			}
		}
	}


	if (m_VectorArray != 0)
	{
		if (m_FileType == -1)		// ascii data
		{
			if ((timePointIndex != 0) || (m_NumberOfTimePoints == m_NumberOfElementTimePoints))
			{
				for (j=0; j< 5; j++)
				{
					m_In2->getline(line, 1000);
				}
			}
			if (m_ScalarGridDim[2] == 1)	// 2D case
			{
				for (i=0; i<m_NumberOfElements; i++)
				{
					if ((timePointIndex == 0) && (m_NumberOfTimePoints != m_NumberOfElementTimePoints))
					{
						m_VectorArray[3*i] = 0;
						m_VectorArray[3*i+1] = 0;
						m_VectorArray[3*i+2] = 0;
					}
					else
					{
						(*m_In2) >> x >> y >> m_VectorArray[3*i] 
								>> m_VectorArray[3*i+1];
						m_VectorArray[3*i+2] = 0;
						m_In2->getline(line, 1000);
					}
				}
			}
			else
			{
				for (i=0; i<m_NumberOfElements; i++)
				{
					if ((timePointIndex == 0) && (m_NumberOfTimePoints != m_NumberOfElementTimePoints))
					{
						m_VectorArray[3*i] = 0;
						m_VectorArray[3*i+1] = 0;
						m_VectorArray[3*i+2] = 0;
					}
					else
					{
						(*m_In2) >> x >> y >> z >> m_VectorArray[3*i] 
								>> m_VectorArray[3*i+1] >> m_VectorArray[3*i+2];
						m_In2->getline(line, 1000);
					}
				}
			}
		}
		else		// binary data
		{
			m_In1->read((unsigned char *) m_VectorArray, 3*m_NumberOfElements*sizeof(float));
		}

		// Assign velocity values to data sets.


		int NodeCount;
		int Node;
		int AdditionalDataSets;
		if (m_ScalarGridDim[2] == 1)   // 2D case
		{ 
			NodeCount = 4;
			AdditionalDataSets = 8;
		}
		else
		{
			NodeCount = 8;
			AdditionalDataSets = 13;
		}
		int *ElementCount= new int[m_NumberOfNodes];
		for (int i=0; i<m_NumberOfNodes; i++)
		{
			ElementCount[i] = 0;
		}


		int Offset = (m_NumberOfNodes + m_NumberOfElements) 
			* (m_NumberOfNodalScalarDataTypes+AdditionalDataSets) + m_NumberOfNodes;

		for (int DataSetIndex = 0; DataSetIndex < 3; DataSetIndex++)
		{
			for (int k=0; k<m_NumberOfNodes; k++)
			{
				m_ScalarArray[k+Offset-m_NumberOfNodes] = 0;
			}
			int NodeI = -1;
			for (i=0; i<m_NumberOfElements; i++)
			{
				m_ScalarArray[i+Offset] = m_VectorArray[3*i+DataSetIndex];
				for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
				{
					NodeI += 1;
					Node = m_Incidence[NodeI];
					if ((Node < 0) || (Node >= m_NumberOfNodes))
					{
						delete [] ElementCount;
						return;
					}
					m_ScalarArray[Node+Offset-m_NumberOfNodes] += m_VectorArray[3*i+DataSetIndex];
					if (DataSetIndex == 0)
					{
						ElementCount[Node] += 1;
					}
				}
			}

			for (k=0; k<m_NumberOfNodes; k++)
			{
				if (ElementCount[k] <= 0)
				{
					delete [] ElementCount;
					return;
				}
				m_ScalarArray[k+Offset-m_NumberOfNodes] = 
					m_ScalarArray[k+Offset-m_NumberOfNodes] / ElementCount[k];
			}
			
			Offset += m_NumberOfNodes + m_NumberOfElements;
		}

		int VOffset = Offset-m_NumberOfNodes;
		int ZOffset = VOffset - (m_NumberOfNodes + m_NumberOfElements);
		int YOffset = ZOffset - (m_NumberOfNodes + m_NumberOfElements);
		int XOffset = YOffset - (m_NumberOfNodes + m_NumberOfElements);
		for (int k=0; k<(m_NumberOfNodes+m_NumberOfElements); k++)
		{
			m_ScalarArray[k+VOffset] = 
				sqrt(m_ScalarArray[k+XOffset]*m_ScalarArray[k+XOffset]
				+ m_ScalarArray[k+YOffset]*m_ScalarArray[k+YOffset]
				+ m_ScalarArray[k+ZOffset]*m_ScalarArray[k+ZOffset]);
		}

		delete [] ElementCount;
	}
}

void SutraDataSource::SetScalarDataTypeTo(int dataTypeIndex)
{
	m_ScalarArrayOffset = dataTypeIndex * (m_NumberOfNodes+m_NumberOfElements);
}

char *SutraDataSource::ReadDimensions(ifstream &in, int &numNodes, int &numElements, int *sdim)
{
	char line[1000];
	char *p1, *p2;
	try
	{
		// skip header
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		// read the mesh dimensions
		in.getline(line, 1000);
		if ((strncmp(line+3, "3-D, REGULAR MESH", 17) == 0) ||
			(strncmp(line+3, "3-D, BLOCKWISE MESH", 19) == 0))
		{
			p1 = line+28;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 37;
			*p2 = '\0';
			sdim[0] = atoi(p1);
			*p2 = ')';
			p1 = line + 40;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 49;
			*p2 = '\0';
			sdim[1] = atoi(p1);
			*p2 = ')';
			p1 = line + 52;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 61;
			*p2 = '\0';
			sdim[2] = atoi(p1);
			numNodes = sdim[0]*sdim[1]*sdim[2];
			numElements = (sdim[0]-1)*(sdim[1]-1)*(sdim[2]-1);
		}
		else if ((strncmp(line+3, "2-D, REGULAR MESH", 17) == 0) || 
			     (strncmp(line+3, "2-D, BLOCKWISE MESH", 19) == 0))
		{
			p1 = line+40;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 49;
			*p2 = '\0';
			sdim[0] = atoi(p1);
			*p2 = ')';
			p1 = line + 52;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 61;
			*p2 = '\0';
			sdim[1] = atoi(p1);
			sdim[2] = 1;
			numNodes = sdim[0]*sdim[1];
			numElements = (sdim[0]-1)*(sdim[1]-1);
		}
		else if (strncmp(line+3, "2-D, IRREGULAR MESH", 19) == 0)
		{
			p1 = line+40;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = p1;
			while (*p2 != ' ')
			{
				p2++;
			}
		    *p2 = '\0';
			numNodes = atoi(p1);
			numElements = 0;
			sdim[0] = numNodes;
			sdim[1] = 0;
			sdim[2] = 1;
		}  
		else if (strncmp(line+3, "3-D, IRREGULAR MESH", 19) == 0)
		{
			p1 = line+40;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = p1;
			while (*p2 != ' ')
			{
				p2++;
			}
		    *p2 = '\0';
			numNodes = atoi(p1);
			numElements = 0;
			sdim[0] = numNodes;
			sdim[1] = 0;
			sdim[2] = 0;
		}  
		else if (strncmp(line+3, "3-D, LAYERED MESH", 17) == 0)
		{
			p1 = line+61;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = p1;
			while (*p2 != ' ')
			{
				p2++;
			}
		    *p2 = '\0';
			numNodes = atoi(p1);
			p1 = line+79;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = p1;
			while (*p2 != ' ')
			{
				p2++;
			}
		    *p2 = '\0';
			numElements = atoi(p1);
			sdim[0] = numNodes;
			sdim[1] = 0;
			sdim[2] = 0;
		}
		else
		{
			return "Error: nod file does not contain data for a 2D mesh or a 3-D regular mesh.";
		}

	}
	catch(...)
	{
		return "Error encountered while reading the nod file";
	}
	return 0;
}

char *SutraDataSource::ReadNodFile(ifstream &in, int *sdim, int &numNodes, int &numElements,
			int &numTimePoints, bool &IceSatFraction, char **&timePointLabels, int &numScalarDataTypes,
			char **&dataTypeLabels, float *&sgCoord, int &TimeUnits)
/*
char *SutraDataSource::ReadNodFile(ifstream &in, int *sdim, int &numNodes,
		int &numElements, int &numTimePoints, bool &IceSatFraction, char **&timePointLabels,
		int &numScalarDataTypes, char **&dataTypeLabels, float *&sgCoord, int &TimeUnits)
*/
{
	char line[1000];
	char *p1, *p2;
	int i;
	char* errMessage;
	errMessage = ReadDimensions(in, numNodes, numElements, sdim);
	if (errMessage)
	{
		return errMessage;
	}

	// skip 2 lines
	in.getline(line, 1000);
	in.getline(line, 1000);

	// read the number of data sets
	in.getline(line, 1000);
	if (strncmp(line+3, "NODEWISE RESULTS", 16)) 
	{
		return "Error encountered while reading the nod file";
	}
	p1 = line + 20;
	while (*p1 == ' ')
	{
		p1++;
	}
	p2 = strchr(p1, ' ');
	*p2 = '\0';
	numTimePoints = atoi(p1);

	if (numTimePoints < 1)
	{
		return "Error: No output data saved in nod file.";
	}

	// allocate space for time points labels
	timePointLabels = new char *[numTimePoints];
	for (i=0; i<numTimePoints; i++)
	{
		timePointLabels[i] = new char[TIME_LABEL_SIZE];
	}
	sgCoord = new float[3*numNodes];
	numScalarDataTypes = 0;

	// Read time point labels and find out how many scalar data types
	try
	{
		// skip 5 lines
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		char timeStep[5];
		for (i=0; i<numTimePoints; i++)
		{
			in.getline(line, 1000);
			p1 = line + 4;
			while (*p1 == ' ') p1++;
			p2 = strchr(p1, ' ');
			*p2 = '\0';
			strcpy(timeStep, p1);
			p1 = p2 + 1;
			while (*p1 == ' ') p1++;
			p2 = strchr(p1, ' ');
			*p2 = '\0';
			float time = atof(p1);
			switch (TimeUnits)
			{
			case 0:
				break;
			case 1:
				time = time/60;
				break;
			case 2:
				time = time/3600;
				break;
			case 3:
				time = time/3600/24;
				break;
			case 4:
				time = time/3600/24/365.25*12;
				break;
			case 5:
				time = time/3600/24/365.25;
				break;
			}
			char timelabel[TIME_LABEL_SIZE];
			sprintf(timelabel, "%e", time);

			strcpy(timePointLabels[i], timelabel);
			strcat(timePointLabels[i], " (");
			strcat(timePointLabels[i], timeStep);
			strcat(timePointLabels[i], ") ");

			// for first time step, find out how many scalar data types
			if (i==0)
			{
				if ((line[40] == 'Y') || (line[40] == 'N'))
				{
					// SUTRA 2.1 file
					if (line[40] == 'Y')
					{
						numScalarDataTypes++;
					}
					if (line[55] == 'Y')
					{
						numScalarDataTypes++;
					}
					if (line[70] == 'Y')
					{
						numScalarDataTypes++;
					}
					if (line[85] == 'Y')
					{
						numScalarDataTypes++;
					}
				} 
				else
				{
					// SUTRA 2D/3D file
					if (line[35] == 'Y')
					{
						numScalarDataTypes++;
					}
					if (line[50] == 'Y')
					{
						numScalarDataTypes++;
					}
					if (line[65] == 'Y')
					{
						numScalarDataTypes++;
					}
				}
				if (numScalarDataTypes == 0) 
				{
					return "Error: No output variables saved in nod file.";
				}

			}
		}
	}
	catch(...)
	{
		return "Error encountered while reading the nod file";
	}

	// RBW Because Total Saturation, and Ice Fraction are calculated rather than read.
	numScalarDataTypes = numScalarDataTypes +2;

	int NumElementDataTypes;
	int NumNodeDataTypes = 1;

	// Read data type labels and x-y-z coordinates
	try
	{
		// skip lines
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);

		if (sdim[2] == 1)   // 2D case
		{
			if (line[15] != 'X' || line[30] != 'Y')
			{
				return "Error: Unable to read x-y coordinates in nod file.";
			}

			NumElementDataTypes = ADDITIONAL_2D_ELEMENT_DATA_SETS;

			// Allocate space for data type labels
			int MaxNumScalarDataTypes = numScalarDataTypes+NumElementDataTypes+NumNodeDataTypes;
			dataTypeLabels = new char *[MaxNumScalarDataTypes];
			for (i=0; i<numScalarDataTypes-2; i++)
			{
				dataTypeLabels[i] = new char[20];
			}
	
			// create data type labels for data read from input file.
			for (i=0; i<numScalarDataTypes-2; i++)
			{
				strncpy(dataTypeLabels[i], line + 31 + (15*i), 15);
				dataTypeLabels[i][15] = '\0';
				mvUtil::TrimLeft(dataTypeLabels[i]);
			}
			if ((numScalarDataTypes < 4)
				 ||(strcmp(dataTypeLabels[numScalarDataTypes-4], "Saturation"))
				 ||(strcmp(dataTypeLabels[numScalarDataTypes-3], "Ice Saturation")))
			{
				numScalarDataTypes = numScalarDataTypes -2;
				IceSatFraction = FALSE;
			}
			else
			{
				strncpy(dataTypeLabels[numScalarDataTypes -4], "Water Saturation", 16);
				dataTypeLabels[numScalarDataTypes -4][16] = '\0';

				dataTypeLabels[numScalarDataTypes -2] = new char[20];
				strncpy(dataTypeLabels[numScalarDataTypes -2], "Total Saturation", 16);
				dataTypeLabels[numScalarDataTypes -2][16] = '\0';
					
				dataTypeLabels[numScalarDataTypes -1] = new char[20];
				strncpy(dataTypeLabels[numScalarDataTypes -1], "Ice Fraction", 12);
				dataTypeLabels[numScalarDataTypes -1][12] = '\0';
				IceSatFraction = TRUE;
			}
	
			dataTypeLabels[numScalarDataTypes] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes], "Max. Permeability", 17);
			dataTypeLabels[numScalarDataTypes][17] = '\0';
	
			dataTypeLabels[numScalarDataTypes+1] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+1], "Min. Permeability", 17);
			dataTypeLabels[numScalarDataTypes+1][17] = '\0';

			dataTypeLabels[numScalarDataTypes+2] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+2], "Long. Disp. Max", 15);
			dataTypeLabels[numScalarDataTypes+2][15] = '\0';

			dataTypeLabels[numScalarDataTypes+3] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+3], "Long. Disp. Min", 15);
			dataTypeLabels[numScalarDataTypes+3][15] = '\0';

			dataTypeLabels[numScalarDataTypes+4] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+4], "Transv. Disp. Max", 17);
			dataTypeLabels[numScalarDataTypes+4][17] = '\0';

			dataTypeLabels[numScalarDataTypes+5] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+5], "Transv. Disp. Min", 17);
			dataTypeLabels[numScalarDataTypes+5][17] = '\0';

			dataTypeLabels[numScalarDataTypes+6] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+6], "Angle1", 6);
			dataTypeLabels[numScalarDataTypes+6][6] = '\0';

			dataTypeLabels[numScalarDataTypes+7] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+7], "Porosity", 8);
			dataTypeLabels[numScalarDataTypes+7][8] = '\0';

			dataTypeLabels[numScalarDataTypes+8] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+8], "X Velocity", 10);
			dataTypeLabels[numScalarDataTypes+8][10] = '\0';

			dataTypeLabels[numScalarDataTypes+9] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+9], "Y Velocity", 10);
			dataTypeLabels[numScalarDataTypes+9][10] = '\0';
			
			dataTypeLabels[numScalarDataTypes+10] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+10], "Z Velocity", 10);
			dataTypeLabels[numScalarDataTypes+10][10] = '\0';
			
			dataTypeLabels[numScalarDataTypes+11] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+11], "Velocity", 8);
			dataTypeLabels[numScalarDataTypes+11][8] = '\0';

			for (i=0; i<numNodes; i++)
			{
				in >> sgCoord[3*i] >> sgCoord[3*i+1];
				sgCoord[3*i+2] = 0;
				in.getline(line, 1000);
			}
		}
		else
		{
			if (line[15] != 'X' || line[30] != 'Y' || line [45] != 'Z')
			{
				return "Error: Unable to read x-y-z coordinates in nod file.";
			}

			NumElementDataTypes = ADDITIONAL_ELEMENT_DATA_SETS;

			// Allocate space for data type labels
			int MaxNumScalarDataTypes = numScalarDataTypes+NumElementDataTypes+NumNodeDataTypes;
			dataTypeLabels = new char *[MaxNumScalarDataTypes];
			for (i=0; i<numScalarDataTypes-2; i++)
			{
				dataTypeLabels[i] = new char[20];
			}
	
			// create data type labels for data read from input file.
			for (i=0; i<numScalarDataTypes-2; i++)
			{
				strncpy(dataTypeLabels[i], line + 46 + (15*i), 15);
				dataTypeLabels[i][15] = '\0';
				mvUtil::TrimLeft(dataTypeLabels[i]);
			}
			if ((numScalarDataTypes < 4)
				 ||(strcmp(dataTypeLabels[numScalarDataTypes-4], "Saturation"))
				 ||(strcmp(dataTypeLabels[numScalarDataTypes-3], "Ice Saturation")))
			{
				numScalarDataTypes = numScalarDataTypes -2;
				IceSatFraction = FALSE;
			}
			else
			{
				strncpy(dataTypeLabels[numScalarDataTypes -4], "Water Saturation", 16);
				dataTypeLabels[numScalarDataTypes -4][16] = '\0';

				dataTypeLabels[numScalarDataTypes -2] = new char[20];
				strncpy(dataTypeLabels[numScalarDataTypes -2], "Total Saturation", 16);
				dataTypeLabels[numScalarDataTypes -2][16] = '\0';
					
				dataTypeLabels[numScalarDataTypes -1] = new char[20];
				strncpy(dataTypeLabels[numScalarDataTypes -1], "Ice Fraction", 12);
				dataTypeLabels[numScalarDataTypes -1][12] = '\0';
				IceSatFraction = TRUE;
			}
	
			dataTypeLabels[numScalarDataTypes] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes], "Max. Permeability", 17);
			dataTypeLabels[numScalarDataTypes][17] = '\0';
	
			dataTypeLabels[numScalarDataTypes+1] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+1], "Mid. Permeability", 17);
			dataTypeLabels[numScalarDataTypes+1][17] = '\0';

			dataTypeLabels[numScalarDataTypes+2] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+2], "Min. Permeability", 17);
			dataTypeLabels[numScalarDataTypes+2][17] = '\0';

			dataTypeLabels[numScalarDataTypes+3] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+3], "Long. Disp. Max", 15);
			dataTypeLabels[numScalarDataTypes+3][15] = '\0';

			dataTypeLabels[numScalarDataTypes+4] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+4], "Long. Disp. Mid", 15);
			dataTypeLabels[numScalarDataTypes+4][15] = '\0';

			dataTypeLabels[numScalarDataTypes+5] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+5], "Long. Disp. Min", 15);
			dataTypeLabels[numScalarDataTypes+5][15] = '\0';

			dataTypeLabels[numScalarDataTypes+6] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+6], "Transv. Disp. Max", 17);
			dataTypeLabels[numScalarDataTypes+6][17] = '\0';

			dataTypeLabels[numScalarDataTypes+7] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+7], "Transv. Disp. Mid", 17);
			dataTypeLabels[numScalarDataTypes+7][17] = '\0';

			dataTypeLabels[numScalarDataTypes+8] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+8], "Transv. Disp. Min", 17);
			dataTypeLabels[numScalarDataTypes+8][17] = '\0';

			dataTypeLabels[numScalarDataTypes+9] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+9], "Angle1", 6);
			dataTypeLabels[numScalarDataTypes+9][6] = '\0';

			dataTypeLabels[numScalarDataTypes+10] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+10], "Angle2", 6);
			dataTypeLabels[numScalarDataTypes+10][6] = '\0';

			dataTypeLabels[numScalarDataTypes+11] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+11], "Angle3", 6);
			dataTypeLabels[numScalarDataTypes+11][6] = '\0';

			dataTypeLabels[numScalarDataTypes+12] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+12], "Porosity", 8);
			dataTypeLabels[numScalarDataTypes+12][8] = '\0';

			dataTypeLabels[numScalarDataTypes+13] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+13], "X Velocity", 10);
			dataTypeLabels[numScalarDataTypes+13][10] = '\0';

			dataTypeLabels[numScalarDataTypes+14] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+14], "Y Velocity", 10);
			dataTypeLabels[numScalarDataTypes+14][10] = '\0';
			
			dataTypeLabels[numScalarDataTypes+15] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+15], "Z Velocity", 10);
			dataTypeLabels[numScalarDataTypes+15][10] = '\0';
			
			dataTypeLabels[numScalarDataTypes+16] = new char[20];
			strncpy(dataTypeLabels[numScalarDataTypes+16], "Velocity", 8);
			dataTypeLabels[numScalarDataTypes+16][8] = '\0';
			
			for (i=0; i<numNodes; i++)
			{
				in >> sgCoord[3*i] >> sgCoord[3*i+1] >> sgCoord[3*i+2];
				in.getline(line, 1000);
			}
		}
	}
	catch(...)
	{
		return "Error encountered while reading the nod file";
	}

	return 0;
}

char *SutraDataSource::ReadEleFile(ifstream &in, int *sdim, int &numTimePoints, char **timePointLabels,
				  float *vgCoord, bool &inputread, int &SteadyFlow, int &SteadyTransport, 
				  int &ElementTimePoints, int &numElements, int &TimeUnits)
{
	char line[1000];
	char *p1, *p2;
	int i;
	try
	{
		// skip header
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		// read the mesh dimensions (element centers)
		// and compare with nod file
		if (sdim[2] == 1)	// 2D case
		{
			in.getline(line, 1000);
			p1 = line+61;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = p1;
			while (*p2 != ' ')
			{
				p2++;
			}
			*p2 = '\0';


			if (numElements == 0)
			{
				// irregular mesh
				numElements = atoi(p1);
			}
			else
			{
				// regular mesh
				if (atoi(p1) != numElements)
				{
					return "Error: ele file inconsistent with nod file";
				}
				*p2 = ')';
				p1 = line + 49;
				while (*p1 == ' ')
				{
					p1++;
				}
				p2 = line + 58;
				*p2 = '\0';
				if (atoi(p1) != sdim[1]-1)
				{
					return "Error: ele file inconsistent with nod file";
				}
			}
		}
		else
		{
			in.getline(line, 1000);
			p1 = line+61;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = p1;
			while (*p2 != ' ')
			{
				p2++;
			}
			*p2 = '\0';
			if (numElements == 0)
			{
				// irregular mesh
				numElements = atoi(p1);
			}
			else
			{
				if (atoi(p1) != numElements)
				{
					return "Error: ele file inconsistent with nod file";
				}
				p1 = p2 + 9;
				if (*p1 != '\0')
				{
					while (*p1 == ' ')
					{
						p1++;
					}
					p2 = p1;
					while (*p2 != ' ')
					{
						p2++;
					}
					*p2 = '\0';
					if (sdim[2] == 0)
					{
						if (atoi(p1) != sdim[0])
						{
							return "Error: ele file inconsistent with nod file";
						}
					}
					else
					{
						if (atoi(p1) != sdim[0]*sdim[1]*sdim[2])
						{
							return "Error: ele file inconsistent with nod file";
						}
					}
				}
			}
		}

		// skip 2 lines
		in.getline(line, 1000);
		in.getline(line, 1000);

		// read the number of data sets
		in.getline(line, 1000);
		if (strncmp(line+3, "VELOCITY RESULTS", 16)) 
		{
			return "Error encountered while reading the ele file";
		}
		p1 = line + 20;
		while (*p1 == ' ')
		{
			p1++;
		}
		p2 = strchr(p1, ' ');
		*p2 = '\0';
		ElementTimePoints = atoi(p1);
		if (ElementTimePoints != numTimePoints-1)
		{
			if (inputread)
			{
				if ((ElementTimePoints != 1)||(SteadyFlow != 1))
				{
					return "Error: ele and nod files contain different time steps";
				}
			}
			else
			{
				if (ElementTimePoints != 1)
				{
					return "Error: ele and nod files contain different time steps";
				}
			}

		}
		// skip 5 lines
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		// get the saved time steps
		char timeStep[5], tpLabel[20];
		for (i=0; i<ElementTimePoints; i++)
		{
			in.getline(line, 1000);
			p1 = line + 4;
			while (*p1 == ' ') p1++;
			p2 = strchr(p1, ' ');
			*p2 = '\0';
			strcpy(timeStep, p1);
			p1 = p2 + 1;
			while (*p1 == ' ') p1++;
			p2 = strchr(p1, ' ');
			*p2 = '\0';


			float time = atof(p1);
			switch (TimeUnits)
			{
			case 0:
				break;
			case 1:
				time = time/60;
				break;
			case 2:
				time = time/3600;
				break;
			case 3:
				time = time/3600/24;
				break;
			case 4:
				time = time/3600/24/365.25*12;
				break;
			case 5:
				time = time/3600/24/365.25;
				break;
			}
			char timelabel[TIME_LABEL_SIZE];
			sprintf(timelabel, "%e", time);


			strcpy(tpLabel, timelabel);
			strcat(tpLabel, " (");
			strcat(tpLabel, timeStep);
			strcat(tpLabel, ") ");

			// The first time step in the node file is time step 0.
			// The first time step in the element file is time step 1.
			// Time step 1 must also be in the node file in versions prior to version 2.2.
			if (ElementTimePoints == numTimePoints)
			{
				if ((SteadyFlow != 1)&&(strcmp(timePointLabels[i], tpLabel) != 0))
				{
					return "Error: ele and nod files contain different time steps";
				}
			}
			else
			{
				if (strcmp(timePointLabels[i+1], tpLabel) != 0)
				{
					return "Error: ele and nod files contain different time steps";
				}
			}
		}
		// skip 5 lines
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);
		in.getline(line, 1000);

		if (sdim[2] == 1)	// 2D case
		{
			if (strncmp(line, "##      X origin       Y origin", 31) != 0)
			{
				return "Error: Unable to read x-y-z coordinates in ele file.";
			}
			if (strncmp(line + 36, "X velocity     Y velocity", 25) != 0)
			{
				return "Error: Unable to read velocity components in ele file.";
			}
			for (i=0; i<numElements; i++)
			{
				in >> vgCoord[3*i] >> vgCoord[3*i+1];
				vgCoord[3*i+2] = 0;
				in.getline(line, 1000);
			}
		}
		else
		{
			if (strncmp(line, "##      X origin       Y origin       Z origin", 46) != 0)
			{
				return "Error: Unable to read x-y-z coordinates in ele file.";
			}
			if (strncmp(line + 51, "X velocity     Y velocity     Z velocity", 40) != 0)
			{
				return "Error: Unable to read velocity components in ele file.";
			}

			for (i=0; i<numElements; i++)
			{
				in >> vgCoord[3*i] >> vgCoord[3*i+1] >> vgCoord[3*i+2];
				in.getline(line, 1000);
			}
		}
	}
	catch(...)
	{
		return "Error encountered while reading the ele file";
	}
	return 0;
}

char *SutraDataSource::LoadBinaryFile()
{
	int i, hasVelocity;
	char header[20];

	m_In1->read((unsigned char *) header, 20*sizeof(char));
	if (strncmp(header, "Sutra binary", 12) != 0)
	{
		m_In1->close();
		return "The input file does not contain Sutra binary data.";
	}
	char *p2 = strchr(header+13, ' ');
	*p2 = '\0';
	m_version = (float) atof(header+13);
	if (fabs(m_version - 1.00) > 0.0001)
	{
		if (fabs(m_version - 2.00) > 0.0001)
		{
			return "Cannot read an older version of Sutra binary data.";
		}
		else
		{
			m_version = 2.0;
		}
	}
	else
	{
		m_version = 1.0;
	}
	m_In1->read((unsigned char *) m_ScalarGridDim, 3*sizeof(int));
	if (m_ScalarGridDim[1] == 0)
	{
		m_GridType = MV_UNSTRUCTED_GRID;
	}
	else
	{
		m_GridType = MV_STRUCTURED_GRID_ALL_ACTIVE;
	}

	if (m_version >= 2)
	{
		m_In1->read((unsigned char *) &m_NumberOfNodes, sizeof(int));
		m_In1->read((unsigned char *) &m_NumberOfElements, sizeof(int));
		m_In1->read((unsigned char *) &m_NumberOfNodalScalarDataTypes, sizeof(int));
		m_In1->read((unsigned char *) &m_NumberOfScalarDataTypes, sizeof(int));
	}
	else
	{
		m_In1->read((unsigned char *) &m_NumberOfScalarDataTypes, sizeof(int));
		m_NumberOfNodalScalarDataTypes = m_NumberOfScalarDataTypes;
	}
	m_DataTypeLabels = new char *[m_NumberOfScalarDataTypes];
	for (i=0; i<m_NumberOfScalarDataTypes; i++)
	{
		m_DataTypeLabels[i] = new char[20];
		m_In1->read((unsigned char *) m_DataTypeLabels[i], 20*sizeof(char));
	}
	m_In1->read((unsigned char *) &m_NumberOfTimePoints, sizeof(int));
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	for (i=0; i<m_NumberOfTimePoints; i++)
	{
		m_TimePointLabels[i] = new char[TIME_LABEL_SIZE];
		if (m_version < 2)
		{
			m_In1->read((unsigned char *) m_TimePointLabels[i], 20*sizeof(char));
		}
		else
		{
			m_In1->read((unsigned char *) m_TimePointLabels[i], TIME_LABEL_SIZE*sizeof(char));
		}
	}
	if (m_version < 2)
	{
		m_NumberOfNodes = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
		m_NumberOfElements = (m_ScalarGridDim[0]-1)*(m_ScalarGridDim[1]-1);
		if (m_ScalarGridDim[2] > 1)
		{
			m_NumberOfElements *= (m_ScalarGridDim[2]-1);
		}
	}
	m_ScalarGridCoordinates = new float[3*m_NumberOfNodes];
	m_In1->read((unsigned char *) m_ScalarGridCoordinates, m_NumberOfNodes*3*sizeof(float));
	m_In1->read((unsigned char *) &hasVelocity, sizeof(int));
	if (hasVelocity)
	{
		m_VectorGridCoordinates = new float[3*m_NumberOfElements];
		m_In1->read((unsigned char *) m_VectorGridCoordinates, m_NumberOfElements*3*sizeof(float));
	}

	if (m_version >= 2)
	{
		if (m_ScalarGridDim[2] == 1)
		{
			//2D
			m_Incidence = new int[4*m_NumberOfElements];
			m_In1->read((unsigned char *) m_Incidence, 4*m_NumberOfElements*sizeof(int));
		}
		else
		{
			//3D
			m_Incidence = new int[8*m_NumberOfElements];
			m_In1->read((unsigned char *) m_Incidence, 8*m_NumberOfElements*sizeof(int));

		}
	}
	else
	{
		m_Incidence = 0;
	}


	int ibousz = 0;
	m_In1->read((unsigned char *) &m_NumberOfModelFeatureTypes, sizeof(int));
	m_ModelFeatureLabels = new char[40*m_NumberOfModelFeatureTypes];
	if (m_NumberOfModelFeatureTypes > 0)
	{
		m_In1->read((unsigned char *) m_ModelFeatureLabels, 
					40*m_NumberOfModelFeatureTypes*sizeof(char));
		m_In1->read((unsigned char *) &ibousz, sizeof(int));
		if (ibousz > 0)
		{
			m_ModelFeatureArray = new int[ibousz];
			m_In1->read((unsigned char *) m_ModelFeatureArray, ibousz*sizeof(int));
		}
	}

	// we use the labels in the fortran code instead of those saved
	// in the binary file. This ensures we use the most updated labels.
	int len = 40*m_NumberOfModelFeatureTypes;
//	sutra_labels_(m_ModelFeatureLabels, len);
//	sutra_labels2_(m_ModelFeatureLabels, len);
//	sutra_labels21_(m_ModelFeatureLabels, len);
	SUTRA_LABELS22_(m_ModelFeatureLabels, len);

	int count = 0;

	if (m_version >= 2)
	{
		m_In1->read((unsigned char *) &count, sizeof(int));
	}

	m_ScalarArray = new float[(m_NumberOfNodes + m_NumberOfElements) 
		* m_NumberOfScalarDataTypes];

	// Set ScalarArray to point to the beginning of the non-transient spatial data.

	float *ScalarArray = m_ScalarArray + ((m_NumberOfNodes + m_NumberOfElements) 
		* (m_NumberOfScalarDataTypes - count));

	if (count > 0)
	{
		m_In1->read((unsigned char *) ScalarArray, (m_NumberOfNodes + m_NumberOfElements) 
			* count *sizeof(float));
	}

	if (m_version >= 2)
	{
		// read mesh info
		m_In1->read((unsigned char *) &m_MeshInfo[0], sizeof(int));
		m_In1->read((unsigned char *) &m_MeshInfo[1], sizeof(int));
		m_In1->read((unsigned char *) &m_MeshInfo[2], sizeof(int));
	}

	if (hasVelocity)
	{
		m_VectorGridDim[0] = m_ScalarGridDim[0]-1;
		m_VectorGridDim[1] = m_ScalarGridDim[1]-1;
		if (m_ScalarGridDim[2] == 1)	// 2D case
		{
			m_VectorGridDim[2] = 1;
		}
		else
		{
			m_VectorGridDim[2] = m_ScalarGridDim[2]-1;
		}
		m_VectorArray = new float[3*m_NumberOfElements];
	}

	return 0;
}

char *SutraDataSource::AtoBConvert1(ifstream &nod, ifstream &ele, const char *inpFile, ofstream &bin, 
			int &numNodes, int &numElements, int &numScalarDataTypes, int &numTimePoints, bool &IceSatFraction, int &nz,
			int &numNodalScalarDataTypes, int &numElementTimePoints, int *incidence, int &TimeUnits)
{

	int i;
	float *sgCoord = 0;
	float *vgCoord = 0;
	char **timePointLabels = 0;
	char **dataTypeLabels = 0;
	int ISTEADYFLOW, ISTEADYTRANSPORT;
	int sdim[3];
	int hasVelocity = 0;
	int ElementScalarArraySize = 0;
	int NodeScalarArraySize = 0;
	float *ElementScalarArray = 0;
	float *NodeScalarArray = 0;
	int MeshInfo[3];
	bool IceSat;
	IceSat = FALSE;
	MeshInfo[0] = -1;
	MeshInfo[1] = -1;
	MeshInfo[2] = -1;

	// Read and datasets 1-4 of input file.
		bool inputread = false;
	int len = strlen(inpFile);
	if (len > 0)
	{
		inputread = true;
		int ierror, istart, nfeatures, ibousz;
		istart = 0;
//		sutra_2d3d_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT, (char *) inpFile, len);
//		sutra_2d3d2_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT,   
//			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
//				&NodeScalarArraySize, (char *) inpFile, len);
//		sutra_21_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT,   
//			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
//				&NodeScalarArraySize, &MeshInfo[0], (char *) inpFile, len);
		SUTRA_22_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
			&ISTEADYTRANSPORT,   
			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
			&NodeScalarArraySize, &MeshInfo[0], (char *) inpFile, len);
		if (ierror)
		{
			if (ierror == -1)
			{
				return "Unable to open inp file";
			}
			else
			{
				return "Error encountered while reading inp file";
			}
		}
	}
	
	
	// Read the nod file
	// Note that ReadNodFile will allocate memory for timePointLabels, dataTypeLabels,
	// and sgCoord
	char *errmsg = ReadNodFile(nod, sdim, numNodes, numElements, numTimePoints, IceSatFraction,
			timePointLabels, numScalarDataTypes, dataTypeLabels, sgCoord, TimeUnits);
	if (errmsg)
	{
		if (timePointLabels)
		{
			for (i=0; i<numTimePoints; i++)
			{
				delete [] timePointLabels[i];
			}
			delete [] timePointLabels;
		}
		if (dataTypeLabels)
		{
			for (i=0; i<numScalarDataTypes; i++)
			{
				delete [] dataTypeLabels[i];
			}
			delete [] dataTypeLabels;
		}
		if (sgCoord) delete [] sgCoord;
		return errmsg;
	}

	numNodalScalarDataTypes = numScalarDataTypes;
	nz = sdim[2];
	if (len != 0)
	{
		if (nz == 1)
		{
			// 2D
			numScalarDataTypes = numScalarDataTypes+ADDITIONAL_2D_ELEMENT_DATA_SETS+ADDITIONAL_NODE_DATA_SETS;
		}
		else
		{
			// 3D
			numScalarDataTypes = numScalarDataTypes+ADDITIONAL_ELEMENT_DATA_SETS+ADDITIONAL_NODE_DATA_SETS;
		}
	}
	// Read the ele file if used
	if (ele.is_open())
	{
		if (numElements == 0)
		{
			vgCoord = new float[3*numNodes];
		}
		else
		{
			vgCoord = new float[3*numElements];
		}
		errmsg = ReadEleFile(ele, sdim, numTimePoints, timePointLabels, vgCoord, 
			inputread, ISTEADYFLOW, ISTEADYTRANSPORT, numElementTimePoints, numElements, TimeUnits);
		if (errmsg)
		{
			for (i=0; i<numTimePoints; i++)
			{
				delete [] timePointLabels[i];
			}
			delete [] timePointLabels;
			for (i=0; i<numScalarDataTypes; i++)
			{
				delete [] dataTypeLabels[i];
			}
			delete [] dataTypeLabels;
			delete [] sgCoord;
			delete [] vgCoord;
			return errmsg;
		}
		hasVelocity = 1;
	}

	// Write mesh data
	char header[20];
	strcpy(header, "Sutra binary 2.00  ");
	bin.write((unsigned char *) header, 20*sizeof(char));
	bin.write((unsigned char *) &sdim, 3*sizeof(int));

	bin.write((unsigned char *) &numNodes, sizeof(int));
	bin.write((unsigned char *) &numElements, sizeof(int));

	bin.write((unsigned char *) &numNodalScalarDataTypes, sizeof(int));
	bin.write((unsigned char *) &numScalarDataTypes, sizeof(int));
	for (i=0; i<numScalarDataTypes; i++)
	{
		bin.write((unsigned char *) dataTypeLabels[i], 20*sizeof(char));
	}
	bin.write((unsigned char *) &numTimePoints, sizeof(int));
	for (i=0; i<numTimePoints; i++)
	{
		bin.write((unsigned char *) timePointLabels[i], TIME_LABEL_SIZE*sizeof(char));
	}
	bin.write((unsigned char *) sgCoord, 3*numNodes*sizeof(float));
	delete [] dataTypeLabels;
	dataTypeLabels = 0;
	delete [] timePointLabels;
	timePointLabels = 0;
	delete [] sgCoord;
	sgCoord = 0;
	bin.write((unsigned char *) &hasVelocity, sizeof(int));
	if (hasVelocity)
	{
		bin.write((unsigned char *) vgCoord, 3*numElements*sizeof(float));
		delete [] vgCoord;
		vgCoord = 0;
	}

	// Read and write incidence, model feature data, and other data from input file
	len = strlen(inpFile);
	if (len > 0)
	{
		int ierror, istart, nfeatures, ibousz;
		istart = 0;

		ElementScalarArraySize = numElements*ADDITIONAL_ELEMENT_DATA_SETS;
		ElementScalarArray = new float[ElementScalarArraySize];
		NodeScalarArraySize = numNodes*ADDITIONAL_NODE_DATA_SETS;
		NodeScalarArray = new float[NodeScalarArraySize];
	
//		sutra_2d3d_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT, (char *) inpFile, len);
//		sutra_2d3d2_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT,  
//			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
//				&NodeScalarArraySize, (char *) inpFile, len);
//		sutra_21_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT,  
//			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
//				&NodeScalarArraySize, &MeshInfo[0], (char *) inpFile, len);
		SUTRA_22_(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
			&ISTEADYTRANSPORT,  
			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
			&NodeScalarArraySize, &MeshInfo[0], (char *) inpFile, len);
		if (ierror)
		{
			delete [] ElementScalarArray;
			delete [] NodeScalarArray;
			if (ierror == -1)
			{
				return "Unable to open inp file";
			}
			else
			{
				return "Error encountered while reading inp file";
			}
		}
		int *ibnode = new int[ibousz];
		istart = 1;
//		sutra_2d3d_(&ierror, &istart, &ibousz, ibnode, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT, (char *) inpFile, len);
//		sutra_2d3d2_(&ierror, &istart, &ibousz, ibnode, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT,  
//			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
//				&NodeScalarArraySize, (char *) inpFile, len);
//		sutra_21_(&ierror, &istart, &ibousz, ibnode, &nfeatures, &ISTEADYFLOW, 
//			&ISTEADYTRANSPORT,  
//			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
//				&NodeScalarArraySize, &MeshInfo[0], (char *) inpFile, len);
		SUTRA_22_(&ierror, &istart, &ibousz, ibnode, &nfeatures, &ISTEADYFLOW, 
			&ISTEADYTRANSPORT,  
			ElementScalarArray, &ElementScalarArraySize, incidence, NodeScalarArray,
			&NodeScalarArraySize, &MeshInfo[0], (char *) inpFile, len);
		if (ierror)
		{
			delete [] ElementScalarArray;
			delete [] NodeScalarArray;
			delete [] ibnode;
			return "Error encountered while reading inp file";
		}

		if (nz == 1)
		{
			// 2D
			// write incidence
			bin.write((unsigned char *) incidence, numElements*4*sizeof(int));
			
		}
		else
		{
			// 3D
			// write incidence
			bin.write((unsigned char *) incidence, numElements*8*sizeof(int));
			
		}

		// Write model features.

		len = 40*nfeatures;
		char *flabels = new char[len];
//		sutra_labels_(flabels, len);
//		sutra_labels2_(flabels, len);
//		sutra_labels21_(flabels, len);
		SUTRA_LABELS22_(flabels, len);
		bin.write((unsigned char *) &nfeatures, sizeof(int));
		bin.write((unsigned char *) flabels, 40*nfeatures*sizeof(char));
		bin.write((unsigned char *) &ibousz, sizeof(int));
		bin.write((unsigned char *) ibnode, ibousz*sizeof(int));
		delete [] ibnode;
		ibnode = 0;
		delete [] flabels;
		flabels = 0;

		// Write additional data from input file to the binary file.

		float *ScalarArray = new float[(numNodes + numElements) 
			* (ADDITIONAL_ELEMENT_DATA_SETS+ADDITIONAL_NODE_DATA_SETS)]; 
		int *ElementCount= new int[numNodes];
		
		int NumberOfElementDataSets;
		int NumberOfNodeDataSets = 1;
		int NodeCount;
		int Node;
		if (sdim[2] == 1)   // 2D case
		{ 
			NodeCount = 4;
			NumberOfElementDataSets = ADDITIONAL_2D_ELEMENT_DATA_SETS;
		}
		else
		{
			NodeCount = 8;
			NumberOfElementDataSets = ADDITIONAL_ELEMENT_DATA_SETS;
		}

		for (int i=0; i<numNodes; i++)
		{
			ElementCount[i] = 0;
		}

		int Offset = 0;

		for (int DataSetIndex = 0; DataSetIndex < NumberOfElementDataSets; DataSetIndex++)
		{
			for (int i=0; i<numNodes; i++)
			{
				ScalarArray[i+Offset] = 0;
			}
			Offset += numNodes + numElements;
		}

		// initialize node data for data that is originally stored by node (SUTRA Data set 14).
		Offset += numNodes;
		for (DataSetIndex = 0; DataSetIndex < NumberOfNodeDataSets; DataSetIndex++)
		{
			for (i=0; i<numElements; i++)
			{
				ScalarArray[i+Offset] = 0;
			}
			Offset += numNodes + numElements;
		}

		Offset = numNodes;

		// store data in ScalarArray and ElementCount.

		for (DataSetIndex = 0; DataSetIndex < NumberOfElementDataSets; DataSetIndex++)
		{
			int NodeI = -1;
			for (int j=0; j<numElements; j++)
			{
				ScalarArray[j+Offset] = ElementScalarArray[j + DataSetIndex*numElements];
				for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
				{
					NodeI += 1;
					Node = incidence[NodeI];
					if ((Node < 0) || (Node >= numNodes))
					{
						delete [] ElementCount;
						delete [] ElementScalarArray;
						delete [] NodeScalarArray;
						delete [] ScalarArray;
						return "Error encountered while reading the inp file";
					}
					ScalarArray[Node+Offset-numNodes] += ElementScalarArray[j + DataSetIndex*numElements];
					if (DataSetIndex == 0)
					{
						ElementCount[Node] += 1;
					}
				}
			}
			Offset += numNodes + numElements;
		}

		// compute nodal values as the arithmetic average of the values of all the 
		// elements or which each node is a part.
			
		Offset = 0;

		for (DataSetIndex = 0; DataSetIndex < NumberOfElementDataSets; DataSetIndex++)
		{
			for (int k=0; k<numNodes; k++)
			{
				if (ElementCount[k] <= 0)
				{
					delete [] ElementCount;
					delete [] ElementScalarArray;
					delete [] NodeScalarArray;
					delete [] ScalarArray;
					return "Error encountered while reading the inp file";
				}
				ScalarArray[k+Offset] = 
					ScalarArray[k+Offset] / ElementCount[k];
			}
			Offset += numNodes + numElements;
		}

		
		for (DataSetIndex = 0; DataSetIndex < NumberOfNodeDataSets; DataSetIndex++)
		{
			for (int j=0; j<numNodes; j++)
			{
				ScalarArray[j+Offset] = NodeScalarArray[j + DataSetIndex*numNodes];
			}
			Offset += numNodes + numElements;
		}

		Offset = (numNodes + numElements) 
			* NumberOfElementDataSets
			+ numNodes;
		for (DataSetIndex = 0; DataSetIndex < NumberOfNodeDataSets; DataSetIndex++)
		{
			int NodeI = -1;
			for (int j=0; j<numElements; j++)
			{
				for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
				{
					NodeI +=1;
					Node = incidence[NodeI];
					ScalarArray[j+Offset] += 
						NodeScalarArray[Node + DataSetIndex*numElements];
				}
				ScalarArray[j+Offset] /= NodeCount;
			}
			Offset += numNodes + numElements;
		}
		
		
		
		// Write the data to the binary file.
		// data stored as numNodes nodal data 
		// followed by numElements element data.

		int count = NumberOfElementDataSets + NumberOfNodeDataSets;

		bin.write((unsigned char *) &count, sizeof(int));

		bin.write((unsigned char *) ScalarArray, (numNodes + numElements) * count *sizeof(float));

		bin.write((unsigned char *) &MeshInfo[0], sizeof(int));
		bin.write((unsigned char *) &MeshInfo[1], sizeof(int));
		bin.write((unsigned char *) &MeshInfo[2], sizeof(int));
			
		delete [] ElementScalarArray;
		delete [] NodeScalarArray;
		delete [] ScalarArray;
		delete [] ElementCount;
	}
	else
	{
		int nfeatures = 0;
		bin.write((unsigned char *) &nfeatures, sizeof(int));

		int count = 0;
		bin.write((unsigned char *) &count, sizeof(int));
	}
	return 0;
}

char *SutraDataSource::AtoBConvert2(ifstream &nod, ifstream &ele, ofstream &bin, int timePoint,
		int numNodes, int numElements, int numScalarDataTypes, int numTimePoints, int nz, 
		int numNodalScalarDataTypes, int numElementTimePoints, int *Incidence)
{
	char line[1000];
	float x, y, z;
	float *array = new float[(numNodes+numElements)*numNodalScalarDataTypes];
	int i, j;
	try
	{
		if (timePoint == 0)
		{
			// skip headers
			for (i=0; i<numTimePoints+12; i++)
			{
				nod.getline(line, 1000);
			}
			if (ele.is_open())
			{
				for (i=0; i<numElementTimePoints+12; i++)
				{
					ele.getline(line, 1000);
				}
			}
		}
		nod.getline(line, 1000);
		nod.getline(line, 1000);
		nod.getline(line, 1000);
		nod.getline(line, 1000);
		nod.getline(line, 1000);
		if (nz == 1)
		{
			for (i=0; i<numNodes; i++)
			{
				nod >> x >> y;
				for (j=0; j<numNodalScalarDataTypes; j++)
				{
					nod >> array[j*(numNodes+numElements) + i];
				}
				nod.getline(line, 1000);
			}
		}
		else
		{
			for (i=0; i<numNodes; i++)
			{
				nod >> x >> y >> z;
				for (j=0; j<numNodalScalarDataTypes; j++)
				{
					nod >> array[j*(numNodes+numElements) + i];
				}
				nod.getline(line, 1000);
			}
		}

		int NodeCount;
		int Node;
		if (nz == 1)   // 2D case
		{ 
			NodeCount = 4;
		}
		else
		{
			NodeCount = 8;
		}

		int Offset = -numElements;
			
		for (int k = 0; k<numNodalScalarDataTypes; k++)
		{
			Offset += (numNodes + numElements);

			for (int i=0; i<numElements; i++)
			{
				array[i+Offset] = 0;
			}

			if (Incidence != 0)
			{
				int NodeI = -1;
				for (int j=0; j<numElements; j++)
				{
					for (int nodeIndex=0; nodeIndex < NodeCount; nodeIndex++)
					{
						NodeI += 1;
						Node = Incidence[NodeI];
						if ((Node < 0) || (Node >= numNodes))
						{
							return "error calculating element values";
						}
						array[j+Offset] += array[Node+Offset-numNodes];
	
					}
					array[j+Offset] /= NodeCount;	
				}
			}
		}



		bin.write((unsigned char *) array, (numNodes+numElements)*numNodalScalarDataTypes*sizeof(float));
	}
	catch(...)
	{
		delete [] array;
		return "Error encountered while reading nod file";
	}
	delete [] array;

	if (ele.is_open())
	{
		array = new float [3*numElements];
		try
		{
			if ((timePoint != 0) || (numTimePoints == numElementTimePoints))
			{
				ele.getline(line, 1000);
				ele.getline(line, 1000);
				ele.getline(line, 1000);
				ele.getline(line, 1000);
				ele.getline(line, 1000);
				if (nz == 1) 
				{
					for (i=0; i<numElements; i++)
					{
						ele >> x >> y >> array[3*i] >> array[3*i+1];
						array[3*i+2] = 0;
						ele.getline(line, 1000);
					}
				}
				else
				{
					for (i=0; i<numElements; i++)
					{
						ele >> x >> y >> z >> array[3*i] >> array[3*i+1] >> array[3*i+2];
						ele.getline(line, 1000);
					}
				}
			}
			else
			{
				for (i=0; i<numElements; i++)
				{
					array[3*i] = 0;
					array[3*i+1] = 0;
					array[3*i+2] = 0;
				}
			}
			bin.write((unsigned char *) array, 3*numElements*sizeof(float));
		}
		catch(...)
		{
			delete [] array;
			return "Error encountered while reading ele file";
		}
		delete [] array;
	}
	return 0;
}

void SutraDataSource::ReleaseMemory()
{

	if (m_Incidence != 0)
	{
		delete [] m_Incidence;
		m_Incidence = 0;
	}
}

int SutraDataSource::GetPrimaryScalarMode()
{
	return MV_CELL_SCALARS;
}

int *SutraDataSource::GetScalarGridDimensions()
{
	m_ScalarGridDim2[0] = m_ScalarGridDim[0];
	m_ScalarGridDim2[1] = m_ScalarGridDim[1];
	m_ScalarGridDim2[2] = m_ScalarGridDim[2];
	if ((m_MeshInfo[1] == 1) || (m_MeshInfo[1] == 4))
	{
		m_ScalarGridDim2[2] = m_MeshInfo[2];
	}

	return m_ScalarGridDim2;
}

float *SutraDataSource::GetScalarGridCoordinates()
{
	return m_ScalarGridCoordinates;
}

float *SutraDataSource::GetScalarArray()
{
		return m_ScalarArray + m_ScalarArrayOffset;
}

int SutraDataSource::GetDataSetToUseForRange()
{
	return MV_USE_POINT_DATA_FOR_RANGE;
}

void SutraDataSource::AssignConnectivity(int *types, vtkCellArray* connectivity, 
  vtkPolyData *meshlines, mvExternalMeshVector *meshvector, mvExternalMeshVector *linesvector)
{
	// connectivity records which nodes are part of each element.

	int ElementType;
	int NodeCount;
	int FaceNodeCount;
	int numCells = GetNumCells();
	bool Layered = ((m_MeshInfo[1] == 1)||(m_MeshInfo[1] == 4));
	m_LayeredMesh = Layered;
	int Divisor;
	if (Layered)
	{
		if (m_MeshInfo[1] == 1)
		{
			// Layered Across
			// Divisor is the number of layers of nodes.
			Divisor = m_MeshInfo[2];
		}
		else
		{
			// Layered Within
			// Divisor is the number of nodes in a layer of nodes.
			Divisor = m_NumberOfNodes / m_MeshInfo[2];
		}
		meshvector->SetSize(m_MeshInfo[2]);
		linesvector->SetSize(m_MeshInfo[2]);
	}
	else
	{
		// Divisor should only be used for meshes.that are layered.
		Divisor = 0;
		meshvector->SetSize(0);
		linesvector->SetSize(0);
	}
	int FirstNode;
	int OtherNode;
	int FirstNodeLayer;
	int OtherNodeLayer;
	bool SameLayer;
		

	// QuadIndicies and HexIndicies indicate the positions of nodes on the face of an element.
	// QuadIndicies is for 2D elements.
	// HexIndicies is for 3D elements.
	int QuadIndicies[4][2] = {
		{0,1}, {1,2}, {2,3}, {3,0}
	};
	int HexIndicies[6][4] = {
		{0,1,2,3}, {7,6,5,4}, {4,0,3,7}, {6,2,1,5}, {4,5,1,0}, {3,2,6,7}
	};

	// Face and OtherFace each represent a face of an element.
	mvSutraFace *Face = 0;
	mvSutraFace *OtherFace = 0;
	mvSutraFace *ThreeDFace = 0;

	// SetFaces is a set of SutraFace that will be used to determine which element faces
	// are on the mesh shell.
	SetFaces *Faces = new SetFaces;
	SetFaces *ThreeDFaces = new SetFaces;
	SetFaces *ExternalMeshFaces = new SetFaces;
	SetFaces *ExternalLinesFaces = new SetFaces;
	SetFaces *EMFaces = new SetFaces;
	SetFaces *ELFaces = new SetFaces;

	SetFaces::iterator itInner;
	SetFaces::iterator itOuter;

	int FaceIndex;
	int NodeStart;
		
	if (m_ScalarGridDim[2] == 1)   // 2D case
	{ 
		ElementType = VTK_QUAD;
		NodeCount = 4;
		FaceNodeCount = 2;
	}
	else
	{
		ElementType = VTK_HEXAHEDRON;
		NodeCount = 8;
		FaceNodeCount = 4;
	}

	int NodeIndex = 0;
	int ConnecIndex = 0;

	vtkIdTypeArray* connec = vtkIdTypeArray::New();
	connec->SetNumberOfValues((NodeCount+1)*numCells);

	for (int ElementIndex = 0; ElementIndex < numCells; ElementIndex++)
	{
		types[ElementIndex] = ElementType;

		// add the number of nodes in each element to connec
		connec->SetValue(ConnecIndex,NodeCount);
		ConnecIndex++;

		NodeStart = NodeIndex;
		for (int NodeI = 0; NodeI < NodeCount; NodeI++)
		{

			// Add the node number of each node in an element to connec
			connec->SetValue(ConnecIndex,m_Incidence[NodeIndex]);
			ConnecIndex++;
			NodeIndex++;
		}

		if (NodeCount == 4)
		{
			// 2D irregular mesh.

			// For each face of the element, create a new SutraFace.
			// If a similar face is already in Faces, delete the new face.
			// Otherwise, add the new face to Faces.
			// When all elements have been processed, Faces will contain one copy
			// of each face in the mesh.  Each face will consist of two node indicies.
			// These indicies indicate where lines need to be drawn between nodes.
			for (FaceIndex = 0; FaceIndex < 4; FaceIndex++)
			{
				Face = new mvSutraFace;
				Face->push_back(m_Incidence[NodeStart + QuadIndicies[FaceIndex][0]]);
				Face->push_back(m_Incidence[NodeStart + QuadIndicies[FaceIndex][1]]);
				Face->sort();
				itInner = Faces->find(Face);
				if (itInner == Faces->end())
				{
					Faces->insert(Face);
				}
				else
				{
					delete Face;
				}
			}

		}
		else
		{
			// 3D irregular mesh or 3D Layered (accross) mesh or 3D Layered (within) mesh.

			// For each face of the element, create a new SutraFace.

			// For Layered meshes, delete the face if the nodes in it aren't all on the same layer.
			// or if a similar face is already in ThreeDFaces.
			// When all the elements have been processed, ThreeDFaces will contain only
			// the faces in which all the nodes are on the same layer
			// 
			// For Irregular meshes,
			// if a similar face is already in ThreeDFaces, remove and delete
			// the existing face in ThreeDFaces and delete the new face.
			// Otherwise, add the new face to ThreeDFaces.
			// When all elements have been processed, ThreeDFaces will contain only
			// the faces that are on the boundary of the mesh.
			for (FaceIndex = 0; FaceIndex < 6; FaceIndex++)
			{
				Face = new mvSutraFace;
				Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][0]]);
				Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][1]]);
				Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][2]]);
				Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][3]]);
				Face->sort();
				itInner = ThreeDFaces->find(Face);
				if (Layered)
				{
					FirstNode = Face->node(0);
					FirstNodeLayer = FirstNode % Divisor;
					SameLayer = TRUE;
					for (int InnerFaceIndex = 1; InnerFaceIndex < 4; InnerFaceIndex++)
					{
						OtherNode = Face->node(InnerFaceIndex);
						OtherNodeLayer = OtherNode % Divisor;
						SameLayer = SameLayer && (FirstNodeLayer == OtherNodeLayer);
					}
					if (SameLayer && (itInner == ThreeDFaces->end()))
					{
						ThreeDFaces->insert(Face);
					}
					else
					{
						delete Face;
					}

				}
				else
				{
					if (itInner == ThreeDFaces->end())
					{
						ThreeDFaces->insert(Face);
					}
					else
					{
						OtherFace = *itInner;
						ThreeDFaces->erase(itInner);
						delete OtherFace;
						delete Face;
					}
				}
			}
			if (Layered)
			{
				for (FaceIndex = 0; FaceIndex < 6; FaceIndex++)
				{
					Face = new mvSutraFace;
					Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][0]]);
					Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][1]]);
					Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][2]]);
					Face->push_back(m_Incidence[NodeStart + HexIndicies[FaceIndex][3]]);
					FirstNode = Face->node(0);
					FirstNodeLayer = FirstNode % Divisor;
					SameLayer = TRUE;
					for (int InnerFaceIndex = 1; InnerFaceIndex < 4; InnerFaceIndex++)
					{
						OtherNode = Face->node(InnerFaceIndex);
						OtherNodeLayer = OtherNode % Divisor;
						SameLayer = SameLayer && (FirstNodeLayer == OtherNodeLayer);
					}
					if (SameLayer)
					{
						delete Face;
					}
					else
					{
						Face->sort();
						itInner = ExternalMeshFaces->find(Face);
						if (itInner == ExternalMeshFaces->end())
						{
							ExternalMeshFaces->insert(Face);
						}
						else
						{
							OtherFace = *itInner;
							ExternalMeshFaces->erase(itInner);
							delete OtherFace;
							delete Face;
						}
					}
				}
			}
		}
	}
	connectivity->SetCells (numCells, connec);
	connec->Delete();

	if (NodeCount != 4)
	{
		if (Layered)
		{
			for (itOuter = ThreeDFaces->begin(); itOuter != ThreeDFaces->end();  ++itOuter) 
			{
				ThreeDFace = *itOuter;
				Face = new mvSutraFace;
				for (FaceIndex = 0; FaceIndex < 4;  FaceIndex++) 
				{
					Face->push_back(ThreeDFace->node(FaceIndex));
				}
				ExternalLinesFaces->insert(Face);
			}
		}
		// For 3D meshes, ThreeDFaces contains all the faces that are on the boundary
		// of the mesh or all the faces in which all the nodes are on the same layer.  
		// One copy of each line between nodes needs to be placed in Faces.
		// This is done in a fashion analogous to how the 2D mesh lines were set up.
		for (itOuter = ThreeDFaces->begin(); itOuter != ThreeDFaces->end();  ++itOuter) 
		{
			ThreeDFace = *itOuter;
			for (FaceIndex = 0; FaceIndex < 3; FaceIndex++)
			{
				Face = new mvSutraFace;
				Face->push_back(ThreeDFace->node(FaceIndex));
				Face->push_back(ThreeDFace->node(FaceIndex+1));
				Face->sort();
				itInner = Faces->find(Face);
				if (itInner == Faces->end())
				{
					Faces->insert(Face);
				}
				else
				{
					delete Face;
				}
			}
			Face = new mvSutraFace;
			Face->push_back(ThreeDFace->node(0));
			Face->push_back(ThreeDFace->node(3));
			Face->sort();
			itInner = Faces->find(Face);
			if (itInner == Faces->end())
			{
				Faces->insert(Face);
			}
			else
			{
				delete Face;
			}
			delete ThreeDFace;
		}
		if (Layered)
		{
			for (itOuter = ExternalMeshFaces->begin(); itOuter != ExternalMeshFaces->end();  ++itOuter) 
			{
				ThreeDFace = *itOuter;
				for (FaceIndex = 0; FaceIndex < 4; FaceIndex++)
				{
					Face = new mvSutraFace;
					if (FaceIndex < 3)
					{
						Face->push_back(ThreeDFace->node(FaceIndex));
						Face->push_back(ThreeDFace->node(FaceIndex+1));
					}
					else
					{
						Face->push_back(ThreeDFace->node(FaceIndex));
						Face->push_back(ThreeDFace->node(0));
					}
					Face->sort();
					itInner = EMFaces->find(Face);
					if (itInner == EMFaces->end())
					{
						EMFaces->insert(Face);
					}
					else
					{
						delete Face;
					}
				}
				delete ThreeDFace;
			}
			for (itOuter = ExternalLinesFaces->begin(); itOuter != ExternalLinesFaces->end();  ++itOuter) 
			{
				ThreeDFace = *itOuter;
				for (FaceIndex = 0; FaceIndex < 4; FaceIndex++)
				{
					Face = new mvSutraFace;
					if (FaceIndex < 3)
					{
						Face->push_back(ThreeDFace->node(FaceIndex));
						Face->push_back(ThreeDFace->node(FaceIndex+1));
					}
					else
					{
						Face->push_back(ThreeDFace->node(FaceIndex));
						Face->push_back(ThreeDFace->node(0));
					}

					Face->sort();
					itInner = ELFaces->find(Face);
					if (itInner == ELFaces->end())
					{
						ELFaces->insert(Face);
					}
					else
					{
						OtherFace = *itInner;
						ELFaces->erase(itInner);
						delete OtherFace;
						delete Face;
					}
				}
				delete ThreeDFace;
			}
		}
	}
	delete ThreeDFaces;
	delete ExternalMeshFaces;
	delete ExternalLinesFaces;
	// For 2D meshes, Faces now contains all the element edges in the mesh.
	// For 3D meshes, Faces now contains all the element edtes that are on the outside of the mesh.

	// points will contain the coordinates of all the points in the mesh.

	vtkPoints *points = vtkPoints::New();
	for (int i = 0; i < m_NumberOfNodes; i++)
	{
		points->InsertNextPoint(m_ScalarGridCoordinates[i*3], 
			m_ScalarGridCoordinates[i*3+1], m_ScalarGridCoordinates[i*3+2]);
	}

	vtkPolyData *externalmesh = 0 ;
	vtkPolyData *externallines = 0;

	meshlines->SetPoints(points);
	for (i = 0; i < meshvector->GetSize(); i++)
	{
//		mvExternalMesh* mesh = meshvector->GetItem(i);
		externalmesh = meshvector->GetItem(i)->DataSet(); 
		externalmesh->SetPoints(points);
	}
	for (i = 0; i < linesvector->GetSize(); i++)
	{
		externallines = linesvector->GetItem(i)->DataSet(); 
		externallines->SetPoints(points);
	}
/*	if (Layered)
	{
		vtkIntArray *LayerNumbers = vtkIntArray::New();
		LayerNumbers->SetNumberOfValues(m_NumberOfNodes*2);
		for (int i = 0; i < m_NumberOfNodes; i++)
		{
			LayerNumbers->SetValue(i*2, i % Divisor);
			LayerNumbers->SetValue(i*2+1, i % Divisor);
		}
		externalmesh->GetPointData()->SetScalars(LayerNumbers);
		externallines->GetPointData()->SetScalars(LayerNumbers);
		LayerNumbers->Delete();

		

		vtkIntArray *EmLayerNumbers = vtkIntArray::New();
		for (itOuter = EMFaces->begin(); itOuter != EMFaces->end();  ++itOuter)
		{
			Face = *itOuter;
			EmLayerNumbers->InsertNextValue(Face->FirstSortedNode() % Divisor);
			EmLayerNumbers->InsertNextValue(Face->FirstSortedNode() % Divisor);
		}
		externalmesh->GetCellData()->SetScalars(EmLayerNumbers);
		EmLayerNumbers->Delete();

		vtkIntArray *ElLayerNumbers = vtkIntArray::New();
		for (itOuter = ELFaces->begin(); itOuter != ELFaces->end();  ++itOuter)
		{
			Face = *itOuter;
			ElLayerNumbers->InsertNextValue(Face->FirstSortedNode() % Divisor);
			ElLayerNumbers->InsertNextValue(Face->FirstSortedNode() % Divisor);
		}
		externallines->GetCellData()->SetScalars(ElLayerNumbers);
		ElLayerNumbers->Delete();
		
		
	}
	*/
	
	
	points->Delete();


	vtkCellArray *polys = vtkCellArray::New();
	polys->Allocate(1000, 1000);
	int *FaceArray = new int[2];
	for (itOuter = Faces->begin(); itOuter != Faces->end();  ++itOuter)
	{
		Face = *itOuter;
		for (int i = 0; i < 2; i++)
		{
			FaceArray[i] = Face->node(i);
		}
		polys->InsertNextCell(2, FaceArray);
		delete Face;
	}
	delete [] FaceArray;
	delete Faces;

	meshlines->SetPolys(polys);
	polys->Delete();


	if (Layered)
	{
		int Layer;
		std::vector<vtkCellArray*> polyMeahVec;
		for (i = 0; i < meshvector->GetSize(); i++)
		{
			polys = vtkCellArray::New();
			polys->Allocate(1000, 1000);
			polyMeahVec.push_back(polys);
		}


//		polys = vtkCellArray::New();
//		polys->Allocate(1000, 1000);
		FaceArray = new int[2];
		for (itOuter = EMFaces->begin(); itOuter != EMFaces->end();  ++itOuter)
		{
			Face = *itOuter;
			for (int i = 0; i < 2; i++)
			{
				FaceArray[i] = Face->node(i);
			}
			Layer = Face->FirstSortedNode() % Divisor;
			polys = polyMeahVec.at(Layer);

			polys->InsertNextCell(2, FaceArray);
			delete Face;
		}
		delete [] FaceArray;

//		externalmesh->SetPolys(polys);
//		polys->Delete();

		for (i = 0; i < meshvector->GetSize(); i++)
		{
			polys = polyMeahVec.at(i);
			externalmesh = meshvector->GetItem(i)->DataSet();
			externalmesh->SetPolys(polys);
			polys->Delete();

		}

		std::vector<vtkCellArray*> polyLinesVec;
		for (i = 0; i < linesvector->GetSize(); i++)
		{
			polys = vtkCellArray::New();
			polys->Allocate(1000, 1000);
			polyLinesVec.push_back(polys);
		}

//		polys = vtkCellArray::New();
//		polys->Allocate(1000, 1000);
		FaceArray = new int[2];
		for (itOuter = ELFaces->begin(); itOuter != ELFaces->end();  ++itOuter)
		{
			Face = *itOuter;
			for (int i = 0; i < 2; i++)
			{
				FaceArray[i] = Face->node(i);
			}
			Layer = Face->FirstSortedNode() % Divisor;
			polys = polyLinesVec.at(Layer);

			polys->InsertNextCell(2, FaceArray);
			delete Face;
		}
		delete [] FaceArray;

//		externallines->SetPolys(polys);
//		polys->Delete();
		for (i = 0; i < linesvector->GetSize(); i++)
		{
			polys = polyLinesVec.at(i);
			externallines = linesvector->GetItem(i)->DataSet();
			externallines->SetPolys(polys);
			polys->Delete();

		}
	}
	delete EMFaces;
	delete ELFaces;

	// In 2d meshes, the cells in shell define all the mesh lines in the mesh.
	// In 3D meshes, the cells in shell define the element outlines for the outside of the mesh.

	return;
}

int SutraDataSource::GetNumPoints()
{
	return m_NumberOfNodes;
}

int SutraDataSource::GetNumCells()
{
	return m_NumberOfElements;
}
