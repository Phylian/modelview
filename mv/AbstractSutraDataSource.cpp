// AbstractSutraDataSource.cpp: implementation of the AbstractSutraDataSource class.
//
//////////////////////////////////////////////////////////////////////

#include "AbstractSutraDataSource.h"
#include "mvUtil.h"
#include <fstream.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

AbstractSutraDataSource::AbstractSutraDataSource()
{
	m_FileType = 0;
	m_In1 = 0;
	m_In2 = 0;
	m_NumberOfNodes = 0;
	m_NumberOfElements = 0;

}

AbstractSutraDataSource::~AbstractSutraDataSource()
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

void AbstractSutraDataSource::GetDefaultModelFeatureColor(int i, float *rgba)
{
	float m_Rgba[4][4] = {
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

char *AbstractSutraDataSource::LoadData(char *dataFileList)
{

	// Parse the data file list;
	char inpFile[256];
	char code[4];
	char *pList = dataFileList;
	char *errmsg;
	int ISTEADYFLOW, ISTEADYTRANSPORT;
	ParseDataFileList(pList, m_NodFile);
	ParseDataFileList(pList, m_EleFile);
	ParseDataFileList(pList, inpFile);
	ParseDataFileList(pList, code);
	m_FileType = atoi(code);

	if (m_FileType == -1)	// ascii data
	{
		bool inputread = false;
		ISTEADYFLOW = 1;
		ISTEADYTRANSPORT = 1;
		// Read the inp file if used
		m_NumberOfModelFeatureTypes = 0;
		int len = strlen(inpFile);
		if (len > 0)
		{
			inputread = true;
			int ierror, istart, ibousz;
			istart = 0;
			GetInput(&ierror, &istart, &ibousz, 0 /*not used*/, 
				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, inpFile, len);
			if (ierror)
			{
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
			istart = 1;
			GetInput(&ierror, &istart, &ibousz, m_ModelFeatureArray, 
				&m_NumberOfModelFeatureTypes, &ISTEADYFLOW, &ISTEADYTRANSPORT, inpFile, len);
			if (ierror)
			{
				return "Error encountered while reading the inp file";
			}
			len = m_NumberOfModelFeatureTypes*40;
			m_ModelFeatureLabels = new char[len];
			memset(m_ModelFeatureLabels, ' ', len);
			GetLabels(m_ModelFeatureLabels, len);
		}

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
				m_NumberOfTimePoints, m_TimePointLabels, m_NumberOfScalarDataTypes,
				m_DataTypeLabels, m_ScalarGridCoordinates);
		if (errmsg)
		{
			return errmsg;
		}
		m_ScalarArray = new float[m_NumberOfNodes * m_NumberOfScalarDataTypes];

		// Read the ele file if used
		if (strlen(m_EleFile) > 0)
		{
			m_In2 = new ifstream;
			m_In2->open(m_EleFile, ios::in|ios::nocreate);
			if (!m_In2->is_open())
			{
				return "Unable to open ele file.";
			}
			m_VectorGridCoordinates = new float[3*m_NumberOfElements];
			errmsg = ReadEleFile(*m_In2, m_ScalarGridDim, m_NumberOfTimePoints,
								m_TimePointLabels, m_VectorGridCoordinates,
								inputread, ISTEADYFLOW, ISTEADYTRANSPORT, 
								m_NumberOfElementTimePoints);

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

void AbstractSutraDataSource::AdvanceOneTimePoint()
{
	SetTimePointTo(-1);
}

void AbstractSutraDataSource::SetTimePointTo(int timePointIndex)
{
	char line[1000];
	int i, j;
	float x, y, z;
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
			// skip over data type labels
			m_In1->read((unsigned char *) &n, sizeof(int));
			for (i=0; i<m_NumberOfScalarDataTypes; i++)
			{
				m_In1->read((unsigned char *) aString, 20*sizeof(char));
			}
			// skip over the time point labels
			m_In1->read((unsigned char *) &n, sizeof(int));
			for (i=0; i<m_NumberOfTimePoints; i++)
			{
				m_In1->read((unsigned char *) aString, 20*sizeof(char));
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

			// skip data
			for (i=0; i<timePointIndex; i++)
			{
				m_In1->read((unsigned char *) m_ScalarArray, 
					m_NumberOfScalarDataTypes*m_NumberOfNodes*sizeof(float));
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
		for (j=0; j< 5; j++)
		{
			m_In1->getline(line, 1000);
		}
		if (m_ScalarGridDim[2] == 1)   // 2D case
		{
			for (i=0; i<m_NumberOfNodes; i++)
			{
				(*m_In1) >> x >> y;
				for (j=0; j<m_NumberOfScalarDataTypes; j++)
				{
					(*m_In1) >> m_ScalarArray[i + j*m_NumberOfNodes];
				}
				m_In1->getline(line, 1000);
			}
		}
		else
		{
			for (i=0; i<m_NumberOfNodes; i++)
			{
				(*m_In1) >> x >> y >> z;
				for (j=0; j<m_NumberOfScalarDataTypes; j++)
				{
					(*m_In1) >> m_ScalarArray[i + j*m_NumberOfNodes];
				}
				m_In1->getline(line, 1000);
			}
		}
	}
	else  		// binary data
	{
		m_In1->read((unsigned char *) m_ScalarArray, 
			m_NumberOfScalarDataTypes*m_NumberOfNodes*sizeof(float));
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
	}
}

void AbstractSutraDataSource::SetScalarDataTypeTo(int dataTypeIndex)
{
	m_ScalarArrayOffset = dataTypeIndex * m_NumberOfNodes;
}

char *AbstractSutraDataSource::ReadNodFile(ifstream &in, int *sdim, int &numNodes,
		int &numElements, int &numTimePoints, char **&timePointLabels,
		int &numScalarDataTypes, char **&dataTypeLabels, float *&sgCoord)
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
		// read the mesh dimensions
		in.getline(line, 1000);
		if (strncmp(line+3, "3-D, REGULAR MESH", 17) == 0)
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
		else if (strncmp(line+3, "2-D, REGULAR MESH", 17) == 0)
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
		else
		{
			return "Error: nod file does not contain data for 3-D or 2-D regular mesh.";
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
	}
	catch(...)
	{
		return "Error encountered while reading the nod file";
	}

	// allocate space for time points labels
	timePointLabels = new char *[numTimePoints];
	for (i=0; i<numTimePoints; i++)
	{
		timePointLabels[i] = new char[40];
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
			strcpy(timePointLabels[i], p1);
			strcat(timePointLabels[i], " (");
			strcat(timePointLabels[i], timeStep);
			strcat(timePointLabels[i], ") ");

			// for first time step, find out how many scalar data types
			if (i==0)
			{
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

	// Allocate space for data type labels
	dataTypeLabels = new char *[numScalarDataTypes];
	for (i=0; i<numScalarDataTypes; i++)
	{
		dataTypeLabels[i] = new char[20];
	}

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
			for (i=0; i<numScalarDataTypes; i++)
			{
				strncpy(dataTypeLabels[i], line + 31 + (15*i), 15);
				dataTypeLabels[i][15] = '\0';
				mvUtil::TrimLeft(dataTypeLabels[i]);
			}
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
			for (i=0; i<numScalarDataTypes; i++)
			{
				strncpy(dataTypeLabels[i], line + 46 + (15*i), 15);
				dataTypeLabels[i][15] = '\0';
				mvUtil::TrimLeft(dataTypeLabels[i]);
			}
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

char *AbstractSutraDataSource::ReadEleFile(ifstream &in, int *sdim, int &numTimePoints, char **timePointLabels,
				  float *vgCoord, bool &inputread, int &SteadyFlow, int &SteadyTransport, int &ElementTimePoints)
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
			p1 = line+37;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 46;
			*p2 = '\0';
			if (atoi(p1) != sdim[0]-1)
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
		else
		{
			in.getline(line, 1000);
			p1 = line+25;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 34;
			*p2 = '\0';
			if (atoi(p1) != sdim[0]-1)
			{
				return "Error: ele file inconsistent with nod file";
			}
			*p2 = ')';
			p1 = line + 37;
			while (*p1 == ' ')
			{
				p1++;
			}
			p2 = line + 46;
			*p2 = '\0';
			if (atoi(p1) != sdim[1]-1)
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
			if (atoi(p1) != sdim[2]-1)
			{
				return "Error: ele file inconsistent with nod file";
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
			strcpy(tpLabel, p1);
			strcat(tpLabel, " (");
			strcat(tpLabel, timeStep);
			strcat(tpLabel, ") ");

			// The first time step in the node file is time step 0.
			// The first time step in the element file is time step 1.
			// Time step 1 must also be in the node file.
			if (ElementTimePoints == numTimePoints)
			{
				if (strcmp(timePointLabels[i], tpLabel) != 0)
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
			int numElements = (sdim[0]-1)*(sdim[1]-1);
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

			int numElements = (sdim[0]-1)*(sdim[1]-1)*(sdim[2]-1);
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

char *AbstractSutraDataSource::LoadBinaryFile()
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
	float version = (float) atof(header+13);
	if (fabs(version - 1.00) > 0.0001)
	{
		return "Cannot read an older version of Sutra binary data.";
	}
	m_In1->read((unsigned char *) m_ScalarGridDim, 3*sizeof(int));
	m_In1->read((unsigned char *) &m_NumberOfScalarDataTypes, sizeof(int));
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
		m_TimePointLabels[i] = new char[20];
		m_In1->read((unsigned char *) m_TimePointLabels[i], 20*sizeof(char));
	}
	m_NumberOfNodes = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
	m_NumberOfElements = (m_ScalarGridDim[0]-1)*(m_ScalarGridDim[1]-1);
	if (m_ScalarGridDim[2] > 1)
	{
		m_NumberOfElements *= (m_ScalarGridDim[2]-1);
	}
	m_ScalarGridCoordinates = new float[3*m_NumberOfNodes];
	m_In1->read((unsigned char *) m_ScalarGridCoordinates, m_NumberOfNodes*3*sizeof(float));
	m_In1->read((unsigned char *) &hasVelocity, sizeof(int));
	if (hasVelocity)
	{
		m_VectorGridCoordinates = new float[3*m_NumberOfElements];
		m_In1->read((unsigned char *) m_VectorGridCoordinates, m_NumberOfElements*3*sizeof(float));
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
	GetLabels(m_ModelFeatureLabels, len);

	m_ScalarArray = new float[m_NumberOfNodes * m_NumberOfScalarDataTypes];

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

char *AbstractSutraDataSource::AtoBConvert1(ifstream &nod, ifstream &ele, const char *inpFile, ofstream &bin, 
			int &numNodes, int &numElements, int &numScalarDataTypes, int &numTimePoints, int &nz)
{
	int i;
	float *sgCoord = 0;
	float *vgCoord = 0;
	char **timePointLabels = 0;
	char **dataTypeLabels = 0;
	int ISTEADYFLOW, ISTEADYTRANSPORT;
	int sdim[3];
	int hasVelocity = 0;

	// Read and datasets 1-4 of input file.
		bool inputread = false;
	int len = strlen(inpFile);
	if (len > 0)
	{
		inputread = true;
		int ierror, istart, nfeatures, ibousz;
		istart = 0;
		GetInput(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
			&ISTEADYTRANSPORT, (char *) inpFile, len);
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
	char *errmsg = ReadNodFile(nod, sdim, numNodes, numElements, numTimePoints,
			timePointLabels, numScalarDataTypes, dataTypeLabels, sgCoord);
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
	nz = sdim[2];
	// Read the ele file if used
	if (ele.is_open())
	{
		vgCoord = new float[3*numElements];
		int tempElementTimePoint = 0;
		errmsg = ReadEleFile(ele, sdim, numTimePoints, timePointLabels, vgCoord, 
			inputread, ISTEADYFLOW, ISTEADYTRANSPORT, tempElementTimePoint);
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

	// Write grid data
	char header[20];
	strcpy(header, "Sutra binary 1.00  ");
	bin.write((unsigned char *) header, 20*sizeof(char));
	bin.write((unsigned char *) &sdim, 3*sizeof(int));
	bin.write((unsigned char *) &numScalarDataTypes, sizeof(int));
	for (i=0; i<numScalarDataTypes; i++)
	{
		bin.write((unsigned char *) dataTypeLabels[i], 20*sizeof(char));
	}
	bin.write((unsigned char *) &numTimePoints, sizeof(int));
	for (i=0; i<numTimePoints; i++)
	{
		bin.write((unsigned char *) timePointLabels[i], 20*sizeof(char));
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

	// Read and write model feature data
	len = strlen(inpFile);
	if (len > 0)
	{
		int ierror, istart, nfeatures, ibousz;
		istart = 0;
		GetInput(&ierror, &istart, &ibousz, 0, &nfeatures, &ISTEADYFLOW, 
			&ISTEADYTRANSPORT, (char *) inpFile, len);
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
		int *ibnode = new int[ibousz];
		istart = 1;
		GetInput(&ierror, &istart, &ibousz, ibnode, &nfeatures, &ISTEADYFLOW, 
			&ISTEADYTRANSPORT, (char *) inpFile, len);
		if (ierror)
		{
			delete [] ibnode;
			return "Error encountered while reading inp file";
		}
		len = 40*nfeatures;
		char *flabels = new char[len];
		GetLabels(flabels, len);
		bin.write((unsigned char *) &nfeatures, sizeof(int));
		bin.write((unsigned char *) flabels, 40*nfeatures*sizeof(char));
		bin.write((unsigned char *) &ibousz, sizeof(int));
		bin.write((unsigned char *) ibnode, ibousz*sizeof(int));
		delete [] ibnode;
		ibnode = 0;
		delete [] flabels;
		flabels = 0;
	}
	else
	{
		int nfeatures = 0;
		bin.write((unsigned char *) &nfeatures, sizeof(int));
	}
	return 0;
}

char *AbstractSutraDataSource::AtoBConvert2(ifstream &nod, ifstream &ele, ofstream &bin, int timePoint,
		int numNodes, int numElements, int numScalarDataTypes, int numTimePoints, int nz)
{
	char line[1000];
	float x, y, z;
	float *array = new float[numNodes*numScalarDataTypes];
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
				for (i=0; i<numTimePoints+12; i++)
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
				for (j=0; j<numScalarDataTypes; j++)
				{
					nod >> array[j*numNodes + i];
				}
				nod.getline(line, 1000);
			}
		}
		else
		{
			for (i=0; i<numNodes; i++)
			{
				nod >> x >> y >> z;
				for (j=0; j<numScalarDataTypes; j++)
				{
					nod >> array[j*numNodes + i];
				}
				nod.getline(line, 1000);
			}
		}
		bin.write((unsigned char *) array, numNodes*numScalarDataTypes*sizeof(float));
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
