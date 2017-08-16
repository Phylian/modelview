#include "AbstractModflowDataSource.h"
#include "ModpathReader.h"
#include "ExternalModflowFile.h"
#include "mvUtil.h"
#include <fstream.h>
#include <stdio.h>
#include <string.h>

#define MAX_NUMBER_OF_MODEL_FEATURES 16

AbstractModflowDataSource::AbstractModflowDataSource() : mvDataSource()
{
	m_Delr = 0;
	m_Delc = 0;
	m_Elev = 0;
	m_TrueElev = 0;
	m_Ibound = 0;
	m_IsMfLayer = 0;
	
	for (int i=0; i<11; i++)
	{
		m_ScalarUnit[i] = 0;
	}
	m_VectorUnit = 0;
	m_UseElevFile = 0;
	m_UseExternalFile = 0;
	m_UnitOffset = 0;
	m_NormalizeVector = 1;
	m_NumberOfScalarDataTypes = 2;	// default is head and K
	m_Transport = 0;
	m_ErrorMsg[0] = 0;
	m_ErrorMsg[1] = "Error encountered while loading data.";  // Generic unspecified error
	m_ErrorMsg[2] = "Error: Unable to open the name file.";
	m_ErrorMsg[3] = "Error: Unable to open the elevation file.";
	m_ErrorMsg[4] = "Error encountered while reading the name file.";
	m_ErrorMsg[5] = "Error encountered while reading the name file for transport package.";
	m_ErrorMsg[6] = "Error encountered while opening data file.";
	m_ErrorMsg[7] = "Error encountered while reading data file.";
	m_ErrorMsg[8] = "Error: No scalar data.";
	m_ErrorMsg[9] = "Error encountered while reading scalar data file.";
	m_ErrorMsg[10] = "Error encountered while reading elevation file.";
	m_ErrorMsg[11] = "Error: Ground-Water Transport not invoked in name file.";
	m_ErrorMsg[12] = "Error 12.";
	m_ErrorMsg[13] = "Error 13.";
	m_ErrorMsg[14] = "Error 14.";
	m_ErrorMsg[15] = "Error 15.";
	m_ErrorMsg[16] = "Error 16.";
	m_ErrorMsg[17] = "Error 17.";
	m_ErrorMsg[18] = "Error 18.";
	m_ErrorMsg[19] = "Error: Internal program error encountered.";
	// The following error message is returned whenever the original Fortran
	// code would have encountered a STOP statement
	m_ErrorMsg[20] = "Error encountered while reading Modflow data files.";
	m_ErrorMsg[21] = "Error reading external file.";

	m_ModelFeatureLabels = new char[MAX_NUMBER_OF_MODEL_FEATURES*40];
	char *p = m_ModelFeatureLabels;
	strncpy(p, "fixed head                              ", 40);
	p += 40;
	strncpy(p, "wells                                   ", 40);
	p += 40;
	strncpy(p, "drains                                  ", 40);
	p += 40;
	strncpy(p, "rivers                                  ", 40);
	p += 40;
	strncpy(p, "streams                                 ", 40);
	p += 40;
	strncpy(p, "reservoirs                              ", 40);
	p += 40;
	strncpy(p, "general-head                            ", 40);
	p += 40;
	strncpy(p, "tm-var spec head                        ", 40);
	p += 40;
	strncpy(p, "trans spec flow                         ", 40);
	p += 40;
	strncpy(p, "trans spec head                         ", 40);
	p += 40;
	strncpy(p, "drain w return flow                     ", 40);
	p += 40;
	strncpy(p, "diff analogy flow                       ", 40);
	p += 40;
	strncpy(p, "multi-node wells                        ", 40);
	p += 40;
	strncpy(p, "streams (SFR1)                          ", 40);
	p += 40;
	strncpy(p, "lake                                    ", 40);
	p += 40;
	strncpy(p, "gwt observations wells                  ", 40);
}

AbstractModflowDataSource::~AbstractModflowDataSource()
{
	ReleaseMemory();
	// Note that the concrete subclass should also deallocate Fortran arrays
	// and close all units during destruction
}

void AbstractModflowDataSource::ReleaseMemory()
{
	mvDataSource::ReleaseMemory();
	if (m_Delr != 0)
	{
		delete [] m_Delr;
		m_Delr = 0;
	}
	if (m_Delc != 0)
	{
		delete [] m_Delc;
		m_Delc = 0;
	}
	if (m_Elev != 0)
	{
		delete [] m_Elev;
		m_Elev = 0;
	}
	if (m_TrueElev != 0)
	{
		delete [] m_TrueElev;
		m_TrueElev = 0;
	}
	if (m_Ibound != 0)
	{
		delete [] m_Ibound;
		m_Ibound = 0;
	}
	if (m_IsMfLayer != 0)
	{
		delete [] m_IsMfLayer;
		m_IsMfLayer = 0;
	}
}

void AbstractModflowDataSource::GetDefaultModelFeatureColor(int i, float *rgba)
{
	float m_Rgba[MAX_NUMBER_OF_MODEL_FEATURES][4] = {
		{.67, .67, .67, 1},		// fixed heads  --  light gray
		{1.0, 0.5, 1.0, 1},		// wells  --  light purple
		{0.5, 1.0, 0.5, 1},		// drains  --  light green
		{0.4, 0.7, 0.7, 1},		// rivers  --  blue gray
		{0.0, 0.5, 1.0, 1},		// streams --  royal blue
		{1.0, 1.0, 0.0, 1},		// reservoirs -- yellow
		{1.0, 0.5, .25, 1},		// general head -- orange
		{1.0, .16, .16, 1},	 	// time varying spec head  --  red
		{.82, .64, .64, 1},	    // transient specified flow  --  light brown
		{0.5, .25, .25, 1},		// transient specified head  -- dark brown
		{0.0, 0.5, 0.25, 1},	// drains with return flow --  dark green
		{0.0, 1.0, 1.0, 1},		// diff analogy flow --  cyan
		{1.0, 0.0, 1.0, 1},		// multi-node wells  -- purple 
		{0.5, 0.5, 1.0, 1},		// streams (SFR1) -- purplish blue 
		{0.58, 0.82, 0.95, 1},	// lake -- light grayish blue 
		{0.5, 0.84, 0.41, 1}	// GWT observations wells -- light grayish green 
	};

	rgba[0] = m_Rgba[i][0];
	rgba[1] = m_Rgba[i][1];
	rgba[2] = m_Rgba[i][2];
	rgba[3] = m_Rgba[i][3];
}

char *AbstractModflowDataSource::LoadData(char *dataFileList)
{
	int i, j, k, ierror, ncol, nrow, nlay, len1, len2, timeunit;
	int nx, ny, nz, nxy, nxyz, ncr, ncrl, scalarArraySize;
	int modelFeatureArraySize;
	float xoffset, yoffset;
	char elevFile[256];
	char nameFile[256];
	char pathlineFile[256];
	char externalFile[256];
	char code[4];

	// Save the dataFileList. This is necessary because the mvDoc checks
	// this variable to determine if data has been loaded.
	m_DataFileList = new char[strlen(dataFileList) + 1];
	strcpy(m_DataFileList, dataFileList);

	// Parse the data file list;
	char *pList = dataFileList;
	ParseDataFileList(pList, nameFile);
	if (m_UseElevFile)
	{
		ParseDataFileList(pList, elevFile);
	}
	ParseDataFileList(pList, pathlineFile);
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
	m_DataType = atoi(code);
	bool CodeSaved = ParseDataFileList(pList, code);
	if (CodeSaved)
	{
		m_PathsBackwardsInTime = atoi(code) != 0;
	}
	else
	{
		m_PathsBackwardsInTime = 0;
	}

	// Make sure the pathline file exists by opening and then closing it.
	if (strlen(pathlineFile) > 0)
	{
		ifstream in(pathlineFile, ios::in|ios::nocreate);
		if (!in.is_open())
		{
			return "Unable to open the pathline file.";
		}
	    in.close();
	}

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

	// Read the grid dimensions.
	len1 = strlen(nameFile);
	len2 = strlen(elevFile);
	MfDims(&ierror, &ncol, &nrow, &nlay, &timeunit, nameFile, elevFile, len1, len2);
	if (ierror != 0)
	{
		return m_ErrorMsg[ierror];
	}

	// Allocate memory for arrays
	nx = ncol + 1;
	ny = nrow + 1;
	nz = nlay + 1;
	nxy = nx*ny;
	nxyz = nx*ny*nz;
	ncr = ncol*nrow;
	ncrl = ncol*nrow*nlay;
	m_TimeUnit = timeunit;
	m_UnitOffset = nxyz + ncrl;

	m_Delr = new float[ncol];
	m_Delc = new float[nrow];
	m_Elev = new float[ncol*nrow*(nlay+1)];
	m_TrueElev = new float[ncol*nrow*(nlay+1)];
	m_Ibound = new int [ncol*nrow*nlay];
	m_ScalarArray = new float[ncrl];	// for temporary storage of K
	m_IsMfLayer = new int[nlay];	// 1 = layer is a quasi 3D confining layer, 0 = not. Indexing from bottom to top.

	for (i=0; i<nlay; i++)
	{
		m_IsMfLayer[i] = 1;
	}

	// Get the grid spacings, inactive cells, etc.
	// m_ScalarUnit get filled with the unit numbers of the files containing
	// scalar data: heads, drawdown, various subsidence arrays and sensitivity.
	MfGrid(&ierror, m_Delr, m_Delc, m_Elev, m_Ibound, m_ScalarArray, 
		&m_InactiveValue1, &m_InactiveValue2, m_ScalarUnit, &m_VectorUnit, &xoffset, &yoffset, 
		m_IsMfLayer);
	if (ierror) 
	{
		return m_ErrorMsg[ierror];
	}

	if (m_ScalarUnit[0] == 0) 
	{
		return m_ErrorMsg[8];
	}

	// Set null values for in Hydraulic conductivity and set positive ibound elements to 1,
	// and negative ibound values to -1 
	for (i=0; i<ncrl; i++)
	{
		if (m_Ibound[i] == 0)
		{
			m_ScalarArray[i] = m_InactiveCellValue;
		}
		else if (m_Ibound[i] > 0)
		{
			m_Ibound[i] = 1;
		}
		else if (m_Ibound[i] < 0)
		{
			m_Ibound[i] = -1;
		}
	}


	int numExternalDataSets = 0;
	int iError = 0;
	// If used, get the number of data arrays in the external file.and use to set
	// the length of the first dimension of dataType.
	if (ExtFileLength > 0)
	{
		ExternalModflowFile::CountDataSets(externalFile, numExternalDataSets, iError);
		if (iError != 0)
		{
			return m_ErrorMsg[21];
		}
	}

	// count the first scalar file
	const int dataTypeLength = 17;


	char ** dataType = new char *[11+numExternalDataSets];
	for (i=0; i<11+numExternalDataSets; i++)
	{
		dataType[i] = new char[dataTypeLength];
		dataType[i][0] = '\0';
	}


//	char dataType[11+numExternalDataSets][dataTypeLength];
	MfCountScalars(&ierror, m_ScalarUnit, &m_NumberOfTimePoints, dataType[0], dataTypeLength);
	if (ierror>0) 
	{
		for (i=0; i<11+numExternalDataSets; i++)
		{
			delete [] dataType[i];
		}
		delete dataType;
		return m_ErrorMsg[9];
	}
	if (m_NumberOfTimePoints == 0) 
	{
		for (i=0; i<11+numExternalDataSets; i++)
		{
			delete [] dataType[i];
		}
		delete dataType;
		return m_ErrorMsg[8];
	}
	dataType[0][16]='\0';
	mvUtil::TrimLeft(dataType[0]);
	mvUtil::ToLowerCase(dataType[0]);
	m_NumberOfScalarDataTypes = 1;

	// count the remaining scalar files, if used
	j = 1;
	for (i=1; i<11; i++)
	{
		if (m_ScalarUnit[j] == 0) break;
		int numTimePoints1;
		MfCountScalars(&ierror, m_ScalarUnit+j, &numTimePoints1, dataType[j], dataTypeLength);
		if (ierror>0) 
		{
			// if error is encountered when counting the j-th scalar file,
			// eliminate it from the scalar unit list and shift later elements upward.
			for (k=j;k<10;k++) 
			{
				m_ScalarUnit[k] = m_ScalarUnit[k+1];
			}
			m_ScalarUnit[10] = 0;
		}
		else
		{
			dataType[j][16]='\0';
			mvUtil::TrimLeft(dataType[j]);
			mvUtil::ToLowerCase(dataType[j]);
			j++;
			m_NumberOfScalarDataTypes++;
		}
	}

	// For flow simulation (not transport), also include hydraulic conductivity
	// as a data type.
	int KPosition = m_NumberOfScalarDataTypes;
	if (!m_Transport) 
	{
		strcpy(dataType[m_NumberOfScalarDataTypes], "K along rows");
		m_NumberOfScalarDataTypes++;
	}

	int ExternPosition = m_NumberOfScalarDataTypes;
	// If used, read the data array names from the external file and update
	// dataType and m_NumberOfScalarDataTypes
	if (ExtFileLength > 0)
	{
		char ** ExternalDataType = new char *[numExternalDataSets];
		for (i=0; i<numExternalDataSets; i++)
		{
			ExternalDataType[i] = new char[dataTypeLength];
			ExternalDataType[i][0] = '\0';
		}

		ExternalModflowFile::GetDataLabels(externalFile, dataTypeLength, ExternalDataType, iError);

		if (iError == 0)
		{
			for (i=0; i<numExternalDataSets; i++)
			{
				strcpy(dataType[m_NumberOfScalarDataTypes+i], ExternalDataType[i]);
			}
		}


		for (i=0; i<numExternalDataSets; i++)
		{
			delete [] ExternalDataType[i];
		}
		delete ExternalDataType;

		if (iError != 0)
		{
			for (i=0; i<11+numExternalDataSets; i++)
			{
				delete [] dataType[i];
			}
			delete dataType;
			return m_ErrorMsg[21];
		}
		m_NumberOfScalarDataTypes = m_NumberOfScalarDataTypes + numExternalDataSets;
	}

	// Count the vector file, if used
	if (m_VectorUnit != 0)
	{
		MfCountVectors(&ierror, &m_VectorUnit);
		if (ierror)
		{
			m_VectorUnit=0;
		}
	}

	// Store the data types.
	m_DataTypeLabels = new char *[m_NumberOfScalarDataTypes];
	for (i=0; i<m_NumberOfScalarDataTypes; i++)
	{
		m_DataTypeLabels[i] = new char[20];
		strcpy(m_DataTypeLabels[i], dataType[i]);
	}
	for (i=0; i<11+numExternalDataSets; i++)
	{
		delete [] dataType[i];
	}
	delete dataType;

	// Get the time points and store them as strings.
	float *timePoints = new float [m_NumberOfTimePoints];
	int *periods = new int [m_NumberOfTimePoints];
	int *steps = new int [m_NumberOfTimePoints];
	int *moves = new int [m_NumberOfTimePoints];
	MfGetTimePoints(timePoints, periods, steps, moves, m_NumberOfTimePoints);
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	char p[10];
	for (i=0; i<m_NumberOfTimePoints; i++)
	{
		m_TimePointLabels[i] = new char[35];
		sprintf(m_TimePointLabels[i], "%.4g", timePoints[i]);
		mvUtil::TrimRight(m_TimePointLabels[i]);
		if (GetTimeUnit() != "")
		{
			strcat(m_TimePointLabels[i], " ");
			strcat(m_TimePointLabels[i], GetTimeUnit());
			strcat(m_TimePointLabels[i], " ");
		}

		if (periods[i] > 0 || steps[i] > 0)
		{
			strcat(m_TimePointLabels[i], " (");
			sprintf(p,"%-4u",periods[i]);
			mvUtil::TrimRight(p);
			strcat(m_TimePointLabels[i], p);
			sprintf(p,"-%-4u",steps[i]);
			mvUtil::TrimRight(p);
			strcat(m_TimePointLabels[i], p);
			if (moves[i] > 0)
			{
				sprintf(p,"-%-4u",moves[i]);
				mvUtil::TrimRight(p);
				strcat(m_TimePointLabels[i], p);
			}
			strcat(m_TimePointLabels[i], ")");
		}
	}
	delete [] timePoints;
	delete [] periods;
	delete [] steps;
	delete [] moves;

	// Enlarge the scalar array
	float *buffer = m_ScalarArray;
	scalarArraySize = m_NumberOfScalarDataTypes*(nxyz + ncrl);
	m_ScalarArray = new float[scalarArraySize];
	for (i=0; i<scalarArraySize; i++)
	{
		m_ScalarArray[i] = 0;
	}

	// Compute pointers to hydraulic conductivity data
	float *pointConductivity = m_ScalarArray + KPosition*m_UnitOffset;
	float *cellConductivity = pointConductivity + nxyz;
	if (!m_Transport)
	{
		memcpy(cellConductivity, buffer, ncrl*sizeof(float));
	}
	delete [] buffer;
	buffer = 0;

	// Get the data from external files, if any.
	float *pointExtValue = 0;
	float *cellExtValue = 0;
	if (ExtFileLength > 0)
	{
		float *ExternalCellValues = new float[ncrl*numExternalDataSets];
		ExternalModflowFile::ReadData(externalFile, iError, nlay, nrow, ncol, ExternalCellValues);

		if (iError != 0)
		{
			delete [] ExternalCellValues;
			return m_ErrorMsg[21];
		}


		for (int ExternalIndex = 0; ExternalIndex < numExternalDataSets; ExternalIndex++)
		{
			pointExtValue = m_ScalarArray + (ExternPosition+ExternalIndex)*m_UnitOffset;
			cellExtValue = pointExtValue + nxyz;
			for (int ValueIndex = 0; ValueIndex < ncrl; ValueIndex++)
			{
				cellExtValue[ValueIndex] = ExternalCellValues[(ExternalIndex*ncrl)+ValueIndex];
			}
		}


		delete [] ExternalCellValues;
	}


	// Count model features
	MfCountFeatures(&ierror, &modelFeatureArraySize, &m_NumberOfModelFeatureTypes, m_Ibound);
	if (ierror)
	{
		return m_ErrorMsg[20];
	}

	// Compute point coordinates in x and y directions
	double xx,yy;
	float *x = new float[nx];
	float *y = new float[ny];
	float *z = new float[nxyz];
	xx = xoffset;
	x[0] = xx;
	for (i=0; i<ncol; i++)
	{
		xx += m_Delr[i];
		x[i+1] = xx;
	}
	yy = yoffset;
	y[0] = yy;
	for (i=0; i<nrow; i++)
	{
		yy += m_Delc[i];
		y[i+1] = yy;
	}

	// Compute coordinates at cell center for drawing vectors. Do this before m_Elev is modified
	if (m_VectorUnit != 0)
	{
		m_VectorArray = new float[3*ncrl];
		m_VectorGridDim[0] = nx - 1;
		m_VectorGridDim[1] = ny - 1;
		m_VectorGridDim[2] = nz - 1;
		m_VectorGridCoordinates = new float[3*ncrl];

		int m = 0;
		for (k=0; k<nlay; k++)
		{
			for (j=0; j<nrow; j++)
			{
				for (i=0; i<ncol; i++)
				{
					m_VectorGridCoordinates[3*m] = (x[i] + x[i+1])/2;
					m_VectorGridCoordinates[3*m+1] = (y[j] + y[j+1])/2;;
					m_VectorGridCoordinates[3*m+2] = (m_Elev[m] + m_Elev[m+ncr])/2;
					m++;
				}
			}
		}
	}

	// Store the original elevations in m_TrueElev for use with particles.
	for (k=0; k<ncrl+ncr; k++)
	{
		m_TrueElev[k] = m_Elev[k];
	}
	// Modify cell bottom elevation for inactive cells. The modified array is used for interpolation.
	float znull = -1.0e20f;
	// For the bottom layer of cells, set the cell bottom elevation
	// to the null value if the cell is inactive. 
	// This layer is always an aqufer layer.
	for (k=0; k<ncr; k++)
	{
		if (m_Ibound[k] == 0)
		{
			m_Elev[k] = znull;
		}
	}
	// Work from the second-to-bottom cell layer upward to the top cell layer
	for (k=ncr; k<ncrl; k++)
	{
		// if the cell layer represents an aquifer layer, set the cell
		// bottom elevation to the null value if the cell is inactive.
		if (m_IsMfLayer[k/ncr])
		{
			if (m_Ibound[k] == 0)
			{
				if (m_IsMfLayer[k/ncr-1])
				{
					if (m_Ibound[k-ncr] == 0)
					{
						m_Elev[k] = znull;
					}
				}
				else
				{
					m_Elev[k] = znull;
				}
			}
		}
		// if the cell layer represents a confining layer, set the cell
		// bottom elevation to the null value if the underlying cell is 
		// inactive
		else
		{
			if (m_Ibound[k-ncr] == 0)
			{
				m_Elev[k] = znull;
			}
		}
	}
	// For the top most cell layer, set the top elevation to the null value
	// if the cell in the layer is inactive.
	// The top most cell layer is always an aquifer layer.
	for (k=ncrl-ncr; k<ncrl; k++)
	{
		if (m_Ibound[k] == 0)
		{
			m_Elev[k+ncr] = znull;
		}
	}

	m_ScalarGridCoordinates = new float[3*nxyz];
	for (k=0; k<nz; k++)
	{
		mvUtil::interp2d(x, y, z+k*nxy, m_Delr, m_Delc, m_Elev+k*ncr, nrow, ncol, znull);
		int m=k*nxy;
		for (j=0; j<ny; j++)
		{
			for (i=0; i<nx; i++)
			{
				m_ScalarGridCoordinates[3*m] = x[i];
				m_ScalarGridCoordinates[3*m+1] = y[j];
				m_ScalarGridCoordinates[3*m+2] = z[m];
				m++;
			}
		}
	}
	// Check elevations
	// At layer edge, interpolated elevation in a deeper layer could be higher 
	// than interpolated elevation in a shallower layer if the "footprint" of
	// the deeper layer is larger than the shallower layer, and if the deeper
	// layer has a bowl shape.
	int m1, m2;
	float *c = m_ScalarGridCoordinates;
	float ave;
	for (j=0; j<ny; j++)
	{
		for (i=0; i<nx; i++)
		{
			for (k=0; k<nz-1; k++)
			{
				m1 = 3*((k*nxy)+j*nx+i) + 2;
				m2 = 3*(((k+1)*nxy)+j*nx+i) + 2;
				if (c[m1]!=znull && c[m2]!=znull && c[m1] > c[m2])
				{
					ave = (c[m1] + c[m2])/2;
					c[m1] = ave;
					c[m2] = ave;
					// propagate the change downward
					for (int kk = k-1; kk>=0; kk--)
					{
						m1 = 3*((kk*nxy)+j*nx+i) + 2;
						if (c[m1]!=znull && c[m1] > ave)
						{
							c[m1] = ave;
						}
					}
				}
			}
		}
	}
	
	// For each layer, replace the znull values by the average of non-null values.
	// These points will not be displayed, but they presence of znull elevation
	// points will affect the size of the bounding box.
	for (k=0; k<nz; k++)
	{
		// compute the average of non null elevation
		float zave = 0;
		int count = 0;
		int index;
		for (j=0; j<nxy; j++)
		{
			index = 3*(k*nxy + j) + 2;
			if (c[index]!=znull)
			{
				zave += c[index];
				count++;
			}
		}
		if (count > 0)
		{
			zave /= count;
		}
		for (j=0; j<nxy; j++)
		{
			index = 3*(k*nxy + j) + 2;
			if (c[index]==znull)
			{
				c[index] = zave;
			}
		}

	}

	m_ScalarGridDim[0] = nx;
	m_ScalarGridDim[1] = ny;
	m_ScalarGridDim[2] = nz;

	// Set null values for hydraulic conductivity if cell thickness is zero
	// and do point interpolation
	if (!m_Transport)
	{
		for (i=0; i<ncrl; i++)
		{
			if (m_Elev[i] == m_Elev[i+ncr])
			{
				cellConductivity[i] = m_InactiveCellValue;
			}
		}
		mvUtil::interp3d(cellConductivity, pointConductivity, m_Delr, m_Delc, m_Elev,
							ncol, nrow, nlay, m_InactiveCellValue, znull);
	}

	// Set null values for external data if cell thickness is zero
	// and do point interpolation
	if (ExtFileLength > 0)
	{
		for (int ExternalIndex = 0; ExternalIndex < numExternalDataSets; ExternalIndex++)
		{
			pointExtValue = m_ScalarArray + (ExternPosition+ExternalIndex)*m_UnitOffset;
			cellExtValue = pointExtValue + nxyz;
			for (i=0; i<ncrl; i++)
			{
				if (m_Elev[i] == m_Elev[i+ncr])
				{
					cellExtValue[i] = m_InactiveCellValue;
				}
			}
			mvUtil::interp3d(cellExtValue, pointExtValue, m_Delr, m_Delc, m_Elev,
							ncol, nrow, nlay, m_InactiveCellValue, znull);
		}

	}

	// Read pathline data
	if (strlen(pathlineFile) > 0)
	{
		ModpathReader::ReadData(pathlineFile, m_NumberOfPathlines,
			m_NumberOfPathlineCoordinates, m_PathlineCoordinates, 
			m_PathlineScalarArray, m_MaxTimePathlineScalarArray, 
			m_MinTimePathlineScalarArray, m_PathlinePointArray, 
			m_PathsBackwardsInTime, m_MinPositiveTime);
	}

	delete [] x;
	delete [] y;
	delete [] z;

	// Read model features (wells, rivers, etc)
	m_ModelFeatureArray = new int[modelFeatureArraySize];
	for (i=0; i<modelFeatureArraySize; i++)
	{
		m_ModelFeatureArray[i] = 0;
	}
	m_StressPeriod = 0;
	
	return 0;
}

void AbstractModflowDataSource::SetScalarDataTypeTo(int dataTypeIndex)
{
	m_ScalarArrayOffset = dataTypeIndex * m_UnitOffset;
}


void AbstractModflowDataSource::AdvanceOneTimePoint()
{
	SetTimePointTo(-1);
}

void AbstractModflowDataSource::SetTimePointTo(int timePointIndex)
{
	int ierror, ncol, nrow, nlay, i, j, k, ncr, per;
	int nameFileUnit = 99;
	ncol = m_ScalarGridDim[0]-1;
	nrow = m_ScalarGridDim[1]-1;
	nlay = m_ScalarGridDim[2]-1;
	ncr = ncol*nrow;
	int nxyz = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
	int ncrl = ncol*nrow*nlay;
	float *pointValues = m_ScalarArray;
	float *cellValues = pointValues + nxyz;

	MfGetScalars(&ierror, m_ScalarUnit, cellValues, &timePointIndex, &per);
	for (i=0; i<ncrl; i++)
	{
		if (m_Ibound[i] == 0 || cellValues[i] == m_InactiveValue1 || cellValues[i] == m_InactiveValue2)
		{
			cellValues[i] = m_InactiveCellValue;
		}
	}
	float znull = -1.0e20f;
	mvUtil::interp3d(cellValues, pointValues, m_Delr, m_Delc, m_Elev,
						ncol, nrow, nlay, m_InactiveCellValue, znull, m_IsMfLayer);

	for (k=1; k<m_NumberOfScalarDataTypes; k++)
	{
		if (m_ScalarUnit[k] != 0)
		{
			pointValues = cellValues + ncrl;
			cellValues = pointValues + nxyz;
			MfGetScalars(&ierror, m_ScalarUnit+k, cellValues, &timePointIndex, &per);
			for (i=0; i<ncrl; i++)
			{
				if (m_Ibound[i] == 0 || cellValues[i] == m_InactiveValue1 || cellValues[i] == m_InactiveValue2)
				{
					cellValues[i] = m_InactiveCellValue;
				}
			}
			mvUtil::interp3d(cellValues, pointValues, m_Delr, m_Delc, m_Elev,
								ncol, nrow, nlay, m_InactiveCellValue, znull, m_IsMfLayer);
		}
	}

	if (m_VectorArray != 0)
	{
		MfGetVectors(&ierror, &m_VectorUnit, m_VectorArray, &timePointIndex);
		if (ierror)
		{
			for (i=0; i<3*ncrl; i++)
			{
				m_VectorArray[i] = 0;
			}
		}
		int m = 0;
		for (k=0; k<nlay; k++)
		{
			for (j=0; j<nrow; j++)
			{
				for (i=0; i<ncol; i++)
				{
					if (m_Ibound[m] == 0 || m_Ibound[m]==22 /*test for lake cells*/ ||
						m_ScalarArray[nxyz+m] == m_InactiveCellValue /* test for inactive scalar value*/)
					{
						m_VectorArray[3*m] = m_InactiveCellValue;
					}
					else
					{
						if(m_NormalizeVector)
						{
							float thick = m_Elev[m+ncr] - m_Elev[m];
							if (thick == 0)
							{
								m_VectorArray[3*m] = 0;
								m_VectorArray[3*m+1] = 0;
							}
							else
							{
								m_VectorArray[3*m] /= (m_Delc[j] * thick);
								m_VectorArray[3*m+1] /= (m_Delr[i] * thick);
							}
							m_VectorArray[3*m+2] /= (m_Delr[i]*m_Delc[j]);
						}
					}

					m++;
				}
			}
		}
	}
	// special case for gwt, which may allow initial concentration to be
	// saved as period 0.
	if (per == 0)
	{
		m_StressPeriod = 0;
		int dummy1, dummy2, dummy3;
		MfCountFeatures(&dummy1, &dummy2, &dummy3, m_Ibound);
		MfGetFeatures(&ierror, m_ModelFeatureArray);
	}
	// All other situations
	else if (per != m_StressPeriod)
	{
		int start;
		// check if the files need to be rewound
		if (per < m_StressPeriod)
		{
			// Calling MfCountFeatures will rewind all iunits and
			// counts boundary cells again. Now the iunit files are
			// ready for reading stress period data
			int dummy1, dummy2, dummy3;
			MfCountFeatures(&dummy1, &dummy2, &dummy3, m_Ibound);
			start = 1;
		}
		else
		{
			start = m_StressPeriod+1;
		}
		for (i = start; i<=per; i++)
		{
			MfGetFeatures(&ierror, m_ModelFeatureArray);
			if (ierror) 
			{
				// decide what to do.
			}
		}
		m_StressPeriod = per;
	}
	
	if (m_Transport)
	{
		if (m_Particle_Concentrations != 0)
		{
			delete [] m_Particle_Concentrations;
			m_Particle_Concentrations = 0;
		}
		if (m_Particle_Coord != 0)
		{
			delete [] m_Particle_Coord;
			m_Particle_Coord = 0;
		}
		m_NumberOfParticles = 0;
		int numCoordinates = 0;

		MFReadParticleCount(&numCoordinates, &ierror, &timePointIndex);
		if (ierror)
		{
			//decide what to do.
		}
		else
		{
			if (numCoordinates)
			{
				float *part_concentrations;
				float *part_coord;

				part_concentrations = new float[numCoordinates];
				part_coord = new float[numCoordinates*3];

				int newNumCoordinates = numCoordinates;

				MFReadParticles(&newNumCoordinates, &ierror, part_coord, 
					part_concentrations, m_Delr, m_Delc, m_TrueElev);
				if (ierror)
				{
					delete [] part_concentrations;
					delete [] part_coord;
					return;
				}

				// numCoordinates is changed by MFReadParticles because inactive
				// particles are skipped.  Therefore, we may not be able to just 
				// use all the points in the array;
				if (newNumCoordinates == numCoordinates)
				{
					m_Particle_Concentrations = part_concentrations;
					m_Particle_Coord = part_coord;

				}
				else
				{
					m_Particle_Concentrations = new float[newNumCoordinates];
					m_Particle_Coord = new float[newNumCoordinates*3];

					for (i = 0; i<newNumCoordinates; i++)
					{
						m_Particle_Concentrations[i] = part_concentrations[i];
					};
					for (i = 0; i<newNumCoordinates*3; i++)
					{
						m_Particle_Coord[i] = part_coord[i];
					};

					delete [] part_concentrations;
					delete [] part_coord;
				}
				m_NumberOfParticles = newNumCoordinates;

			}
		}
	}
	

}

void AbstractModflowDataSource::MFReadParticleCount(int *numCoordinates,
			int *ierror, int *istep)
{
	// override in GWT and MOC3D
	*numCoordinates = 0;
	m_NumberOfParticles = 0;
}

void AbstractModflowDataSource::MFReadParticles(int *NP, int *ierror, 
	float *part_coord, float *part_concentrations, float *delr, float *delc, 
	float *elev)
{
	// override in GWT and MOC3D
	*ierror = 1;
}

 char *AbstractModflowDataSource::GetTimeUnit()
 {
 	switch(m_TimeUnit)
	{
	case 0:	
		return "";
	case 1:
		return "s";
	case 2:
		return "min";
	case 3:
		return "hr";
	case 4:
		return "day";
	case 5:
		return "yr";
	default:
		return "";
	}
}
