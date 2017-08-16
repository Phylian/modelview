#include "Mt3dmsDataSource.h"
#include "Mt3dmsReader.h"
#include "Mt3dmsReader1.h"
#include "Mt3dmsReader2.h"
#include "ModpathReader.h"
#include "mvUtil.h"
#include <stdio.h>
#include <string.h>
#include <fstream.h>
#include <stdlib.h>

Mt3dmsDataSource::Mt3dmsDataSource() : mvDataSource()
{
	m_Delr = 0;
	m_Delc = 0;
	m_Elev = 0;
	m_DataType = 0;
	m_NumberOfScalarDataTypes = 1;
	m_ErrorMsg[0] = 0;
	m_ErrorMsg[1] = "Error encountered while loading data.";  // Generic unspecified error
	m_ErrorMsg[2] = "Unable to open the \"cnf\" file.";
	m_ErrorMsg[3] = "Unable to open the concentration (ucn) file.";
	m_ErrorMsg[4] = "Unable to open the flow transport link (flt) file.";
	m_ErrorMsg[5] = "Error encountered while reading the \"cnf\" file.";
	m_ModelFeatureLabels = new char[360];
	char *p = m_ModelFeatureLabels;
	strncpy(p, "constant head                           ", 40);
	p += 40;
	strncpy(p, "wells                                   ", 40);
	p += 40;
	strncpy(p, "drains                                  ", 40);
	p += 40;
	strncpy(p, "rivers                                  ", 40);
	p += 40;
	strncpy(p, "general-head                            ", 40);
	p += 40;
	strncpy(p, "streams                                 ", 40);
	p += 40;
	strncpy(p, "reservoirs                              ", 40);
	p += 40;
	strncpy(p, "specified flow                          ", 40);
	p += 40;
	strncpy(p, "multinode wells                         ", 40);
}

Mt3dmsDataSource::~Mt3dmsDataSource()
{
	switch(m_DataType)
	{
	default:
		mtcleanup0_();
		return;
	case 2:
		mtcleanup1();
		return;
	case 3:
		mtcleanup2_();
		return;
	}
	ReleaseMemory();
}

void Mt3dmsDataSource::ReleaseMemory()
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
}

void Mt3dmsDataSource::GetDefaultModelFeatureColor(int i, float *rgba)
{
	float fcolor[9][4] = {
		{.67, .67, .67, 1},		// constant heads  --  light gray
		{1.0, 0.5, 1.0, 1},		// wells  --  light purple
		{0.5, 1.0, 0.5, 1},		// drains  --  light green
		{0.4, 0.7, 0.7, 1},		// rivers  --  blue gray
		{1.0, 0.5, .25, 1},		// general head -- orange
		{0.0, 0.5, 1.0, 1},		// streams --  royal blue
		{1.0, 1.0, 0.0, 1},		// reservoirs -- yellow
		{.82, .64, .64, 1},	    // transient specified flow  --  light brown
		{1.0, 0.0, 1.0, 1},		// multi-node wells  -- purple 
	};

	rgba[0] = fcolor[i][0];
	rgba[1] = fcolor[i][1];
	rgba[2] = fcolor[i][2];
	rgba[3] = fcolor[i][3];
}

char *Mt3dmsDataSource::LoadData(char *dataFileList)
{
	int i, j, k, len, ierror, ncol, nrow, nlay;
	int nx, ny, nz, nxy, nxyz, ncr, ncrl;

	// Save the dataFileList. This is necessary because the mvDoc checks
	// this variable to determine if data has been loaded.
	m_DataFileList = new char[strlen(dataFileList) + 1];
	strcpy(m_DataFileList, dataFileList);

	// Parse the data file list;
	char cnfFile[256];
	char ucnFile[256];
	char ftlFile[256];
	char pathlineFile[256];
	char code[4];
	char *pList = dataFileList;
	ParseDataFileList(pList, cnfFile);
	ParseDataFileList(pList, ucnFile);
	ParseDataFileList(pList, ftlFile);
	ParseDataFileList(pList, pathlineFile);
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

	// If pathline file is used, test open it
	if (strlen(pathlineFile) > 0)
	{
		ifstream in(pathlineFile, ios::in|ios::nocreate);
		if (!in.is_open())
		{
			return "Unable to open the pathline file";
		}
	    in.close();
	}


	// Read the grid dimensions
	if (strlen(ftlFile) == 0)
	{
		strcpy(ftlFile, " ");
	}
	MtDims(&ierror, cnfFile, ucnFile, ftlFile, &ncol, &nrow, &nlay);
	if (ierror) 
	{
		return m_ErrorMsg[ierror];
	}
	if (ftlFile[0] == ' ')
	{
		ftlFile[0] = '\0';
	}

	nx = ncol + 1;
	ny = nrow + 1;
	nz = nlay + 1;
	nxy = nx*ny;
	nxyz = nx*ny*nz;
	ncr = ncol*nrow;
	ncrl = ncol*nrow*nlay;

	m_ScalarGridDim[0] = nx;
	m_ScalarGridDim[1] = ny;
	m_ScalarGridDim[2] = nz;

	// Allocate memory
	m_Delr = new float[ncol];
	m_Delc = new float[nrow];
	m_Elev = new float[ncol*nrow*(nlay+1)];

	// Read grid data from cnf file
	MtGrid(&ierror, m_Delr, m_Delc, m_Elev, &m_Mt3dmsInactiveCellValue);
	if (ierror != 0) 
	{
		return m_ErrorMsg[ierror];
	}

	// read concentration file to determine the number of time points
	char dataType[17];
	len = 17;
	MtCountScalars(&ierror, &m_NumberOfTimePoints, dataType, len);
	if (ierror > 0) 
	{
		return "Error encountered while reading the concentration (ucn) file.";;
	}
	dataType[16]='\0';
	mvUtil::TrimLeft(dataType);
	mvUtil::ToLowerCase(dataType);

	// Store the data types.
	m_DataTypeLabels = new char *[1];
	m_DataTypeLabels[0] = new char[20]; 
	strcpy(m_DataTypeLabels[0], dataType);

	// Get the time points and store them as strings.
	float *timePoints = new float [m_NumberOfTimePoints];
	int *periods = new int [m_NumberOfTimePoints];
	int *steps = new int [m_NumberOfTimePoints];
	int *moves = new int [m_NumberOfTimePoints];
	MtGetTimePoints(timePoints, periods, steps, moves, m_NumberOfTimePoints);
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	char p[10];
	for (i=0; i<m_NumberOfTimePoints; i++)
	{
		m_TimePointLabels[i] = new char[30];
		sprintf(m_TimePointLabels[i], "%.4g (", timePoints[i]);
		mvUtil::TrimRight(m_TimePointLabels[i]);
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
	delete [] timePoints;
	delete [] periods;
	delete [] steps;
	delete [] moves;

	m_ScalarArray = new float[nxyz + ncrl];

	// Read the cell values for first concentration data set 
	// This is needed to determine inactive cells
	float *cellValues = m_ScalarArray + nxyz;
	int timePointIndex = 0;
	MtGetScalars(&ierror, cellValues, &timePointIndex);
	if (m_Mt3dmsInactiveCellValue != m_InactiveCellValue)
	{
		for (int i=0; i<ncrl; i++)
		{
			if (cellValues[i] == m_Mt3dmsInactiveCellValue)
			{
				cellValues[i] = m_InactiveCellValue;
			}
		}
	}

	// Compute point coordinates
	float znull = -1.0e20f;

	for (k=0; k<ncr; k++)
	{
		if (cellValues[k] == m_InactiveCellValue)
		{
			m_Elev[k] = znull;
		}
	}
	for (k=ncr; k<ncrl; k++)
	{
		if (cellValues[k] == m_InactiveCellValue && cellValues[k-ncr] == m_InactiveCellValue)
		{
			m_Elev[k] = znull;
		}
	}
	for (k=ncrl-ncr; k<ncrl; k++)
	{
		if (cellValues[k] == m_InactiveCellValue)
		{
			m_Elev[k+ncr] = znull;
		}
	}

	float *x = new float[nx];
	float *y = new float[ny];
	float *z = new float[nxyz];
	float xorigin = 0;
	float yorigin = 0;
	x[0] = xorigin;
	for (i=0; i<ncol; i++)
	{
		x[i+1] = x[i] + m_Delr[i];
	}
	y[0] = yorigin;
	for (i=0; i<nrow; i++)
	{
		y[i+1] = y[i] + m_Delc[i];
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
					// propergate the change downward
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

	// count vectors and model features, allocate memory
	if (strlen(ftlFile) > 0)
	{
		int modelFeatureArraySize;
		MtCountVectorsAndFeatures(&ierror, &modelFeatureArraySize, &m_NumberOfModelFeatureTypes);
		if (ierror)
		{
			delete [] x;
			delete [] y;
			delete [] z;
			return "Error encountered while reading the flow-transport link (ftl) file.";
		}
		m_VectorGridDim[0] = m_ScalarGridDim[0] - 1;
		m_VectorGridDim[1] = m_ScalarGridDim[1] - 1;
		m_VectorGridDim[2] = m_ScalarGridDim[2] - 1;
		m_VectorArray = new float[3*ncrl];
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
		m_ModelFeatureArray = new int[modelFeatureArraySize];
		for (i=0; i<modelFeatureArraySize; i++)
		{
			m_ModelFeatureArray[i] = 0;
		}
	}

	delete [] x;
	delete [] y;
	delete [] z;

	// read pathline data
	if (strlen(pathlineFile) > 0)
	{
		ModpathReader::ReadData(pathlineFile, m_NumberOfPathlines,
			m_NumberOfPathlineCoordinates, m_PathlineCoordinates, 
			m_PathlineScalarArray,  m_MaxTimePathlineScalarArray, 
			m_MinTimePathlineScalarArray, m_PathlinePointArray, 
			m_PathsBackwardsInTime, m_MinPositiveTime);
	}

	return 0;
}

void Mt3dmsDataSource::AdvanceOneTimePoint()
{
	SetTimePointTo(-1);
}

void Mt3dmsDataSource::SetTimePointTo(int timePointIndex)
{
	int ierror, ncol, nrow, nlay, i, j, k, ncr;
	ncol = m_ScalarGridDim[0]-1;
	nrow = m_ScalarGridDim[1]-1;
	nlay = m_ScalarGridDim[2]-1;
	ncr = ncol*nrow;
	int nxyz = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
	int ncrl = ncol*nrow*nlay;
	float *cellValues = m_ScalarArray + nxyz;

	MtGetScalars(&ierror, cellValues, &timePointIndex);
	if (m_Mt3dmsInactiveCellValue != m_InactiveCellValue)
	{
		for (int i=0; i<ncrl; i++)
		{
			if (cellValues[i] == m_Mt3dmsInactiveCellValue)
			{
				cellValues[i] = m_InactiveCellValue;
			}
		}
	}
	float znull = -1.0e20f;
	mvUtil::interp3d(cellValues, m_ScalarArray, m_Delr, m_Delc, m_Elev,
						ncol, nrow, nlay, m_InactiveCellValue, znull);

	if (m_VectorArray != 0)
	{
		int update;
		MtGetVectorsAndFeatures(&update, m_VectorArray, &timePointIndex, m_ModelFeatureArray);

		if (update)
		{
			int m = 0;
			for (k=0; k<nlay; k++)
			{
				for (j=0; j<nrow; j++)
				{
					for (i=0; i<ncol; i++)
					{
						if (cellValues[m] == m_InactiveCellValue)
						{
							m_VectorArray[3*m] = m_InactiveCellValue;
						}
						else
						{
							float thick = m_Elev[m+ncr] - m_Elev[m];
							m_VectorArray[3*m] /= (m_Delc[j] * thick);
							m_VectorArray[3*m+1] /= (m_Delr[i] * thick);
							m_VectorArray[3*m+2] /= (m_Delr[i]*m_Delc[j]);
						}
						m++;
					}
				}
			}
		}
	}
}

void Mt3dmsDataSource::MtDims(int *ierror, char *cnfFile, char *ucnFile, char *ftlFile, int *nc, int *nr, int *nl)
{
	int unstruct = 0;
	int len1 = strlen(cnfFile);
	int len2 = strlen(ucnFile);
	int len3 = strlen(ftlFile);
	switch(m_DataType)
	{
	case 0:	
		unstruct = 1;
		//fall through
	case 1:
		mtdims0_(ierror, cnfFile, ucnFile, ftlFile, &unstruct, nc, nr, nl, len1, len2, len3);
		return;
	case 2:
		mtdims1(ierror, cnfFile, ucnFile, ftlFile, &unstruct, nc, nr, nl, len1, len2, len3);
		return;
	case 3:
		mtdims2_(ierror, cnfFile, ucnFile, ftlFile, &unstruct, nc, nr, nl, len1, len2, len3);
		return;
	}
}

void Mt3dmsDataSource::MtGrid(int *ierror, float *delr, float *delc, float *m_Elev, float *cinact)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mtgrid0_(ierror, delr, delc, m_Elev, cinact);
		return;
	case 2:
		mtgrid1(ierror, delr, delc, m_Elev, cinact);
		return;
	case 3:
		mtgrid2_(ierror, delr, delc, m_Elev, cinact);
		return;
	}
}


void Mt3dmsDataSource::MtCountScalars(int *ierror, int *numTimePoints, char *dataType, int len)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mtcountscalars0_(ierror, numTimePoints, dataType, len);
		return;
	case 2:
		mtcountscalars1(ierror, numTimePoints, dataType, len);
		return;
	case 3:
		mtcountscalars2_(ierror, numTimePoints, dataType, len);
		return;
	}
}

void Mt3dmsDataSource::MtCountVectorsAndFeatures(int *ierror, int *modelFeatureArraySize, 
											int *numModelFeatureTypes)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mtcountvectorsandfeatures0_(ierror, modelFeatureArraySize, numModelFeatureTypes);
		return;
	case 2:
		mtcountvectorsandfeatures1(ierror, modelFeatureArraySize, numModelFeatureTypes);
		return;
	case 3:
		mtcountvectorsandfeatures2_(ierror, modelFeatureArraySize, numModelFeatureTypes);
		return;
	}
}

void Mt3dmsDataSource::MtGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mtgettimepoints0_(timePoints, periods, steps, moves, &numTimePoints);
		return;
	case 2:
		mtgettimepoints1(timePoints, periods, steps, moves, &numTimePoints);
		return;
	case 3:
		mtgettimepoints2_(timePoints, periods, steps, moves, &numTimePoints);
		return;
	}
}

void Mt3dmsDataSource::MtGetScalars(int *ierror, float *cellValues, int *timePointIndex)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mtgetscalars0_(ierror, cellValues, timePointIndex);
		return;
	case 2:
		mtgetscalars1(ierror, cellValues, timePointIndex);
		return;
	case 3:
		mtgetscalars2_(ierror, cellValues, timePointIndex);
		return;
	}
}

void Mt3dmsDataSource::MtGetVectorsAndFeatures(int *update, float *flowArray, int *timePointIndex, int *ibnode)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mtgetvectorsandfeatures0_(update, flowArray, timePointIndex, ibnode);
		return;
	case 2:
		mtgetvectorsandfeatures1(update, flowArray, timePointIndex, ibnode);
		return;
	case 3:
		mtgetvectorsandfeatures2_(update, flowArray, timePointIndex, ibnode);
		return;
	}
}
