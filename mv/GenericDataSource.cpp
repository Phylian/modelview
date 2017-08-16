#include "GenericDataSource.h"
#include <stdio.h>
#include <string.h>
#include <fstream.h>

GenericDataSource::GenericDataSource()
{
}

GenericDataSource::~GenericDataSource()
{
	ReleaseMemory();
}

int GenericDataSource::GetPrimaryScalarMode()
{
	return MV_CELL_SCALARS;
}

char *GenericDataSource::LoadData(char *dataFileList)
{
	int i, j, k;
	ReleaseMemory();
	m_DataFileList = new char[strlen(dataFileList) + 1];
	strcpy(m_DataFileList, dataFileList);
	ifstream in("D:\\Mv\\data\\Herk\\422-426 sgsim.btr");
	m_ScalarGridDim[0] = 121;
	m_ScalarGridDim[1] = 41;
	m_ScalarGridDim[2] = 61;
	float dx, dy, dz, time;
	dx = 1;
	dy = 1;
	dz = .1f; 
	time = 0;
	int numberOfPoints = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
	int numberOfCells = (m_ScalarGridDim[0]-1)*(m_ScalarGridDim[1]-1)*(m_ScalarGridDim[2]-1);
	m_ScalarGridCoordinates = new float[numberOfPoints*3];
	m_ScalarArray = new float[numberOfPoints + numberOfCells];

	for (i=0; i<numberOfPoints + numberOfCells; i++)
	{
		m_ScalarArray[i] = 0;
	}

	float dummy;
	char line[200];
	in.getline(line,200);
	in.getline(line,200);
	in.getline(line,200);
	in.getline(line,200);
	for (i=0; i<numberOfCells; i++)
	{
		in >> dummy >> m_ScalarArray[numberOfPoints + i];
	}
	in.close();

	float x, y, z;
	int m = 0;
	for (k=0; k<m_ScalarGridDim[2]; k++)
	{
		z = k*dz;
		for (j=0; j<m_ScalarGridDim[1]; j++)
		{
			y = j*dy;
			for (i=0; i<m_ScalarGridDim[0]; i++)
			{
				x = i*dx;
				m_ScalarGridCoordinates[3*m] = x;
				m_ScalarGridCoordinates[3*m+1] = y;
				m_ScalarGridCoordinates[3*m+2] = z;
				m++;
			}
		}
	}

	m_NumberOfTimePoints = 1;
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	for (i=0; i<m_NumberOfTimePoints; i++)
	{
		m_TimePointLabels[i] = new char[10];
		sprintf(m_TimePointLabels[i], "%d", time);
	}
	m_NumberOfScalarDataTypes = 1;
	m_DataTypeLabels = new char *[1];
	m_DataTypeLabels[0] = new char[20]; 
	strcpy(m_DataTypeLabels[0], "log k");

	/*
	ifstream in("D:\\Mv\\data\\fogg\\plume.dat");
	in >> m_ScalarGridDim[0] >> m_ScalarGridDim[1] >> m_ScalarGridDim[2];
	m_ScalarGridDim[0]++;
	m_ScalarGridDim[1]++;
	m_ScalarGridDim[2]++;
	float dx, dy, dz, time;
	in >> dx >> dy >> dz;
	int nc;
	in >> time >> nc;
	int *index = new int[nc];
	for (i=0; i<nc; i++)
	{
		in >> index[i];
	}
	int numberOfPoints = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
	int numberOfCells = (m_ScalarGridDim[0]-1)*(m_ScalarGridDim[1]-1)*(m_ScalarGridDim[2]-1);
	m_ScalarGridCoordinates = new float[numberOfPoints*3];
	m_ScalarArray = new float[numberOfPoints + numberOfCells];

	for (i=0; i<numberOfPoints + numberOfCells; i++)
	{
		m_ScalarArray[i] = 0;
	}

	for (i=0; i<nc; i++)
	{
		in >> m_ScalarArray[numberOfPoints + index[i] - 1];
	}
	in.close();

	float x, y, z;
	int m = 0;
	for (k=0; k<m_ScalarGridDim[2]; k++)
	{
		z = k*dz;
		for (j=0; j<m_ScalarGridDim[1]; j++)
		{
			y = j*dy;
			for (i=0; i<m_ScalarGridDim[0]; i++)
			{
				x = i*dx;
				m_ScalarGridCoordinates[3*m] = x;
				m_ScalarGridCoordinates[3*m+1] = y;
				m_ScalarGridCoordinates[3*m+2] = z;
				m++;
			}
		}
	}

	m_NumberOfTimePoints = 1;
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	for (i=0; i<m_NumberOfTimePoints; i++)
	{
		m_TimePointLabels[i] = new char[10];
		sprintf(m_TimePointLabels[i], "%d", time);
	}
	m_NumberOfScalarDataTypes = 1;
	m_DataTypeLabels = new char *[1];
	m_DataTypeLabels[0] = new char[20]; 
	strcpy(m_DataTypeLabels[0], "concentration");
	*/


	/*
	m_ScalarGridDim[0] = 6;
	m_ScalarGridDim[1] = 4;
	m_ScalarGridDim[2] = 3;
	int numberOfPoints = m_ScalarGridDim[0]*m_ScalarGridDim[1]*m_ScalarGridDim[2];
	int numberOfCells = (m_ScalarGridDim[0]-1)*(m_ScalarGridDim[1]-1)*(m_ScalarGridDim[2]-1);
	m_ScalarGridCoordinates = new float[numberOfPoints*3];
	m_ScalarArray = new float[numberOfPoints + numberOfCells];
	int m = 0;
	for (k=0; k<m_ScalarGridDim[2]; k++)
	{
		for (j=0; j<m_ScalarGridDim[1]; j++)
		{
			for (i=0; i<m_ScalarGridDim[0]; i++)
			{
				m_ScalarGridCoordinates[3*m] = (float) i;
				m_ScalarGridCoordinates[3*m+1] = (float) j;
				m_ScalarGridCoordinates[3*m+2] = (float) k;
				m_ScalarArray[m] = i/6.0f;
				m++;
			}
		}
	}
	int koffset, joffset, p;
	for (k=0; k<2; k++)
	{
		koffset = k*24;
		for (int j=0; j<3; j++)
		{
			joffset = koffset + j*6;
			for (int i=0; i<5; i++)
			{
				if (i+j+k < 5)
				{
					p = joffset + i;
					m_ScalarArray[m] = (m_ScalarArray[p] + m_ScalarArray[p+1]
									 +  m_ScalarArray[p+6] + m_ScalarArray[p+7]
									 +  m_ScalarArray[p+24] + m_ScalarArray[p+25]
									 +  m_ScalarArray[p+30] + m_ScalarArray[p+31])/8;
				}
				else
				{
					m_ScalarArray[m] = 1e30f;
				}
				m++;
			}
		}
	}
	
	m_NumberOfTimePoints = 6;
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	for (i=0; i<m_NumberOfTimePoints; i++)
	{
		m_TimePointLabels[i] = new char[10];
		sprintf(m_TimePointLabels[i], "%d", i*10);
	}
	m_NumberOfScalarDataTypes = 1;
	m_DataTypeLabels = new char *[1];
	m_DataTypeLabels[0] = new char[20]; 
	strcpy(m_DataTypeLabels[0], "generic");

	m_VectorGridDim[0] = 5;
	m_VectorGridDim[1] = 3;
	m_VectorGridDim[2] = 2;
	int np = m_VectorGridDim[0]*m_VectorGridDim[1]*m_VectorGridDim[2];

	m_VectorGridCoordinates = new float[np*3];
	m_VectorArray = new float[np*3];

	m=0;
	for (k=0; k<m_VectorGridDim[2]; k++)
	{
		for (j=0; j<m_VectorGridDim[1]; j++)
		{
			for (i=0; i<m_VectorGridDim[0]; i++)
			{
				m_VectorGridCoordinates[3*m] = 0.5f + i;
				m_VectorGridCoordinates[3*m+1] = 0.5f + j;
				m_VectorGridCoordinates[3*m+2] = 0.5f + k;
				if (m_ScalarArray[m + 72] > 9e29f)
				{
					m_VectorArray[3*m] = 1e30f;
				}
				else
				{
					m_VectorArray[3*m] = 0.5e-6f;
				}
				m_VectorArray[3*m+1] = 0.5e-6f;
				m_VectorArray[3*m+2] = 0.5e-6f;
				m++;
			}
		}
	}

	m_NumberOfPathlines = 2;
	m_NumberOfPathlineCoordinates = 7;
	m_PathlineCoordinates = new float[m_NumberOfPathlineCoordinates*3];
	m_PathlinePointArray = new int[9];
	i=0;
	m_PathlinePointArray[i] = 4;  i++;
	for (k=0; k<4; k++)
	{
		m_PathlineCoordinates[3*k] = 0.6f * k + 0.8f;
		m_PathlineCoordinates[3*k+1] = 0.1f * k + 0.4f;
		m_PathlineCoordinates[3*k+2] = 0.2f * k + 0.2f;
		m_PathlinePointArray[i] = k; i++;
	}
	m_PathlinePointArray[i] = 3; i++;
	for (k=4; k<7; k++)
	{
		m_PathlineCoordinates[3*k] = 0.2f * k + 0.2f;
		m_PathlineCoordinates[3*k+1] = 0.6f * k + 0.4f;
		m_PathlineCoordinates[3*k+2] = 0.1f * k + 0.8f;
		m_PathlinePointArray[i] = k; i++;
	}
	*/

	return 0;
}

void GenericDataSource::AdvanceOneTimePoint()
{
	int m = 0;
	for (int k=0; k<3; k++)
	{
		for (int j=0; j<4; j++)
		{
			for (int i=0; i<6; i++)
			{
				m_ScalarArray[m] += .1f;
				m++;
			}
		}
	}
	int koffset, joffset, p;
	for (k=0; k<2; k++)
	{
		koffset = k*24;
		for (int j=0; j<3; j++)
		{
			joffset = koffset + j*6;
			for (int i=0; i<5; i++)
			{
				if (i+j+k < 5)
				{
					p = joffset + i;
					m_ScalarArray[m] = (m_ScalarArray[p] + m_ScalarArray[p+1]
									 +  m_ScalarArray[p+6] + m_ScalarArray[p+7]
									 +  m_ScalarArray[p+24] + m_ScalarArray[p+25]
									 +  m_ScalarArray[p+30] + m_ScalarArray[p+31])/8;
				}
				else
				{
					m_ScalarArray[m] = 1e30f;
				}
				m++;
			}
		}
	}
}

void GenericDataSource::SetTimePointTo(int timePointIndex)
{
	int m = 0;
	for (int k=0; k<3; k++)
	{
		for (int j=0; j<4; j++)
		{
			for (int i=0; i<6; i++)
			{
				m_ScalarArray[m] = i/6.0f + timePointIndex * 0.1f;
				m++;
			}
		}
	}
	int koffset, joffset, p;
	for (k=0; k<2; k++)
	{
		koffset = k*24;
		for (int j=0; j<3; j++)
		{
			joffset = koffset + j*6;
			for (int i=0; i<5; i++)
			{
				if (i+j+k < 5)
				{
					p = joffset + i;
					m_ScalarArray[m] = (m_ScalarArray[p] + m_ScalarArray[p+1]
									 +  m_ScalarArray[p+6] + m_ScalarArray[p+7]
									 +  m_ScalarArray[p+24] + m_ScalarArray[p+25]
									 +  m_ScalarArray[p+30] + m_ScalarArray[p+31])/8;
				}
				else
				{
					m_ScalarArray[m] = 1e30f;
				}
				m++;
			}
		}
	}
}
