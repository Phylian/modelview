#include "mvDataSource.h"
#include <string.h>

mvDataSource::mvDataSource()
{
	m_DataFileList = 0;
	m_NumberOfTimePoints = 0;
	m_TimePointLabels = 0;
	m_NumberOfScalarDataTypes = 0;
	m_DataTypeLabels = 0;
	m_ScalarGridDim[0] = 0;
	m_ScalarGridDim[1] = 0;
	m_ScalarGridDim[2] = 0;
	m_ScalarGridCoordinates = 0;
	m_ScalarArray = 0;
	m_ScalarArrayOffset = 0;
	// The following value of 1.0e30f is repeated in the 
	// dll's for reading subsidence in MODFLOW
	m_InactiveCellValue = 1.0e30f;
	m_VectorGridDim[0] = 0;
	m_VectorGridDim[1] = 0;
	m_VectorGridDim[2] = 0;
	m_VectorGridCoordinates = 0;
	m_VectorArray = 0;
	m_NumberOfPathlines = 0;
	m_NumberOfPathlineCoordinates = 0;
	m_PathlineCoordinates = 0;
	m_PathlinePointArray = 0;
	m_PathlineScalarArray = 0;
	m_MaxTimePathlineScalarArray = 0;
	m_MinTimePathlineScalarArray = 0;
	m_NumberOfModelFeatureTypes = 0;
	m_ModelFeatureArray = 0;
	m_ModelFeatureLabels = 0;
	m_Particle_Coord = 0;
	m_Particle_Concentrations = 0;
	m_NumberOfParticles = 0;
	m_PathsBackwardsInTime = 0;
	m_PathLineScalarMode = MP_TRAVEL_TIME;
}

mvDataSource::~mvDataSource()
{
	ReleaseMemory();
}

void mvDataSource::ReleaseMemory()
{
	if (m_DataFileList != 0)
	{
		delete [] m_DataFileList;
		m_DataFileList = 0;
	}
	if (m_NumberOfTimePoints > 0 && m_TimePointLabels != 0)
	{
		for (int i=0; i<m_NumberOfTimePoints; i++)
		{
			delete [] m_TimePointLabels[i];
		}
		delete m_TimePointLabels;
		m_TimePointLabels = 0;
	}
	if (m_NumberOfScalarDataTypes > 0 && m_DataTypeLabels != 0)
	{
		for (int i=0; i<m_NumberOfScalarDataTypes; i++)
		{
			delete [] m_DataTypeLabels[i];
		}
		delete m_DataTypeLabels;
		m_DataTypeLabels = 0;
	}
	if (m_ScalarGridCoordinates != 0)
	{
		delete [] m_ScalarGridCoordinates;
		m_ScalarGridCoordinates = 0;
	}
	if (m_ScalarArray != 0)
	{
		delete [] m_ScalarArray;
		m_ScalarArray = 0;
	}
	if (m_VectorGridCoordinates != 0)
	{
		delete [] m_VectorGridCoordinates;
		m_VectorGridCoordinates = 0;
	}
	if (m_VectorArray != 0)
	{
		delete [] m_VectorArray;
		m_VectorArray = 0;
	}
	if (m_PathlineCoordinates != 0)
	{
		delete [] m_PathlineCoordinates;
		m_PathlineCoordinates = 0;
	}
	if (m_PathlinePointArray != 0)
	{
		delete [] m_PathlinePointArray;
		m_PathlinePointArray = 0;
	}
	if (m_PathlineScalarArray != 0)
	{
		delete [] m_PathlineScalarArray;
		m_PathlineScalarArray = 0;
	}
	if (m_MaxTimePathlineScalarArray != 0)
	{
		delete [] m_MaxTimePathlineScalarArray;
		m_MaxTimePathlineScalarArray = 0;
	}
	if (m_MinTimePathlineScalarArray != 0)
	{
		delete [] m_MinTimePathlineScalarArray;
		m_MinTimePathlineScalarArray = 0;
	}
	if (m_ModelFeatureArray != 0) 
	{
		delete [] m_ModelFeatureArray;
		m_ModelFeatureArray = 0;
	}
	if (m_ModelFeatureLabels)
	{
		delete [] m_ModelFeatureLabels;
		m_ModelFeatureLabels = 0;
	}

	if (m_Particle_Coord != 0) 
	{
		delete [] m_Particle_Coord;
		m_Particle_Coord = 0;
	}
	if (m_Particle_Concentrations != 0) 
	{
		delete [] m_Particle_Concentrations;
		m_Particle_Concentrations = 0;
	}
}

bool mvDataSource::ParseDataFileList(char *&pList, char *fileName)
{
	char *end = strchr(pList, '\n');
	if (end == NULL)
	{
		return 0;
	}
	else
	{
		int len = end - pList;
		strncpy(fileName, pList, len);
		*(fileName + len) = '\0';
		pList += len + 1;
		return 1;
	}
}

int mvDataSource::GetDataSetToUseForRange()
{
	if (GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		return MV_USE_CELL_DATA_FOR_RANGE;
	}
	else
	{
		return MV_USE_POINT_DATA_FOR_RANGE;
	}
}

int mvDataSource::GetNumPoints()
{
	const int *sdim = GetScalarGridDimensions();
	return sdim[0]*sdim[1]*sdim[2];
}

int mvDataSource::GetNumCells()
{
	const int *sdim = GetScalarGridDimensions();
	int numCells = (sdim[0]-1)*(sdim[1]-1);
	if (sdim[2] > 1)
	{
		numCells *= (sdim[2]-1);
	}
	return numCells;
}

float *mvDataSource::GetPathlineScalarArray()
{
	switch (m_PathLineScalarMode)
	{
	case MP_TRAVEL_TIME:
		return m_PathlineScalarArray;
	case MP_MIN_TRAVEL_TIME:
		return m_MinTimePathlineScalarArray;
	case MP_MAX_TRAVEL_TIME:
		return m_MaxTimePathlineScalarArray;
	default:
		return m_PathlineScalarArray;
	}
}
