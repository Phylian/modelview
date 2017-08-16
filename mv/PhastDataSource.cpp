#include "PhastDataSource.h"
#include <assert.h>
#include <stdio.h>

static char s_ERR_MSG[400];

#if ((H5_VERS_MAJOR > 1) || (H5_VERS_MAJOR == 1 && H5_VERS_MINOR > 6) || (H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 6 && H5_VERS_RELEASE >= 3))
#define hssize_t hsize_t
#endif

#define ARRAY_SIZE(A) (sizeof(A)/sizeof(A[0])) 

/*
  Added for code coop comment
*/

PhastDataSource::PhastDataSource() : mvDataSource()
{
	H5open();

	m_hidFile         = -1;
	m_nTimePointIndex = -1;
	m_nDataTypeIndex  = -1;
	m_nActiveCount    = -1;
}

PhastDataSource::~PhastDataSource()
{
	// dont't let mvDataSource attempt to delete
	/// m_NumberOfTimePoints = 0;
	/// m_TimePointLabels = 0;

	mvDataSource::ReleaseMemory();

	// Close the HDF5 file
	if (m_hidFile > 0) H5Fclose(m_hidFile);

	// Close the HDF5 library
	H5close();
}

char* PhastDataSource::LoadData(char *dataFileList)
{
	if (OpenHDF(dataFileList))
	{
		return s_ERR_MSG;
	}
	if (LoadGrid())
	{
		return s_ERR_MSG;
	}
	if (LoadDataTypeLabels())
	{
		return s_ERR_MSG;
	}
	if (LoadTimeStepLabels())
	{
		return s_ERR_MSG;
	}
	if (LoadFeatures())
	{
		return s_ERR_MSG;
	}
	if (SetScalarArray(0, 0))
	{
		return s_ERR_MSG;
	}

	// Save the dataFileList. This is necessary because the mvDoc checks
	// this variable to determine if data has been loaded.
	assert(m_DataFileList == 0);
	m_DataFileList = new char[strlen(dataFileList) + 1];
	if (m_DataFileList == 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
		return s_ERR_MSG;
	}
	strcpy(m_DataFileList, dataFileList);
	return 0;
}

int PhastDataSource::OpenHDF(char *dataFileList)
{
	char szTitle[_MAX_PATH];
	char szPath[_MAX_PATH];

	char *pszList = dataFileList;
	ParseDataFileList(pszList, szTitle);
	ParseDataFileList(pszList, szPath);

	char szCurrentDirectory[_MAX_PATH];
	::GetCurrentDirectory(_MAX_PATH, szCurrentDirectory);
	::OutputDebugString("pwd = ");
	::OutputDebugString(szCurrentDirectory);
	::OutputDebugString("\n");

	::_snprintf(m_szHDF, ARRAY_SIZE(m_szHDF), "%s%s", szPath, szTitle);

	m_hidFile = H5Fopen(m_szHDF, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (!(m_hidFile > 0))
	{
		// try current directory
		::_snprintf(m_szHDF, ARRAY_SIZE(m_szHDF), "%s\\%s", szCurrentDirectory, szTitle);

		m_hidFile = H5Fopen(m_szHDF, H5F_ACC_RDONLY, H5P_DEFAULT);
		if (!(m_hidFile > 0))
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s", m_szHDF);
			return 1;
		}
	}
	return 0;
}

void PhastDataSource::AdvanceOneTimePoint()
{
	assert(m_nTimePointIndex + 1 >= 0 && m_nTimePointIndex + 1 < m_NumberOfTimePoints);
	if (m_nTimePointIndex + 1 >= 0 && m_nTimePointIndex + 1 < m_NumberOfTimePoints)
	{
		SetTimePointTo(m_nTimePointIndex + 1);
	}
}

void PhastDataSource::SetTimePointTo(int timePointIndex)
{
	if (SetScalarArray(timePointIndex, m_nDataTypeIndex))
	{
		// report error
		assert(0);
		::MessageBox(NULL, s_ERR_MSG, "Phast HDF Error", MB_ICONERROR);
	}
}

void PhastDataSource::SetScalarDataTypeTo(int dataTypeIndex)
{
	if (SetScalarArray(m_nTimePointIndex, dataTypeIndex))
	{
		// report error
		assert(0);
		::MessageBox(NULL, s_ERR_MSG, "Phast HDF Error", MB_ICONERROR);
	}
}

int PhastDataSource::SetScalarArray(int timePointIndex, int dataTypeIndex)
{
	static const char szName[] = "Scalars";
	herr_t status;
	hsize_t dims[1], maxdims[1];

	if (m_nTimePointIndex == timePointIndex && m_nDataTypeIndex == dataTypeIndex) return 0;

	// open timestep group
	assert(timePointIndex >= 0 && timePointIndex < m_NumberOfTimePoints);
	hid_t group_id = H5Gopen(m_hidFile, m_TimePointLabels[timePointIndex]);

	if (group_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s group", m_TimePointLabels[timePointIndex]);
		assert(0);
		return 1;
	}

	// load velocities
	bool bReadVectors = true;
	if (SetVectorArray(group_id, timePointIndex))
	{
		bReadVectors = false;
		// s_ERR_MSG set in call
		// return 1; for now ignore vector errors
	}

	hid_t dset_id = H5Dopen(group_id, szName);
	if (dset_id < 0)
	{
		assert(bReadVectors);
		status = H5Gclose(group_id);
		assert(status >= 0);

		int nxyz = m_ScalarGridDim[0] * m_ScalarGridDim[1] * m_ScalarGridDim[2];
		for (int i = 0; i < nxyz; ++i)
		{
			m_ScalarArray[i] = m_InactiveCellValue;
		}
		m_nDataTypeIndex = dataTypeIndex;
		m_nTimePointIndex = timePointIndex;

		if (!bReadVectors) {
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open /%s/%s dataset", m_TimePointLabels[timePointIndex], szName);
			return 1;
		}
		return 0;
	}

	hid_t dspace_id = H5Dget_space(dset_id);
	if (dspace_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_space for /%s/%s dataspace", m_TimePointLabels[timePointIndex], szName);
		assert(0);
		return 1;
	}

	if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on /%s/%s dataspace", m_TimePointLabels[timePointIndex], szName);
		assert(0);
		return 1;
	}

	int* indices = new int[dims[0]];
	if (indices == 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
		return 1;
	}

	if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, indices) < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read /%s/%s dataspace", m_TimePointLabels[timePointIndex], szName);
		assert(0);
		return 1;
	}
	if (H5Sclose(dspace_id) < 0) assert(false);
	if (H5Dclose(dset_id) < 0) assert(false);

	bool bFound = false;
	for (int i = 0; i < dims[0]; ++i)
	{
		if (indices[i] == dataTypeIndex)
		{
			bFound = true;
			break;
		}
	}

	if (bFound)
	{
		static const char szActiveArray[] = "ActiveArray";
		hsize_t active_dims[1], active_maxdims[1];
		hssize_t start[1];
		hsize_t  count[1];

		hid_t dset_id = H5Dopen(group_id, szActiveArray);
		if (dset_id < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open /%s/%s dataset", m_TimePointLabels[timePointIndex], szActiveArray);
			assert(0);
			return 1;
		}

		hid_t file_dspace_id = H5Dget_space(dset_id);
		if (file_dspace_id < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_space for /%s/%s dataspace", m_TimePointLabels[timePointIndex], szActiveArray);
			assert(0);
			return 1;
		}

		if (H5Sget_simple_extent_dims(file_dspace_id, active_dims, active_maxdims) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on /%s/%s dataspace", m_TimePointLabels[timePointIndex], szActiveArray);
			assert(0);
			return 1;
		}
		assert(active_dims[0] == dims[0] * m_nActiveCount);

		// make the file dataspace selection
		start[0] = i * m_nActiveCount;
		count[0] = m_nActiveCount;

		status = H5Sselect_hyperslab(file_dspace_id, H5S_SELECT_SET, start, NULL, count, NULL);
		assert(status >= 0);

		status = H5Dread(dset_id, H5T_NATIVE_FLOAT, m_hidMemSpace, file_dspace_id, H5P_DEFAULT, m_ScalarArray);
		assert(status >= 0);

		if (H5Sclose(file_dspace_id) < 0) assert(false);
		if (H5Dclose(dset_id) < 0) assert(false);

		m_nDataTypeIndex = dataTypeIndex;
		m_nTimePointIndex = timePointIndex;
	}
	else
	{
		int nxyz = m_ScalarGridDim[0] * m_ScalarGridDim[1] * m_ScalarGridDim[2];
		for (int i = 0; i < nxyz; ++i)
		{
			m_ScalarArray[i] = m_InactiveCellValue;
		}
		m_nDataTypeIndex = dataTypeIndex;
		m_nTimePointIndex = timePointIndex;
	}


	delete[] indices;

	status = H5Gclose(group_id);
	assert(status >= 0);

	char buffer[180];
	::_snprintf(buffer, ARRAY_SIZE(buffer), "PhastDataSource::SetScalarArray(timePointIndex = %d, dataTypeIndex = %d)\n", timePointIndex, dataTypeIndex);
	::OutputDebugString(buffer);

	return 0;
}

int PhastDataSource::SetVectorArray(hid_t timestep_group_id, int timePointIndex)
{
	static const char* szNames[] = {"Vx_node", "Vy_node", "Vz_node", "Vmask"};

	herr_t status;
	hsize_t dims[1];

	if (m_VectorArray != 0)
	{
		hssize_t start[1];
		hsize_t  stride[1];
		hsize_t  count[1];

		int nxyz = m_ScalarGridDim[0] * m_ScalarGridDim[1] * m_ScalarGridDim[2];
		int nxyz3 = nxyz * 3;

		// create the memory dataspace
		dims[0] = nxyz3;
		hid_t memspace = H5Screate_simple(1, dims, NULL);
		assert(memspace > 0);

		for (hssize_t i = 0; i < 3; ++i)
		{
			hid_t dset_id = H5Dopen(timestep_group_id, szNames[i]);
			if (dset_id < 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open /%s/%s dataset", m_TimePointLabels[timePointIndex], szNames[i]);
				for (int m = 0; m < nxyz; ++m)
				{
					m_VectorArray[m*3]   = 0;
					m_VectorArray[m*3+1] = 0;
					m_VectorArray[m*3+2] = 0;
				}
				return i;
			}

			// set memory hyperslab selection
			start[0]  = i;
			stride[0] = 3;
			count[0]  = nxyz;
;
			status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, stride, count, NULL);
			assert(status >= 0);

			status = H5Dread(dset_id, H5T_NATIVE_FLOAT, memspace, H5S_ALL, H5P_DEFAULT, m_VectorArray);
			assert(status >= 0);

			if (H5Dclose(dset_id) < 0) assert(false);
		}

		// read mask
		hid_t dset_id = H5Dopen(timestep_group_id, szNames[3]);
		if (dset_id < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open /%s/%s dataset", m_TimePointLabels[timePointIndex], szNames[3]);
			return 1;
		}

		int* vmask = new int[nxyz];
		if (vmask == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}
		status = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, vmask);
		assert(status >= 0);

		if (H5Dclose(dset_id) < 0) assert(false);


		// apply mask
		for (int m = 0; m < nxyz; ++m)
		{
			if (vmask[m] == 0 /*** || timePointIndex == 0 ***/ /*TODO change when first timestep is fixed*/)
			{
				m_VectorArray[m*3]   = m_InactiveCellValue;
				m_VectorArray[m*3+1] = m_InactiveCellValue;
				m_VectorArray[m*3+2] = m_InactiveCellValue;
			}
		}

		delete[] vmask;
	}
	return 0;
}


void PhastDataSource::GetDefaultModelFeatureColor(int i, float *rgba)
{
	static float s_Rgba[6][4] = {
		{1.0, 0.5, .25, 1},		// Flux -- orange
		{1.0, .16, .16, 1},	 	// Leaky  --  red
		{0.0, 0.5, 1.0, 1},		// River --  royal blue
		{.82, .64, .64, 1},	    // Specified  --  light brown
		{1.0, 0.5, 1.0, 1},		// Wells  --  light purple
		{0.0, 1.0, 0.0, 1},		// Drains  --  green
	};
	rgba[0] = s_Rgba[i][0];
	rgba[1] = s_Rgba[i][1];
	rgba[2] = s_Rgba[i][2];
	rgba[3] = s_Rgba[i][3];
}

/**
	TODO

  Preconditions:
	TODO

  Postconditions:
	m_hidMemSpace is set to H5S_ALL if all cells are active
	otherwise set to the active memory dataspace
*/
int PhastDataSource::LoadGrid()
{
	static const char* names[4] = {"/Grid/X", "/Grid/Y", "/Grid/Z", "/Grid/Active"};
	hsize_t dims[1], maxdims[1];
	hid_t dspace_id;
	hid_t dset_id;
	float* coor[3] = {0, 0, 0};

	for (int e = 0; e < 3; ++e)
	{
		dset_id = H5Dopen(m_hidFile, names[e]);
		if (dset_id < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataset", names[e]);
			return 1;
		}

		dspace_id = H5Dget_space(dset_id);
		if (dspace_id < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataspace", names[e]);
			return 1;
		}

		if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on %s dataspace", names[e]);
			return 1;
		}
		m_ScalarGridDim[e] = dims[0];
		coor[e] = new float[m_ScalarGridDim[e]];
		assert(coor[e] != 0);
		if (coor[e] == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, coor[e]) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", names[e]);
			return 1;
		}
		if (H5Sclose(dspace_id) < 0) assert(false);
		if (H5Dclose(dset_id) < 0) assert(false);
	}

	int nx = m_ScalarGridDim[0];
	int nxy = nx * m_ScalarGridDim[1];
	int nxyz = nxy * m_ScalarGridDim[2];

	assert(m_ScalarGridCoordinates == 0);
	m_ScalarGridCoordinates = new float[3*nxyz];
	assert(m_ScalarGridCoordinates != 0);
	if (m_ScalarGridCoordinates == 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
		return 1;
	}


	int m;
	for (int k = 0; k < m_ScalarGridDim[2]; ++k)
	{
		for (int j = 0; j < m_ScalarGridDim[1]; ++j)
		{
			for (int i = 0; i < m_ScalarGridDim[0]; ++i)
			{
				m = k*nxy + j*nx + i;
				m_ScalarGridCoordinates[3*m]   = coor[0][i];
				m_ScalarGridCoordinates[3*m+1] = coor[1][j];
				m_ScalarGridCoordinates[3*m+2] = coor[2][k];
			}
		}
	}
	for (e = 0; e < 3; ++e)
	{
		delete[] coor[e];
	}

	assert(m_ScalarArray == 0);
	m_ScalarArray = new float[nxyz];
	assert(m_ScalarArray != 0);
	if (m_ScalarArray == 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
		return 1;
	}


	// check for active: exists only if not all active
	dset_id = H5Dopen(m_hidFile, names[3]);
	if (dset_id > 0)
	{
		dspace_id = H5Dget_space(dset_id);
		if (dspace_id < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataspace", names[3]);
			return 1;
		}

		if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on %s dataspace", names[3]);
			return 1;
		}

		hsize_t nActiveCells = dims[0];
		assert(nxyz > nActiveCells);
		m_nActiveCount = dims[0];

		int* arrnActiveCells = new int[nActiveCells];
		assert(arrnActiveCells != 0);
		if (arrnActiveCells == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, arrnActiveCells) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataset", names[3]);
			return 1;
		}
		if (H5Sclose(dspace_id) < 0) assert(false);
		if (H5Dclose(dset_id) < 0) assert(false);

		dims[0] = nxyz;
		m_hidMemSpace = H5Screate_simple(1, dims, NULL);
		assert(m_hidMemSpace > 0);

		hssize_t (*coord)[1] = new hssize_t[nActiveCells][1];
		if (coord == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		for (hsize_t j = 0; j < nActiveCells; ++j)
		{
			coord[j][0] = arrnActiveCells[j];
		}
		delete[] arrnActiveCells;

		herr_t status = H5Sselect_elements(m_hidMemSpace, H5S_SELECT_SET, nActiveCells, (const hssize_t **)coord);
		assert(status >= 0);
		delete[] coord;

		for (int i = 0; i < nxyz; ++i)
		{
			m_ScalarArray[i] = m_InactiveCellValue;
		}
	}
	else
	{
		// all active
		dims[0] = nxyz;
		m_hidMemSpace = H5Screate_simple(1, dims, NULL);
		assert(m_hidMemSpace > 0);

		// m_hidMemSpace = H5S_ALL;
		m_nActiveCount = nxyz;
	}

	// check for velocities
	return LoadVels();
}

int PhastDataSource::LoadDataTypeLabels()
{
	static const char name[] = "Scalars";
	hsize_t dims[1], maxdims[1];
	hid_t dspace_id;
	hid_t dset_id;
	hid_t dtype_id;
	herr_t status;

	dset_id = H5Dopen(m_hidFile, name);
	if (dset_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataset", name);
		return 1;
	}

	dspace_id = H5Dget_space(dset_id);
	if (dspace_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataspace", name);
		return 1;
	}
	if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on %s dataspace", name);
		return 1;
	}
	m_NumberOfScalarDataTypes = dims[0];

	dtype_id = H5Dget_type(dset_id);
	if (dtype_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_type of %s dataspace", name);
		return 1;
	}

	if (H5Tget_strpad(dtype_id) == H5T_STR_NULLTERM)
	{
		// fixed length strings
		size_t len = H5Tget_size(dtype_id);
		char* szScalars = new char[len * m_NumberOfScalarDataTypes];
		if (szScalars == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, szScalars) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", name);
			return 1;
		}
		if (H5Dclose(dset_id) < 0) assert(false);

		assert(m_DataTypeLabels == NULL);
		m_DataTypeLabels = new char*[m_NumberOfScalarDataTypes];
		assert(m_DataTypeLabels != 0);
		if (m_DataTypeLabels == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		for (int i = 0; i < m_NumberOfScalarDataTypes; ++i) 
		{
			m_DataTypeLabels[i] = new char[strlen(szScalars + i * len) + 1];
			assert(m_DataTypeLabels[i] != 0);
			if (m_DataTypeLabels[i] == 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
				return 1;
			}
			strcpy(m_DataTypeLabels[i], szScalars + i * len);
		}
		delete[] szScalars;
	}
	else
	{
		// variable length strings
		char** szScalars = new char*[m_NumberOfScalarDataTypes];
		if (szScalars == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, szScalars) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", name);
			return 1;
		}
		if (H5Dclose(dset_id) < 0) assert(false);

		assert(m_DataTypeLabels == NULL);
		m_DataTypeLabels = new char*[m_NumberOfScalarDataTypes];
		assert(m_DataTypeLabels != 0);
		if (m_DataTypeLabels == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		for (int i = 0; i < m_NumberOfScalarDataTypes; ++i) 
		{
			m_DataTypeLabels[i] = new char[strlen(szScalars[i]) + 1];
			assert(m_DataTypeLabels[i] != 0);
			if (m_DataTypeLabels[i] == 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
				return 1;
			}
			strcpy(m_DataTypeLabels[i], szScalars[i]);
		}

		// reclaim memory used to store variable length strings
		status = H5Dvlen_reclaim(dtype_id, dspace_id, H5P_DEFAULT, szScalars);
		assert(status >= 0);

		delete[] szScalars;
	}

	if (H5Sclose(dspace_id) < 0) assert(false);

	return 0;
}

int PhastDataSource::LoadTimeStepLabels()
{
	static const char name[] = "TimeSteps";
	hsize_t dims[1], maxdims[1];
	hid_t dspace_id;
	hid_t dset_id;
	hid_t dtype_id;
	herr_t status;

	dset_id = H5Dopen(m_hidFile, name);
	if (dset_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataset", name);
		return 1;
	}

	dspace_id = H5Dget_space(dset_id);
	if (dspace_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataspace", name);
		return 1;
	}
	if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on %s dataspace", name);
		return 1;
	}
	m_NumberOfTimePoints = dims[0];

	dtype_id = H5Dget_type(dset_id);
	if (dtype_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_type of %s dataspace", name);
		return 1;
	}

	if (H5Tget_strpad(dtype_id) == H5T_STR_NULLTERM)
	{
		// fixed length strings
		size_t len = H5Tget_size(dtype_id);
		char* szTimeSteps = new char[len * m_NumberOfTimePoints];
		if (szTimeSteps == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, szTimeSteps) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", name);
			return 1;
		}
		if (H5Dclose(dset_id) < 0) assert(false);

		assert(m_TimePointLabels == NULL);
		m_TimePointLabels = new char*[m_NumberOfTimePoints];
		assert(m_TimePointLabels != 0);
		if (m_TimePointLabels == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		for (int i = 0; i < m_NumberOfTimePoints; ++i) 
		{
			m_TimePointLabels[i] = new char[strlen(szTimeSteps + i * len) + 1];
			assert(m_TimePointLabels[i] != 0);
			if (m_TimePointLabels[i] == 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
				return 1;
			}
			strcpy(m_TimePointLabels[i], szTimeSteps + i * len);
		}
		delete[] szTimeSteps;
	}
	else
	{
		// variable length strings
		char** szTimeSteps = new char*[m_NumberOfTimePoints];
		assert(szTimeSteps != 0);
		if (szTimeSteps == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, szTimeSteps) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", name);
			return 1;
		}
		if (H5Dclose(dset_id) < 0) assert(false);

		assert(m_TimePointLabels == NULL);
		m_TimePointLabels = new char*[m_NumberOfTimePoints];
		assert(m_TimePointLabels != 0);
		if (m_TimePointLabels == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		for (int i = 0; i < m_NumberOfTimePoints; ++i) 
		{
			m_TimePointLabels[i] = new char[strlen(szTimeSteps[i]) + 1];
			assert(m_TimePointLabels[i] != 0);
			if (m_TimePointLabels[i] == 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
				return 1;
			}
			strcpy(m_TimePointLabels[i], szTimeSteps[i]);
		}

		// reclaim memory used to store variable length strings
		status = H5Dvlen_reclaim(dtype_id, dspace_id, H5P_DEFAULT, szTimeSteps);
		assert(status >= 0);

		delete[] szTimeSteps;
	}

	if (H5Sclose(dspace_id) < 0) assert(false);
	
	return 0;
}

int PhastDataSource::LoadFeatures()
{
	static const char* arrszFeatures[6] = {"/Features/Flux", "/Features/Leaky", "/Features/River", "/Features/Specified", "/Features/Wells", "/Features/Drain"};
	int arrnFeatures[6] = {0, 0, 0, 0, 0};
	hid_t dset_id[6];
	hsize_t dims[1], maxdims[1];


	m_NumberOfModelFeatureTypes = ARRAY_SIZE(arrszFeatures);
	

	assert(m_ModelFeatureLabels == 0);
	m_ModelFeatureLabels = new char[m_NumberOfModelFeatureTypes*40];
	assert(m_ModelFeatureLabels != 0);
	if (m_ModelFeatureLabels == 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
		return 1;
	}

	int modelFeatureArraySize = m_NumberOfModelFeatureTypes;
	for (int i = 0; i < m_NumberOfModelFeatureTypes; ++i)
	{
		::_snprintf(m_ModelFeatureLabels + i*40, 40, "%-39s", arrszFeatures[i] + 10);
		dset_id[i] = H5Dopen(m_hidFile, arrszFeatures[i]);
		if (dset_id[i] > 0)
		{
			hid_t dspace_id = H5Dget_space(dset_id[i]);
			if (dspace_id < 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataspace", arrszFeatures[i]);
				return 1;
			}

			if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on %s dataspace", arrszFeatures[i]);
				return 1;
			}
			arrnFeatures[i] = dims[0];
			modelFeatureArraySize += dims[0];
			if (H5Sclose(dspace_id) < 0) assert(false);
		}
	}

	assert(m_ModelFeatureArray == 0);
	m_ModelFeatureArray = new int[modelFeatureArraySize];
	assert(m_ModelFeatureArray != 0);
	if (m_ModelFeatureArray == 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
		return 1;
	}

	int* pInt = m_ModelFeatureArray;
	for (int j = 0; j < m_NumberOfModelFeatureTypes; ++j)
	{
		pInt[0] = arrnFeatures[j];
		++pInt;
		if (dset_id[j] > 0)
		{
			if (H5Dread(dset_id[j], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, pInt) < 0)
			{
				::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataset", arrszFeatures[j]);
				return 1;
			}
			pInt += arrnFeatures[j];
			if (H5Dclose(dset_id[j]) < 0) assert(false);
		}
	}
	return 0;
}

int PhastDataSource::LoadVels()
{
	static const char name[] = "Vectors";
	hsize_t dims[1], maxdims[1];
	hid_t dspace_id;
	hid_t dset_id;
	hid_t dtype_id;
	herr_t status;

	dset_id = H5Dopen(m_hidFile, name);
	if (dset_id < 0)
	{
		// No Vectors => no velocities
		return 0;
	}

	dspace_id = H5Dget_space(dset_id);
	if (dspace_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't open %s dataspace", name);
		return 1;
	}
	if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_simple_extent_dims on %s dataspace", name);
		return 1;
	}


	dtype_id = H5Dget_type(dset_id);
	if (dtype_id < 0)
	{
		::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't get_type of %s dataspace", name);
		return 1;
	}

	bool bVelFound = false;
	if (H5Tget_strpad(dtype_id) == H5T_STR_NULLTERM)
	{
		// fixed length strings
		size_t len = H5Tget_size(dtype_id);
		char* szVectors = new char[len * dims[0]];
		assert(szVectors != 0);
		if (szVectors == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, szVectors) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", name);
			return 1;
		}
		if (H5Dclose(dset_id) < 0) assert(false);

		for (hsize_t i = 0; i < dims[0]; ++i) 
		{
			if (strcmp("Velocities", szVectors + i * len) == 0)
			{
				bVelFound = true;
				break;
			}
		}

		delete[] szVectors;
	}
	else
	{
		// variable length strings
		char** szVectors = new char*[dims[0]];
		assert(szVectors != 0);
		if (szVectors == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}

		if (H5Dread(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, szVectors) < 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Can't read %s dataspace", name);
			return 1;
		}
		if (H5Dclose(dset_id) < 0) assert(false);

		for (hsize_t i = 0; i < dims[0]; ++i) 
		{
			if (strcmp("Velocities", szVectors[i]) == 0)
			{
				bVelFound = true;
				break;
			}
		}

		// reclaim memory used to store variable length strings
		status = H5Dvlen_reclaim(dtype_id, dspace_id, H5P_DEFAULT, szVectors);
		assert(status >= 0);

		delete[] szVectors;
	}

	if (H5Sclose(dspace_id) < 0) assert(false);


	if (bVelFound)
	{
		int nxyz = m_ScalarGridDim[0] * m_ScalarGridDim[1] * m_ScalarGridDim[2];
		assert(nxyz > 0);

		assert(m_VectorArray == 0);  // shouldn't be allocated yet
		m_VectorArray = new float[3*nxyz];
		assert(m_VectorArray != 0);
		if (m_VectorArray == 0)
		{
			::_snprintf(s_ERR_MSG, ARRAY_SIZE(s_ERR_MSG), "Out of memory");
			return 1;
		}
	}

	return 0;
}

int PhastDataSource::GetNumPoints()
{
	const int *sdim = GetScalarGridDimensions();
	return sdim[0]*sdim[1]*sdim[2];
}

int PhastDataSource::GetNumCells()
{
	const int *sdim = GetScalarGridDimensions();
	int numCells = (sdim[0])*(sdim[1]);
	if (sdim[2] > 1)
	{
		numCells *= (sdim[2]);
	}
	return numCells;
}
