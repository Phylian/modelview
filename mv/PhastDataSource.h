#ifndef __PhastDataSource_h
#define __PhastDataSource_h

#include "mvDataSource.h"
#include "mvModelFeatures.h"

#define _HDF5USEDLL_
#include <hdf5.h>       /* HDF routines */

class MV_EXPORT PhastDataSource : public mvDataSource  
{
public:
	PhastDataSource();
	virtual ~PhastDataSource();

	virtual char *GetName() {return GetNameStatic();}

	static char *GetNameStatic() {return "Phast HDF";}

	virtual int GetPrimaryScalarMode() {return MV_POINT_SCALARS;}

	virtual int GetGridType() {return MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS;}

	virtual char *LoadData(char *dataFileList);

	virtual void AdvanceOneTimePoint();

	virtual void SetTimePointTo(int timePointIndex);

	virtual void SetScalarDataTypeTo(int dataTypeIndex);

	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS;}	

	virtual void GetDefaultModelFeatureColor(int i, float *rgba);

	virtual int GetNumberOfTimePoints() {return m_NumberOfTimePoints;}

	virtual float *GetScalarArray() {return m_ScalarArray;}

	virtual int *GetVectorGridDimensions() {return m_ScalarGridDim;}

	virtual float *GetVectorGridCoordinates() {return m_ScalarGridCoordinates;}

	virtual float *GetVectorArray() {return m_VectorArray;}

	virtual int GetNumPoints();

	virtual int GetNumCells();


private:
	char  m_szHDF[_MAX_PATH];
	hid_t m_hidFile;
	int   m_nTimePointIndex;
	int   m_nDataTypeIndex;
	hid_t m_hidMemSpace;
	int   m_nActiveCount;

private:
	int SetScalarArray(int timePointIndex, int dataTypeIndex);
	int SetVectorArray(hid_t timestep_group_id, int timePointIndex);

	int LoadGrid();
	int LoadDataTypeLabels();
	int LoadTimeStepLabels();
	int LoadFeatures();
	int LoadVels();
	int OpenHDF(char *dataFileList);
};

#endif