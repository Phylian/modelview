#ifndef __GenericDataSource_h
#define __GenericDataSource_h

#include "mvDataSource.h"
#include "mvModelFeatures.h"

class MV_EXPORT GenericDataSource : public mvDataSource
{
public:
	GenericDataSource();
	virtual ~GenericDataSource();

	static char *GetNameStatic() {return "Generic Data";}
	virtual char *GetName() {return GetNameStatic();}
	virtual int GetPrimaryScalarMode();
	virtual int GetGridType() {return MV_STRUCTURED_GRID_ALL_ACTIVE;}
	virtual char *LoadData(char *dataFileList);
	virtual void AdvanceOneTimePoint();
	virtual void SetTimePointTo(int timePointIndex);
	virtual void SetScalarDataTypeTo(int dataTypeIndex) {}
	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_CELLS;}	
	virtual void GetDefaultModelFeatureColor(int i, float *rgba) {}

protected:

};

#endif
