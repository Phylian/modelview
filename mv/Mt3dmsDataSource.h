#ifndef __Mt3dmsDataSource_h
#define __Mt3dmsDataSource_h

#include "mvDataSource.h"
#include "mvModelFeatures.h"

class MV_EXPORT Mt3dmsDataSource : public mvDataSource  
{
public:
	Mt3dmsDataSource();
	virtual ~Mt3dmsDataSource();

	static char *GetNameStatic() {return "Mt3dms";}
	virtual char *GetName() {return GetNameStatic();}
	virtual int GetPrimaryScalarMode() {return MV_CELL_SCALARS;}
	virtual int GetGridType() {return MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS;}
	virtual void GetDefaultModelFeatureColor(int i, float *rgba);
	virtual char *LoadData(char *dataFileList);
	virtual void AdvanceOneTimePoint();
	virtual void SetTimePointTo(int timePointIndex);
	virtual void SetScalarDataTypeTo(int dataTypeIndex) {/*Not used*/}
	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_CELLS;}

protected:
	float m_Mt3dmsInactiveCellValue;
	float *m_Delr;
	float *m_Delc;
	float *m_Elev;
	int m_DataType;
	char *m_ErrorMsg[8];

	virtual void ReleaseMemory();
	void MtDims(int *ierror, char *cnfFile, char *ucnFile, char *lmtFile, int *nc, int *nr, int *nl);
	void MtGrid(int *ierror, float *delr, float *delc, float *elev, float *cinact);
	void MtCountScalars(int *ierror, int *numTimePoints, char *dataType, int len);
    void MtCountVectorsAndFeatures(int *ierror, int *ibousz, int *nfeat);
    void MtGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints);
	void MtGetScalars(int *ierror, float *cellValues, int *timePointIndex);
	void MtGetVectorsAndFeatures(int *ierror, float *flowArray, int *timePointIndex, int *ibnode);
};

#endif
