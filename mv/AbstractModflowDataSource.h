#ifndef __AbstractModflowDataSource_h
#define __AbstractModflowDataSource_h

#include "mvDataSource.h"
#include "mvModelFeatures.h"

/**
 * Abstract base class for reading Modflow data. 
 */
class MV_EXPORT AbstractModflowDataSource : public mvDataSource  
{
public:
	AbstractModflowDataSource();
	virtual ~AbstractModflowDataSource();

	int m_UseExternalFile;
	virtual int GetPrimaryScalarMode() {return MV_CELL_SCALARS;}
	virtual int GetGridType() {return MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS;}
	virtual void GetDefaultModelFeatureColor(int i, float *rgba);
	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_CELLS;}
	virtual char *LoadData(char *dataFileList);
	virtual void AdvanceOneTimePoint();
	virtual void SetTimePointTo(int timePointIndex);
	virtual void SetScalarDataTypeTo(int dataTypeIndex);
	virtual char *GetTimeUnit();

protected:
	int m_ScalarUnit[11];
	int m_VectorUnit;
	float *m_Delr;
	float *m_Delc;
	float *m_Elev;
	float *m_TrueElev;
	int *m_Ibound;
	int m_StressPeriod;
	int m_DataType;
	int m_UseElevFile;
	int m_Transport;
	float m_InactiveValue1;
	float m_InactiveValue2;
	int m_NormalizeVector;
	char *m_ErrorMsg[22];
	int *m_IsMfLayer;
	int m_TimeUnit;

	virtual void ReleaseMemory();
	virtual void MfDims(int *ierror, int *ncol, int *nrow, int *nlay, int *timeunit,
						char *namefile, char *elevfile, int len1, int len2) = 0;
	virtual void MfGrid(int *ierror, float *delr, float *delc, float *elev, 
						int *ibound, float *conductivity, float *hnoflo, float *hdry, 
						int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer)=0;
	virtual void MfCountScalars(int *ierror, int *iunit, int *numTimePoints, char *dataType, int len) = 0;
	virtual void MfCountVectors(int *ierror, int *iunit) = 0;
    virtual void MfCountFeatures(int *ierror, int *ibousz, int *nfeat, int *ibound) = 0;
	virtual void MfGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints) = 0;
	virtual void MfGetScalars(int *ierror, int *iunit, float *cellValues, int *timePointIndex, int *kper) = 0;
	virtual void MfGetVectors(int *ierror, int *iunit, float *flowArray, int *timePointIndex) = 0;
    virtual void MfGetFeatures(int *ierror, int *ibnode) = 0;
	virtual void MFReadParticleCount(int *numCoordinates,
			int *ierror, int *istep);
	virtual void MFReadParticles(int *NP, int *ierror, float *part_coord, 
		float *part_concentrations, float *delr, float *delc, float *elev);
};

#endif