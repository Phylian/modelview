#ifndef __Modflow96DataSource_h
#define __Modflow96DataSource_h

#include "AbstractModflowDataSource.h"

class MV_EXPORT Modflow96DataSource : public AbstractModflowDataSource  
{
public:

	Modflow96DataSource();
	virtual ~Modflow96DataSource();

	static char *GetNameStatic() {return "Modflow 96";}
	virtual char *GetName() {return GetNameStatic();}

protected:

	virtual void MfDims(int *ierror, int *ncol, int *nrow, int *nlay, int *timeunit,
						char *namefile, char *elevfile, int len1, int len2);
	virtual void MfGrid(int *ierror, float *delr, float *delc, float *elev, int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer);
	virtual void MfCountScalars(int *ierror, int *iunit, int *numTimePoints, char *dataType, int len);
	virtual void MfCountVectors(int *ierror, int *iunit);
    virtual void MfCountFeatures(int *ierror, int *ibousz, int *nfeat, int *notUsed);
    virtual void MfGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints);
	virtual void MfGetScalars(int *ierror, int *iunit, float *cellValues, int *timePointIndex, int *kper);
	virtual void MfGetVectors(int *ierror, int *iunit, float *flowArray, int *timePointIndex);
    virtual void MfGetFeatures(int *ierror, int *ibnode);
    void MfCleanup();
};

#endif