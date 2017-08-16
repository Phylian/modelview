#include "Modflow2000DataSource.h"
#include "Modflow2000Reader.h"
#include "Modflow2000Reader1.h"
#include "Modflow2000Reader2.h"

Modflow2000DataSource::Modflow2000DataSource() : AbstractModflowDataSource()
{
	m_UseElevFile = 0;
	m_Transport = 0;
	m_ErrorMsg[8] = "Error: No head or drawdown data.";
	m_ErrorMsg[9] = "Error encountered while reading Modflow 2000 data file.";
	m_ErrorMsg[20] = "Error encountered while reading Modflow 2000 data files.";
}

Modflow2000DataSource::~Modflow2000DataSource()
{
	MfCleanup(); 
}

void Modflow2000DataSource::MfCleanup()
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kcleanup0_();
		return;
	case 2:
		mf2kcleanup1();
		return;
	case 3:
		mf2kcleanup2_();
		return;
	}
}

void Modflow2000DataSource::MfDims(int *ierror, int *ncol, int *nrow, int *nlay, int *timeunit, char *namefile, char *elevfile, int len1, int len2)
{
	// Note: elevfile and len2 are not used.
	int unstruct = 0;
	int gwt = 0;
	switch(m_DataType)
	{
	case 0:
		unstruct = 1;
		//fall through
	case 1:	
		mf2kdims0_(ierror, ncol, nrow, nlay, &gwt, &unstruct, timeunit, namefile, len1);
		return;
	case 2:
		mf2kdims1(ierror, ncol, nrow, nlay, &gwt, &unstruct, timeunit, namefile, len1);
		return;
	case 3:
		mf2kdims2_(ierror, ncol, nrow, nlay, &gwt, &unstruct, timeunit, namefile, len1);
		return;
	}
}

void Modflow2000DataSource::MfGrid(int *ierror, float *delr, float *delc, float *elev,
								   int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kgrid0_(ierror, delr, delc, elev, ibound, conductivity, hnoflo, hdry, sunit, vunit, xoffset, yoffset, isMfLayer);
		return;
	case 2:
		mf2kgrid1(ierror, delr, delc, elev, ibound, conductivity, hnoflo, hdry, sunit, vunit, xoffset, yoffset, isMfLayer);
		return;
	case 3:
		mf2kgrid2_(ierror, delr, delc, elev, ibound, conductivity, hnoflo, hdry, sunit, vunit, xoffset, yoffset, isMfLayer);
		return;
	}
}

void Modflow2000DataSource::MfCountScalars(int *ierror, int *iunit, int *numTimePoints, char *dataType, int len)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kcountscalars0_(ierror, iunit, numTimePoints, dataType, len);
		return;
	case 2:
		mf2kcountscalars1(ierror, iunit, numTimePoints, dataType, len);
		return;
	case 3:
		mf2kcountscalars2_(ierror, iunit, numTimePoints, dataType, len);
		return;
	}
}

void Modflow2000DataSource::MfCountVectors(int *ierror, int *iunit)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kcountvectors0_(ierror, iunit);
		return;
	case 2:
		mf2kcountvectors1(ierror, iunit);
		return;
	case 3:
		mf2kcountvectors2_(ierror, iunit);
		return;
	}
}

void Modflow2000DataSource::MfCountFeatures(int *ierror, int *modelFeatureArraySize, 
											int *numModelFeatureTypes, int *ibound)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kcountfeatures0_(ierror, modelFeatureArraySize, numModelFeatureTypes, ibound);
		return;
	case 2:
		mf2kcountfeatures1(ierror, modelFeatureArraySize, numModelFeatureTypes, ibound);
		return;
	case 3:
		mf2kcountfeatures2_(ierror, modelFeatureArraySize, numModelFeatureTypes, ibound);
		return;
	}
}

void Modflow2000DataSource::MfGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kgettimepoints0_(timePoints, periods, steps, moves, &numTimePoints);
		return;
	case 2:
		mf2kgettimepoints1(timePoints, periods, steps, moves, &numTimePoints);
		return;
	case 3:
		mf2kgettimepoints2_(timePoints, periods, steps, moves, &numTimePoints);
		return;
	}
}

void Modflow2000DataSource::MfGetScalars(int *ierror, int *iunit, float *cellValues, int *timePointIndex, int *kper)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kgetscalars0_(ierror, iunit, cellValues, timePointIndex, kper);
		return;
	case 2:
		mf2kgetscalars1(ierror, iunit, cellValues, timePointIndex, kper);
		return;
	case 3:
		mf2kgetscalars2_(ierror, iunit, cellValues, timePointIndex, kper);
		return;
	}
}


void Modflow2000DataSource::MfGetVectors(int *ierror, int *iunit, float *vectorArray, int *timePointIndex)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kgetvectors0_(ierror, iunit, vectorArray, timePointIndex);
		return;
	case 2:
		mf2kgetvectors1(ierror, iunit, vectorArray, timePointIndex);
		return;
	case 3:
		mf2kgetvectors2_(ierror, iunit, vectorArray, timePointIndex);
		return;
	}
}

void Modflow2000DataSource::MfGetFeatures(int *ierror, int *ibnode)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kgetfeatures0_(ierror, ibnode);
		return;
	case 2:
		mf2kgetfeatures1(ierror, ibnode);
		return;
	case 3:
		mf2kgetfeatures2_(ierror, ibnode);
		return;
	}
}
