#include "Mf2kgwtDataSource.h"
#include "Modflow2000Reader.h"
#include "Modflow2000Reader1.h"
#include "Modflow2000Reader2.h"

Mf2kgwtDataSource::Mf2kgwtDataSource() : AbstractModflowDataSource()
{
	m_NormalizeVector = 0;
	m_NumberOfScalarDataTypes = 1;	// show only concentration for now
	m_UseElevFile = 0;
	m_Transport = 1;
	m_ErrorMsg[8] = "Error: No concentration data.";
	m_ErrorMsg[9] = "Error encountered while reading concentration file.";
	m_ErrorMsg[11] = "Error: Ground-Water Transport Process (GWT) not invoked in name file.";
	m_ErrorMsg[20] = "Error encountered while reading Modflow 2000 - GWT data files.";
}

Mf2kgwtDataSource::~Mf2kgwtDataSource()
{
	MfCleanup(); 
}

void Mf2kgwtDataSource::MfCleanup()
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

void Mf2kgwtDataSource::MfDims(int *ierror, int *ncol, int *nrow, int *nlay, int *timeunit, char *namefile, char *elevfile, int len1, int len2)
{
	// Note: elevfile and len2 are not used.
	int unstruct = 0;
	int gwt = 1;
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

void Mf2kgwtDataSource::MfGrid(int *ierror, float *delr, float *delc, float *elev,
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

void Mf2kgwtDataSource::MfCountScalars(int *ierror, int *iunit, int *numTimePoints, char *dataType, int len)
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

void Mf2kgwtDataSource::MfCountVectors(int *ierror, int *iunit)
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

void Mf2kgwtDataSource::MfCountFeatures(int *ierror, int *modelFeatureArraySize, 
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

void Mf2kgwtDataSource::MfGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints)
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

void Mf2kgwtDataSource::MfGetScalars(int *ierror, int *iunit, float *cellValues, int *timePointIndex, int *kper)
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


void Mf2kgwtDataSource::MfGetVectors(int *ierror, int *iunit, float *vectorArray, int *timePointIndex)
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

void Mf2kgwtDataSource::MfGetFeatures(int *ierror, int *ibnode)
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


void Mf2kgwtDataSource::MFReadParticleCount(int *numCoordinates,
			int *ierror, int *istep)
{
	
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kreadparticlecount0_(numCoordinates, ierror, istep);
		break;
	case 2:
		mf2kreadparticlecount1(numCoordinates, ierror, istep);
		break;
	case 3:
		mf2kreadparticlecount2_(numCoordinates, ierror, istep);
		break;
	}
	return;
}

void Mf2kgwtDataSource::MFReadParticles(int *NP, int *ierror, 
	float *part_coord, float *part_concentrations, float *delr, float *delc, 
	float *elev)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf2kreadparticles0_(NP, ierror, part_coord, part_concentrations, delr, delc, elev);
		break;
	case 2:
		mf2kreadparticles1(NP, ierror, part_coord, part_concentrations, delr, delc, elev);
		break;
	case 3:
		mf2kreadparticles2_(NP, ierror, part_coord, part_concentrations, delr, delc, elev);
		break;
	}
	return;
}