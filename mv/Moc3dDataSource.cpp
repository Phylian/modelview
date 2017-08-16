#include "Moc3dDataSource.h"
#include "Modflow96Reader.h"
#include "Modflow96Reader1.h"
#include "Modflow96Reader2.h"

Moc3dDataSource::Moc3dDataSource() : AbstractModflowDataSource()
{
	m_NormalizeVector = 0;
	m_NumberOfScalarDataTypes = 1;	// show only concentration for now
	m_UseElevFile = 1;
	m_Transport = 1;
	m_ErrorMsg[3] = "Error: Unable to open the top elevation file";
	m_ErrorMsg[8] = "Error: No concentration data.";
	m_ErrorMsg[9] = "Error encountered while reading concentration file.";
	m_ErrorMsg[11] = "Error: MOC3D not invoked in the name file.";
	m_ErrorMsg[10] = "Error encountered while reading top elevation file";
	m_ErrorMsg[20] = "Error encountered while reading Moc3D data files";
}

Moc3dDataSource::~Moc3dDataSource()
{
	MfCleanup(); 
}

void Moc3dDataSource::MfCleanup()
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96cleanup0_();
		return;
	case 2:
		mf96cleanup1();
		return;
	case 3:
		mf96cleanup2_();
		return;
	}
}

void Moc3dDataSource::MfDims(int *ierror, int *ncol, int *nrow, int *nlay, int *timeunit, char *namefile, char *elevfile, int len1, int len2)
{
	int unstruct = 0;
	int moc = 1;
	switch(m_DataType)
	{
	case 0:
		unstruct = 1;
		//fall through
	case 1:	
		mf96dims0_(ierror, ncol, nrow, nlay, &moc, &unstruct, timeunit, namefile, elevfile, len1, len2);
		return;
	case 2:
		mf96dims1(ierror, ncol, nrow, nlay, &moc, &unstruct, timeunit, namefile, elevfile, len1, len2);
		return;
	case 3:
		mf96dims2_(ierror, ncol, nrow, nlay, &moc, &unstruct, timeunit, namefile, elevfile, len1, len2);
		return;
	}
}

void Moc3dDataSource::MfGrid(int *ierror, float *delr, float *delc, float *elev,
				int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96grid0_(ierror, delr, delc, elev, ibound, conductivity, hnoflo, hdry, sunit, vunit, xoffset, yoffset, isMfLayer);
		return;
	case 2:
		mf96grid1(ierror, delr, delc, elev, ibound, conductivity, hnoflo, hdry, sunit, vunit, xoffset, yoffset, isMfLayer);
		return;
	case 3:
		mf96grid2_(ierror, delr, delc, elev, ibound, conductivity, hnoflo, hdry, sunit, vunit, xoffset, yoffset, isMfLayer);
		return;
	}
}

void Moc3dDataSource::MfCountScalars(int *ierror, int *iunit, int *numTimePoints, char *dataType, int len)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96countscalars0_(ierror, iunit, numTimePoints, dataType, len);
		return;
	case 2:
		mf96countscalars1(ierror, iunit, numTimePoints, dataType, len);
		return;
	case 3:
		mf96countscalars2_(ierror, iunit, numTimePoints, dataType, len);
		return;
	}
}

void Moc3dDataSource::MfCountVectors(int *ierror, int *iunit)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96countvectors0_(ierror, iunit);
		return;
	case 2:
		mf96countvectors1(ierror, iunit);
		return;
	case 3:
		mf96countvectors2_(ierror, iunit);
		return;
	}
}

void Moc3dDataSource::MfCountFeatures(int *ierror, int *modelFeatureArraySize, 
											int *numModelFeatureTypes, int *notUsed)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96countfeatures0_(ierror, modelFeatureArraySize, numModelFeatureTypes);
		return;
	case 2:
		mf96countfeatures1(ierror, modelFeatureArraySize, numModelFeatureTypes);
		return;
	case 3:
		mf96countfeatures2_(ierror, modelFeatureArraySize, numModelFeatureTypes);
		return;
	}
}

void Moc3dDataSource::MfGetTimePoints(float *timePoints, int *periods, int *steps, int *moves, int numTimePoints)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96gettimepoints0_(timePoints, periods, steps, moves, &numTimePoints);
		return;
	case 2:
		mf96gettimepoints1(timePoints, periods, steps, moves, &numTimePoints);
		return;
	case 3:
		mf96gettimepoints2_(timePoints, periods, steps, moves, &numTimePoints);
		return;
	}
}

void Moc3dDataSource::MfGetScalars(int *ierror, int *iunit, float *cellValues, int *timePointIndex, int *kper)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96getscalars0_(ierror, iunit, cellValues, timePointIndex, kper);
		return;
	case 2:
		mf96getscalars1(ierror, iunit, cellValues, timePointIndex, kper);
		return;
	case 3:
		mf96getscalars2_(ierror, iunit, cellValues, timePointIndex, kper);
		return;
	}
}


void Moc3dDataSource::MfGetVectors(int *ierror, int *iunit, float *vectorArray, int *timePointIndex)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96getvectors0_(ierror, iunit, vectorArray, timePointIndex);
		return;
	case 2:
		mf96getvectors1(ierror, iunit, vectorArray, timePointIndex);
		return;
	case 3:
		mf96getvectors2_(ierror, iunit, vectorArray, timePointIndex);
		return;
	}
}

void Moc3dDataSource::MfGetFeatures(int *ierror, int *ibnode)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96getfeatures0_(ierror, ibnode);
		return;
	case 2:
		mf96getfeatures1(ierror, ibnode);
		return;
	case 3:
		mf96getfeatures2_(ierror, ibnode);
		return;
	}
}

void Moc3dDataSource::MFReadParticleCount(int *numCoordinates,
			int *ierror, int *istep)
{
	
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96readparticlecount0_(numCoordinates, ierror, istep);
		break;
	case 2:
		mf96readparticlecount1(numCoordinates, ierror, istep);
		break;
	case 3:
		mf96readparticlecount2_(numCoordinates, ierror, istep);
		break;
	}
	return;
}

void Moc3dDataSource::MFReadParticles(int *NP, int *ierror, 
	float *part_coord, float *part_concentrations, float *delr, float *delc, 
	float *elev)
{
	switch(m_DataType)
	{
	case 0:	//fall through
	case 1:
		mf96readparticles0_(NP, ierror, part_coord, part_concentrations, delr, delc, elev);
		break;
	case 2:
		mf96readparticles1(NP, ierror, part_coord, part_concentrations, delr, delc, elev);
		break;
	case 3:
		mf96readparticles2_(NP, ierror, part_coord, part_concentrations, delr, delc, elev);
		break;
	}
	return;
}