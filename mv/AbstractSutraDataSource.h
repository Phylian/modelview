// AbstractSutraDataSource.h: interface for the AbstractSutraDataSource class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __AbstractSutraDataSource_h
#define __AbstractSutraDataSource_h

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "mvDataSource.h"
#include "mvModelFeatures.h"

class MV_EXPORT AbstractSutraDataSource : public mvDataSource  
{
public:
	AbstractSutraDataSource();
	virtual ~AbstractSutraDataSource();

	static char *GetNameStatic() {return "SUTRA";}
	virtual char *GetName() {return GetNameStatic();}
	virtual int GetPrimaryScalarMode() {return MV_POINT_SCALARS;}

	virtual int GetGridType() {return MV_STRUCTURED_GRID_ALL_ACTIVE;}
	virtual char *LoadData(char *dataFileList);
	virtual void AdvanceOneTimePoint();
	virtual void SetTimePointTo(int timePointIndex);
	virtual void SetScalarDataTypeTo(int dataTypeIndex);
	//virtual int GetTimeLabelOption() {return 1;}
	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS;}
	virtual int GetInitialGridDisplayPolicy() {return MV_INITIAL_DISPLAY_GRID_OUTLINE;}
	virtual void GetDefaultModelFeatureColor(int i, float *rgba);

	char *AtoBConvert1(ifstream &nod, ifstream &ele, const char *inpFile, ofstream &bin, 
			int &numNodes, int &numElements, int &numScalarDataTypes, int &numTimePoints, int &nz);
	char *AtoBConvert2(ifstream &nod, ifstream &ele, ofstream &out, int timeStep,
			int numNodes, int numElements, int numScalarDataTypes, int numTimePoints, int nz);

protected:
	ifstream *m_In1;
	ifstream *m_In2;

	char m_NodFile[256];
	char m_EleFile[256];
	int m_NumberOfNodes;
	int m_NumberOfElements;
	int m_NumberOfElementTimePoints;
	int m_FileType;


	static char *ReadNodFile(ifstream &in, int *sdim, int &numNodes, int &numElements,
			int &numTimePoints, char **&timePointLabels, int &numScalarDataTypes,
			char **&dataTypeLabels, float *&sgCoord);

	static char *ReadEleFile(ifstream &in, int *sdim, int &numTimePoints, 
			char **timePointLabels, float *vgCoord, bool &inputread, 
			int &SteadyFlow, int &SteadyTransport, int &ElementTimePoints);

	char *LoadBinaryFile();

	virtual void GetInput(int *ierror, int *istart, int *ibousz, int *ibnode, 
  int *nfeatures, int *ISTEADYFLOW, int *ISTEADYTRANSPORT, char *inpfile, int len) = 0;

	virtual void GetLabels(char *flabels, int len) = 0;

};

#endif 
