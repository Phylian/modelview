#ifndef __SutraDataSource_h
#define __SutraDataSource_h

#include "mvDataSource.h"
#include "mvModelFeatures.h"
#include "mvSutraFace.h"
#include "mvFaceLess.h"
#include <set>

#define ADDITIONAL_ELEMENT_DATA_SETS 16
#define ADDITIONAL_2D_ELEMENT_DATA_SETS 11
#define VECTOR_DATA_SETS 4
#define ADDITIONAL_NODE_DATA_SETS 1
#define MAXIMUM_NUMBER_OF_SUTRA_MODEL_FEATURE_TYPES 4
#define TIME_LABEL_SIZE 40

class mvExternalMeshVector;
class ifstream;
class ofstream;
class vtkPolyData;

typedef std::set<mvSutraFace*, mvFaceLess> SetFaces;

class MV_EXPORT SutraDataSource : public mvDataSource  
{
public:
	SutraDataSource();
	int m_UseExternalFile;
	virtual ~SutraDataSource();

	static char *GetNameStatic() {return "SUTRA";}
	virtual char *GetName() {return GetNameStatic();}
	virtual int GetPrimaryScalarMode() ;//{return MV_POINT_SCALARS;}

	virtual int GetGridType() {return m_GridType;}
	virtual bool GetLayeredMesh() {return m_LayeredMesh;}
	virtual char *LoadData(char *dataFileList);
	virtual void AdvanceOneTimePoint();
	virtual void SetTimePointTo(int timePointIndex);
	virtual void SetScalarDataTypeTo(int dataTypeIndex);
	//virtual int GetTimeLabelOption() {return 1;}
	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS;}
	virtual int GetInitialGridDisplayPolicy() {return MV_INITIAL_DISPLAY_GRID_OUTLINE;}
	virtual void GetDefaultModelFeatureColor(int i, float *rgba);
	virtual int *GetScalarGridDimensions();
	virtual float *GetScalarGridCoordinates();
	virtual float *GetScalarArray();
	virtual int GetDataSetToUseForRange();

	/*
	  ReadDimensions reads the number of nodes and elements in the model and 
	  the dimensions of the model.

      It is called by AtoBConvert1 and ReadNodFile.
	*/

	static char *ReadDimensions(ifstream &in, int &numNodes, int &numElements, int *sdim);

	/*
	  AtoBConvert1 reads data from the nod, ele and inp files and saves 
	  the non-transient data to a binary file.

      Called from DataFilesDialog.cpp
	*/
	static char *AtoBConvert1(ifstream &nod, ifstream &ele, const char *inpFile, ofstream &bin, 
			int &numNodes, int &numElements, int &numScalarDataTypes, int &numTimePoints, bool &IceSatFraction, int &nz,
			int &numNodalScalarDataTypes, int &numElementTimePoints, int *incidence, int &TimeUnits);

	/*
	  AtoBConvert2 reads data from the nod, ele and inp files and saves 
	  the transient data to a binary file.

      Called from DataFilesDialog.cpp
	*/
	static char *AtoBConvert2(ifstream &nod, ifstream &ele, ofstream &out, int timeStep,
			int numNodes, int numElements, int numScalarDataTypes, int numTimePoints, int nz,
			int numNodalScalarDataTypes, int numElementTimePoints, int *Incidence);

	virtual void AssignConnectivity(int *types, vtkCellArray* connectivity, 
		vtkPolyData *meshlines, mvExternalMeshVector *meshvector, mvExternalMeshVector *linesvector);
	virtual int GetNumPoints();
	virtual int GetNumCells();


protected:
	ifstream *m_In1;
	ifstream *m_In2;

	int m_ScalarGridDim2[3];
	char m_NodFile[256];
	char m_EleFile[256];
	int m_NumberOfNodes;
	int m_NumberOfElements;
	int m_NumberOfElementTimePoints;
	int m_FileType;
	int m_TimeUnits;
	bool m_LayeredMesh;
	int *m_Incidence;
// m_MeshInfo[0] is set to 2 for 2D models and 3 for 3D models
// m_MeshInfo[1] indicates the type of mesh.
//        IRREGULAR MESH      ==>   m_MeshInfo[1] = 0  
//        LAYERED MESH ACROSS ==>   m_MeshInfo[1] = 1  
//        REGULAR MESH        ==>   m_MeshInfo[1] = 2  
//        BLOCKWISE MESH      ==>   m_MeshInfo[1] = 3  
//        LAYERED MESH WITHIN ==>   m_MeshInfo[1] = 4  
// m_MeshInfo[2] is set to the number of node layers if the mesh is a layered mesh.
// Otherwise it is set to 0.
	int m_MeshInfo[3];
	int m_NumberOfNodalScalarDataTypes;
	float m_version;
	int m_GridType;
	bool m_IceSatFraction;

	static char *ReadNodFile(ifstream &in, int *sdim, int &numNodes, int &numElements,
			int &numTimePoints, bool &IceSatFraction, char **&timePointLabels, int &numScalarDataTypes,
			char **&dataTypeLabels, float *&sgCoord, int &TimeUnits);

	static char *ReadEleFile(ifstream &in, int *sdim, int &numTimePoints, 
			char **timePointLabels, float *vgCoord, bool &inputread, 
			int &SteadyFlow, int &SteadyTransport, int &ElementTimePoints, int &numElements, int &TimeUnits);

	char *LoadBinaryFile();
	virtual void ReleaseMemory();

};

#endif