#ifndef __mvModelFeatures_h
#define __mvModelFeatures_h

#include "mvDisplayObject.h"

#define MV_DISPLAY_MODEL_FEATURES_AS_CELLS 1
#define MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS 2

class vtkCubeSource;
class vtkGlyph3D;
class vtkPoints;
class vtkPolyData;
class vtkStructuredGrid;
class vtkThreshold;
class vtkThresholdPoints;
class vtkTransform;
class vtkTransformPolyDataFilter;
class vtkLookupTable;

class MV_EXPORT mvModelFeatures : public mvDisplayObject  
{
public:
	mvModelFeatures();
	virtual ~mvModelFeatures();

	void SetDisplayMode(int displayMode);
	int GetDisplayMode() const;
	void SetNumberOfModelFeatureTypes(int numModelFeatureTypes);
	void SetModelFeatureArray(int *modelFeatureArray);
	void SetGridPoints(vtkPoints *gridPoints);
	void SetScale(float xScale, float yScale, float zScale);
	void Build();
	void SetDisplayOrder(int *displayOrder);
	int *GetDisplayOrder();
	void GetColor(int i, float *rgba);
	void SetColor(int i, float *rgba);
	void UpdateDisplay();
	void SetDefaultGlyphSize(float s);
	void EnlargeGlyphs();
	void ShrinkGlyphs();
	void SetFullGridDimensions(const int *dim);
	void SetSubgridExtent(int imin, int imax, int jmin, int jmax, int kmin, int kmax);
	void SubgridOn();
	void SubgridOff();
	float GetGlyphSize();
	void UpdateCells();
    void AssignConnectivity(int *types, vtkCellArray* connectivity);
	void SetGridType(int i);

protected:
	int m_DisplayMode;
	int m_NumberOfModelFeatureTypes;
	int *m_ModelFeatureArray;
	int *m_DisplayOrder;
	int m_Dim[3];
	int m_Subgrid[6];
	int m_NumberOfGlyphs;
	int m_NumberOfCells;
	vtkLookupTable *m_Lut;
	vtkPointSet *m_StructuredGrid;
	vtkThreshold *m_ThresholdCells;
	float *m_CellScalarArray;
	int m_SubgridIsActivated;

	vtkPoints *m_GridPoints;
	vtkPolyData *m_PolyData;
	vtkGlyph3D *m_Glyph;
	vtkCubeSource *m_CubeSource;
	vtkThresholdPoints *m_ThresholdPoints;
	vtkTransform *m_Transform;
	vtkTransformPolyDataFilter *m_TransformFilter;
	float m_DefaultGlyphSize;
	float *m_GlyphScalarArray;

	int m_GridType;
	int *m_CellTypes;
	vtkCellArray *m_Connectivity;

	void SetDisplayOrderForGlyphs();
	void SetDisplayOrderForCells();
};

#endif
