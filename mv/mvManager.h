#ifndef __mvManager_h
#define __mvManager_h

#include "mvDataSource.h"
#include <Math.h>
#include <typeinfo>

class mvCustomAppendPolyData;
class mvGUISettings;
class mvColorBandFilter;
class mvExtractGrid;
class mvPathlines;
class mvGridLines;
class mvMeshLines;
class mvExternalMesh;
class mvExternalMeshVector;
class mvGridOutline;
class mvGridShell;
class mvBoundingBox;
class mvAxes;
class mvModelFeatures;
class mvColorBar;
class mvDisplayText;
class mvParticles;
class mvOverlay;
class mvClipParticles;

class vtkActor;
class vtkCellArray;
class vtkClipPolyData;
class vtkContourFilter;
class vtkCubeSource;
class vtkCutter;
class vtkDataSetMapper;
class vtkExtractGrid;
class vtkGeometryFilter;
class vtkGlyph3D;
class vtkHedgeHog;
class vtkLookupTable;
class vtkLogLookupTable;
class vtkPlane;
class vtkPoints;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkPropCollection;
class vtkFloatArray;
class vtkStructuredGrid;
class vtkThreshold;
class vtkThresholdPoints;
class vtkPointSet;
class vtkMaskPoints;
class vtkExtractGeometry;

#define MV_VERSION 1.6

#define MV_SMOOTH 0
#define MV_BLOCKY 1
#define MV_BANDED 2

#define MV_TUBE 0
#define MV_LINE 1

class MV_EXPORT mvManager
{
public:
	mvManager();
	virtual ~mvManager();

	// Data set and animation control
	void ClearData();
	char *LoadData(char *modelType, char *dataFileList);
	char *GetDataFileList() const;
	int HasVectorData() const;
	int HasPathlineData() const;
	void AssumeAllCellsAreActive(int b);
	int AreAllCellsActive() const;
	int GetGridType() const;
	bool GetIsStructuredGrid();
	bool GetLayeredMesh() const;
	void ApplyDefaultSettings();
	void SetImmediateModeRendering(int b);
	void SetReleaseDataFlag(int b);
	const int *GetScalarGridDimensions() const;
	const int *GetVectorGridDimensions() const;
	vtkPropCollection *GetPropCollection() const {return m_PropCollection;}
	int GetNumberOfTimePoints() const;
	char **GetTimePointLabels();
	int GetNumberOfScalarDataTypes() const;
	char **GetDataTypeLabels() const;
	int GetCurrentTimePointIndex() const {return m_TimePointIndex;}
	void SetScalarDataTypeTo(int dataTypeIndex);
	int GetActiveScalarDataType() const {return m_ActiveDataType;}
	char *GetActiveScalarDataName() const;
	int GetPrimaryScalarMode() const;
	char *GetModelName() const;
	int GetTimeLabelOption() const;
	void SetTimePointTo(int timePointIndex);
	void AdvanceOneTimePoint();
	int GetInitialDisplayTimePoint();
	char *GetWarningMessage() {return m_WarningMessage;}
	void ClearWarningMessage() {m_WarningMessage[0] = '\0';}

	// Scalar data display
	void HideScalarData();
	void ShowScalarDataAsSolid();
	void ShowScalarDataAsIsosurfaces();
	int IsSolidVisible() const;
	int AreIsosurfacesVisible() const;
	void SetNumberOfColorBands(int numberOfColorBands);
	int GetNumberOfColorBands() const;

	// Scalar Sub grid
	void SetScalarSubgridExtent(int imin, int imax, int jmin, int jmax, int kmin, int kmax);
	void ScalarSubgridOn();
	void ScalarSubgridOff();
	int IsScalarSubgridOn() const;
	const int *GetScalarSubgridExtent();

	// Vectors
	void ShowVectors();
	void HideVectors();
	int AreVectorsVisible() const;
	void SetVectorColor(float red, float green, float blue);
	const float *GetVectorColor() const;
	void SetVectorScaleFactor(float scaleFactor);
	float GetVectorScaleFactor() const;
	float GetVectorLineWidth();
	void SetVectorLineWidth(float width);
	
	float ComputeOptimalVectorSize();
	void SetVectorSizeToOptimal();
	void SubsampleVectors(int imin, int imax, int jmin, int jmax,
				int kmin, int kmax, int irate, int jrate, int krate);
	const int *GetVectorSubsampleExtents() const;
	const int *GetVectorSubsampleRate() const;
	void VectorThresholdOn();
	void VectorThresholdOff();
	int IsVectorThresholdOn() const;
	void SetVectorThresholdLimits(float minValue, float maxValue);
	void GetVectorThresholdLimits(float *limits) const;
	int GetLogTransformVector() {return m_VectorLog10Transform;};
	void SetLogTransformVector(int Value);
	void CropVectors(float xmin, float xmax, float ymin, 
		float ymax, float zmin, float zmax, int cropAngle);
	int GetVectorCroppingAngle() {return m_VectorClippingAngle;};
	const float *GetVectorCropBounds() const {return m_VectorBounds;}


	// Vector Glyph (The small cube at the base of the vector)
	void ActivateVectorGlyph(int active);
	int IsVectorGlyphActivated() const;
	void EnlargeVectorGlyph();
	void ShrinkVectorGlyph();

	// Pathlines
	void ShowPathlines();
	void HidePathlines();
	int ArePathlinesVisible() const;
	void SetPathlineRepresentationToLine();
	void SetPathlineRepresentationToTube();
	int GetPathlineRepresentation() const;
	void SetPathlineTubeDiameter(float diameter);
	float GetPathlineTubeDiameter() const;
	void SetPathlineTimeClippingMode(int mode);
	int GetPathlineTimeClippingMode() const {return m_PathlineTimeClippingMode;}
	void SetPathlineTimeClippingRange(float minTime, float maxTime);
	void SetPathlineColorBarEndPoints(float valueBlue, float valueRed);
	float GetPathlineClipTimeMin() const;
	float GetPathlineClipTimeMax() const;
	float GetPathlineTimeBlue() const;
	float GetPathlineTimeRed() const;
	void GetPathlineTimeRange(float *range) const;
	void UpdatePathlineScalars();
	int GetPathlineLogTransform() const;
	void SetPathlineLogTransform(int Value);

	// ModelFeatures
	void ShowModelFeatures();
	void HideModelFeatures();
	int AreModelFeaturesVisible() const;
	int *GetModelFeatureDisplayOrder();
	void SetModelFeatureDisplayOrder(int *displayOrder);
	int HasModelFeatures() const;
	int GetNumberOfModelFeatureTypes() const;
	const char *GetModelFeatureLabels() const;
	void EnlargeModelFeatureGlyphs();
	void ShrinkModelFeatureGlyphs();
	int GetModelFeatureDisplayMode() const;
	void SetModelFeatureColor(char *modelFeatureName, float *rgba);
	void GetModelFeatureColor(char *modelFeatureName, float *rgba);

	// Particles
	void ShowParticles();
	void HideParticles();
	int AreParticlesVisible() const;
	int HasParticles() const;
	void EnlargeParticleGlyphs();
	void ShrinkParticleGlyphs();
	void FilterParticles(float concMin, float concMax);
	float GetMaxDisplayedParticleConcentration() const;
	float GetMinDisplayedParticleConcentration() const;
	float GetMaxParticleConcentration() const;
	float GetMinParticleConcentration() const;
	void CropParticles(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax);

	float GetParticleClippingXMin() {return m_ParticleClippingXMin;}
	float GetParticleClippingXMax() {return m_ParticleClippingXMax;}
	float GetParticleClippingYMin() {return m_ParticleClippingYMin;}
	float GetParticleClippingYMax() {return m_ParticleClippingYMax;}
	float GetParticleClippingZMin() {return m_ParticleClippingZMin;}
	float GetParticleClippingZMax() {return m_ParticleClippingZMax;}


	// Overlay
	int HasOverlay();
	void ShowOverlay();
	void HideOverlay();
	int IsOverlayVisible() const;
	void ClearOverlayData();
	void SetOverlayFileName(char *filename);
	void SetOverlayType(int type);
	void SetOverlayCoordinatesAtGridOrigin(double xorig, double yorig);
	void SetOverlayElevation(double elev);
	void SetOverlayAngle(double angle);
	void SetOverlayToGridScale(double scale);
	void SetOverlayDrape(int b);
	void SetOverlayDrapeGap(double gap);
	void SetOverlayTrim(int b);
	void SetOverlayCrop(int b);
	char *GetOverlayFileName();
	int GetOverlayType();
	void GetOverlayCoordinatesAtGridOrigin(double &xorig, double &yorig);
	double GetOverlayElevation();
	double GetOverlayAngle();
	double GetOverlayToGridScale();
	int GetOverlayDrape();
	double GetOverlayDrapeGap();
	int GetOverlayTrim();
	int GetOverlayCrop();
	void GetOverlayBounds(double &xmin, double &xmax, double &ymin, double &ymax);
	int UpdateOverlay(char *errMsg);
	void RemoveOverlay();

	// Grid shell
	int IsGridShellVisible() const;
	void ShowGridShell();
	void HideGridShell();
	void SetGridShellColor(float red, float green, float blue);
	void SetGridShellOpacity(float opacity);
	const float *GetGridShellColor() const;
	float GetGridShellOpacity() const;

	// Grid lines
	int AreActivatedGridLinesVisible() const;
	void ShowActivatedGridLines();
	void HideGridLines();
	void ActivateGridLines(int i);
	void DeactivateGridLines(int i);
	void SetGridLinePositions(int posX, int posY, int posZ);
	void SetGridLineColor(float r, float g, float b);
	int AreGridLinesActive(int i) const;
	void GetGridLinePositions(int *ibuff) const;
	const float *GetGridLineColor() const;
	void ActivateGridOutline();
	void DeactivateGridOutline();
	int IsGridOutlineActive() const;

	// Mesh lines
	int AreMeshLinesVisible() const;
	void ShowMeshLines();
	void HideMeshLines();
	int GetMeshOutlineChoice() {return m_MeshOutlineChoice;}
	void SetMeshOutlineChoice(int i);
	void AddExternalMeshActors();

	// Bounding Box
	int IsBoundingBoxVisible() const;
	void ShowBoundingBox();
	void HideBoundingBox();
	void SetBoundingBoxColor(float r, float g, float b);
	const float *GetBoundingBoxColor() const;
	void SetBoundingBoxBounds();

	// Axes
	int AreAxesVisible() const;
	void ShowAxes();
	void HideAxes();
	float GetDefaultAxesSize();
	float GetDefaultAxesTubeDiameter();
	void SetAxesNormalizedSize(float size);
	void SetAxesNormalizedPosition(float x, float y, float z);
	void SetAxesNormalizedTubeDiameter(float d);
	float GetAxesNormalizedSize() const;
	float GetAxesNormalizedTubeDiameter() const;
	const float *GetAxesNormalizedPosition() const;
	void SetAxesRepresentationToLine();
	void SetAxesRepresentationToTube();
	int GetAxesRepresentation() const;

	bool GetCanLogTransformXAxis();
	bool GetCanLogTransformYAxis();
	bool GetCanLogTransformZAxis();
	bool GetLogTransformXAxis();
	bool GetLogTransformYAxis();
	bool GetLogTransformZAxis();
	void SetLogTransformXAxis(bool transform);
	void SetLogTransformYAxis(bool transform);
	void SetLogTransformZAxis(bool transform);
	char *XAxisLabel();
	char *YAxisLabel();
	char *ZAxisLabel();

	// Time Label
	int IsTimeLabelVisible() const;
	void ShowTimeLabel();
	void HideTimeLabel();
	void SetTimeLabelPosition(float x, float y);
	const float *GetTimeLabelPosition() const;
	void SetTimeLabelFontSize(int size);
	int GetTimeLabelFontSize() const;
	void SetTimeLabelColor(float r, float g, float b);
	const float *GetTimeLabelColor() const;

	// Title Label
	int IsTitleVisible() const;
	void ShowTitle();
	void HideTitle();
	void SetTitlePosition(float x, float y);
	const float *GetTitlePosition() const;
	void SetTitleFontSize(int size);
	int GetTitleFontSize() const;
	void SetTitleColor(float r, float g, float b);
	const float *GetTitleColor() const;

	// Color Bar
	void SetColorBarEndPoints(float valueBlue, float valueRed);
	int IsColorBarVisible() const;
	void ShowColorBar();
	void HideColorBar();
	float GetColorBarValueBlue() const;
	float GetColorBarValueRed() const;
	int GetColorBarColorScheme() const;
	void UseLinearColorBar();
	void UseLogColorBar();
	int IsColorBarLinear() const;
	bool IsColorBarNormal() const;
	void SetColorBarWidth(int w);
	void SetColorBarHeight(int h);
	void SetColorBarFontSize(int f);
	void SetColorBarOffset(int r);
	void SetColorBarTextColor(float r, float g, float b);
	void SetColorBarNumberOfLabels(int n);
	void SetColorBarLabelPrecision(int d);
	void SetColorBarColorScheme(int value);
	int GetColorBarWidth() const;
	int GetColorBarHeight() const;
	int GetColorBarFontSize() const;
	int GetColorBarOffset() const;
	const float *GetColorBarTextColor() const;
	int GetColorBarNumberOfLabels() const;
	int GetColorBarLabelPrecision() const;
	unsigned long GetColorBarFirstCustomColor() const;
	unsigned long GetColorBarLastCustomColor() const;
	void SetColorBarFirstCustomColor(unsigned long value);
	void SetColorBarLastCustomColor(unsigned long value);
	int GetColorBarSource() const;
	void SetColorBarSource(int value);

	// Lighting
	void SetDiffuseLighting(float diffuse);
	void SetAmbientLighting(float ambient);
	void SetSpecularLighting(float specular);
	void SetSpecularPower(float specularPower);
	float GetDiffuseLighting() const;
	float GetAmbientLighting() const;
	float GetSpecularLighting() const;
	float GetSpecularPower() const;

	// X-Y-Z Scaling
	void SetScale(float xScale, float yScale, float zScale);
	const float *GetScale() const;

	// Data Range
	void GetScalarDataRange(float *range) const;
	void GetVectorMagnitudeRange(float *range) const;

	// Solid Control
	void SetSolidDisplayToBlocky();
	void SetSolidDisplayToSmooth();
	void SetSolidDisplayToBanded();
	void SolidThresholdOn();
	void SolidThresholdOff();
	int IsSolidThresholdOn() const;
	void SetSolidThresholdLimits(float minValue, float maxValue);
	int GetSolidDisplayMode() const;
	void GetSolidThresholdLimits(float *limits) const;

	// Isosurface Control
	void SetRegularIsosurfaces(int numIsosurface, float minValue, float maxValue);
	void SetCustomIsosurfaces(int count, const float *values);
	int GetNumberOfRegularIsosurfaces() const;
	int GetNumberOfCustomIsosurfaces() const;
	void GetRegularIsosurfaceRange(float *range) const;
	const float *GetCustomIsosurfaceValues() const;
	int UsingRegularIsosurfaces() const;

	// Cropping
	void Crop(float xmin, float xMax, float yMin, float yMax, float zMin, float zMax, float cropAngle);
	const float *GetCropBounds() const {return m_CropBounds;}
	float GetHorizontalCropAngle() const;
	void ShowCroppedAwayPieces();
	void HideCroppedAwayPieces();
	int AreCroppedAwayPiecesShown() const;
	void SetCroppedAwayPiecesColor(float red, float green, float blue);
	void SetCroppedAwayPiecesOpacity(float opacity);
	const float *GetCroppedAwayPiecesColor() const;
	float GetCroppedAwayPiecesOpacity() const;

	// Serialization
	char *Serialize(const char *fileName, mvGUISettings *gui) const;
	char *Deserialize(const char *fileName, mvGUISettings *gui);

protected:
	int m_MeshOutlineChoice;
	int m_TimePointIndex;	// used to keep track of time point when animating
	int m_ActivatedGridLinesVisibility;	
	int m_GridLinesActivated[3];
	int m_MeshActivated;
	int m_GridOutlineActivated;
	float m_ActiveScalarRange[2];	// need this to avoid scalar value for inactive cells
	float m_CropBounds[6];	// These boundaries are relative wrt the bounding box
	float m_CropAngle;
	float *m_ScaledVectorArray;
	float *m_VectorMagnitudeArray;
	float m_VectorMagnitudeRange[2];
	float *m_VectorLogMagnitudeArray;
	float m_VectorLogMagnitudeRange[2];
	float m_MinPositiveVector;
	int m_VectorLog10Transform;
	int m_DoVectorThreshold;
	int m_ShowCroppedAwayPieces;
	int m_PathlineTimeClippingMode;
	float m_PathlineClipTimeMin;
	float m_PathlineClipTimeMax;
	int m_VectorGlyphActivated;
	float m_VectorBounds[6];

	int m_NumScalarDataTypes;

	float *m_ColorBarValueBlue;
	float *m_ColorBarValueRed;
	int *m_UseLogColorBar;
	int *m_NumColorBarLabels;
	int *m_ColorBarLabelPrecision;
	int	m_ColorBarDataSource;

	int *m_SolidDisplayMode;
	int *m_DoSolidThreshold;
	float *m_SolidThresholdMax;
	float *m_SolidThresholdMin;
	int *m_NumberOfColorBands;

	int *m_UseRegularIsosurface;
	int *m_NumberOfRegularIsosurfaces;
	float *m_RegularIsosurfaceMax;
	float *m_RegularIsosurfaceMin;
	int *m_NumberOfCustomIsosurfaces;
	float **m_CustomIsosurfaceValues;

	int m_ActiveDataType;

	float m_ParticleClippingXMin;
	float m_ParticleClippingXMax;
	float m_ParticleClippingYMin;
	float m_ParticleClippingYMax;
	float m_ParticleClippingZMin;
	float m_ParticleClippingZMax;

	float m_VectorClippingXMin;
	float m_VectorClippingXMax;
	float m_VectorClippingYMin;
	float m_VectorClippingYMax;
	float m_VectorClippingZMin;
	float m_VectorClippingZMax;
	int m_VectorClippingAngle;

	char m_WarningMessage[500];
	float m_version;

	// Base class pointer to the data source
	mvDataSource *m_DataSource;

	// Scalar Data Set
	// m_ScalarDataSet will be either a vtkStructuredGrid or a vtkUnstructuredGrid.
	// Which one will be determined in AfterNewDataSource depending on whether the 
	// grid is structured or unstructured.  The only case where a grid is unstructured
	// is an Irregular or Layered mesh in SUTRA.
	vtkPointSet *m_ScalarDataSet;
	vtkPoints *m_ScalarGridPoints;
	vtkFloatArray *m_PointScalars;
	vtkFloatArray *m_CellScalars;

	// Vector Data Set
	vtkPointSet *m_VectorDataSet;
	vtkPoints *m_VectorGridPoints;
	vtkFloatArray *m_Vectors;
	vtkFloatArray *m_VectorMagnitudes;

	// Pathline Data Set
	vtkPolyData *m_PathlineDataSet;
	vtkPoints *m_PathlinePoints;
	vtkFloatArray *m_PathlineScalars;
	vtkCellArray *m_PathlineLines;

	// Mappers
	vtkPolyDataMapper *m_SolidMapper;
	vtkPolyDataMapper *m_CroppedAwayPiecesMapper;
	vtkPolyDataMapper *m_IsosurfaceMapper;
	vtkPolyDataMapper *m_VectorMapper;
	vtkPolyDataMapper *m_VectorGlyphMapper;

	// Lookup tables
	vtkLookupTable *m_LutRedToBlue;
	vtkLookupTable *m_LutBlueToRed;
	vtkLookupTable *m_LutModifiedRedToBlue;
	vtkLookupTable *m_LutModifiedBlueToRed;
	vtkLookupTable *m_LutCustomScale;
	vtkLookupTable *m_LutReversedCustomScale;

	vtkLogLookupTable *m_LogLutRedToBlue;
	vtkLogLookupTable *m_LogLutBlueToRed;
	vtkLogLookupTable *m_LogLutModifiedRedToBlue;
	vtkLogLookupTable *m_LogLutModifiedBlueToRed;
	vtkLogLookupTable *m_LogLutCustomScale;
	vtkLogLookupTable *m_LogLutReversedCustomScale;

	// Actors
	vtkActor *m_SolidActor;
	vtkActor *m_CroppedAwayPiecesActor;
	vtkActor *m_IsosurfaceActor;
	vtkActor *m_VectorActor;
	vtkActor *m_VectorGlyphActor;
	vtkPropCollection *m_PropCollection;

	// Filters to create grid lines, bounding box, full solid, and isosurfaces
	vtkExtractGrid *m_ScalarSubDataSet;
	vtkThreshold *m_ActiveScalarDataSet;
	vtkContourFilter *m_Isosurface;

	// Filter to create Color bands
	mvColorBandFilter *m_ColorBandFilter;

	// Filters to create the smooth solid
	vtkClipPolyData *m_GridShellClipMin;
	vtkClipPolyData *m_GridShellClipMax;
	vtkContourFilter *m_SmoothSolidIsosurface;
	mvCustomAppendPolyData *m_SmoothSolid;

	// Filters to create the solid cuttings
	mvCustomAppendPolyData *m_CroppedAwayPieces;

	// Filters to create the blocky solid
	vtkThreshold *m_BlockySolidThreshold;
	vtkGeometryFilter *m_BlockySolid;

	// Filters for cropping 
	vtkPlane *m_Plane[6];
	vtkClipPolyData *m_Cropper[6];

	// Cropped Solid
	vtkCutter *m_ExtractFace[6];
	vtkClipPolyData *m_FaceCrop[24];
	mvCustomAppendPolyData *m_Faces;
	vtkClipPolyData *m_FacesClipMin;
	vtkClipPolyData *m_FacesClipMax;
	mvCustomAppendPolyData *m_CroppedSolid;
	vtkThreshold *m_FacesThreshold;
	vtkGeometryFilter *m_FacesThresholdGeometry;

	// Cropped Isosurface
	vtkCutter *m_IsosurfaceCutter[3];

	// Filter to create vector lines
	mvExtractGrid *m_ExtractVector;
	vtkMaskPoints *m_ExtractIrregularMeshVector;
	vtkExtractGeometry *m_CropVectors;
	mvClipParticles *m_ClipVectors;

	vtkThresholdPoints *m_ActiveVectorDataSet;
	vtkThresholdPoints *m_VectorThreshold;
	vtkHedgeHog *m_HedgeHog;
	vtkCubeSource *m_CubeSource;
	vtkGlyph3D *m_VectorGlyph;

	mvPathlines *m_Pathlines;
	mvGridLines *m_GridLines[3];
	mvMeshLines *m_MeshLines;
	mvExternalMeshVector *m_ExternalMeshVector;
	mvExternalMeshVector *m_ExternalLinesVector;
	bool m_ExternalMeshActorsAdded;

	mvGridOutline *m_GridOutline;
	mvGridShell *m_GridShell;
	mvBoundingBox *m_BoundingBox;
	mvAxes *m_Axes;
	mvModelFeatures *m_ModelFeatures;
	mvColorBar *m_ColorBar;
	mvDisplayText *m_TimeLabel;
	mvDisplayText *m_Title;

	mvParticles *m_Particles;
	mvOverlay *m_Overlay;

	// Protected methods
	void ComputeActiveScalarRange();
	void ComputeVectorMagnitudes();
	void OnDataModified();
	void UpdateScaledVectorArray();
	void UpdateColorBands();
	void UpdateCrop();
	void BuildPipelineForSolid();
	void BuildPipelineForIsosurface();
	void ReleaseArrayMemory();
	void ResetParticleCropping();
	void AfterNewDataSource();
	void UpdateLogTransformedData();
};

#endif
