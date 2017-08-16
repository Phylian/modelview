// MvDoc.h : interface of the CMvDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MVDOC_H__D3A07DDA_F3BF_11D3_8105_00C04F61038F__INCLUDED_)
#define AFX_MVDOC_H__D3A07DDA_F3BF_11D3_8105_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class mvManager;
class mvGUISettings;
class vtkPropCollection;

class CDataDlg;
class CColorBarDlg;
class CLightingDlg;
class CGridDlg;
class CGeometryDlg;
class CSolidDlg;
class CIsosurfaceDlg;
class CVectorDlg;
class CPathlinesDlg;
class CModelFeaturesDlg;
class CCropDlg;
class CAnimationDlg;
class CParticlesDlg;
class COverlayDlg;
class CAxesDlg;

enum AnimationType {atTime, atSpace};

class CMvDoc : public CDocument
{
protected: // create from serialization only
	CMvDoc();
	DECLARE_DYNCREATE(CMvDoc)

// Attributes
public:

// Operations
public:

	// Methods call by CMvView
	void CreateToolDialogs();
	vtkPropCollection *GetPropCollection();
	int GetInteractorStyle() {return m_InteractorStyle;}
	BOOL IsAnimating() {return m_IsAnimating;}

	// Get methods
	char **GetTimePointLabels();
	int GetNumberOfTimePoints();
	void GetScalarDataRange(float *range);
	void GetVectorMagnitudeRange(float *range);
	char *GetModelName();
	char *GetDataName();

	// Callback functions for modeless dialog boxes (tools)

	// Data
	void SetScalarDataTypeTo(int index);

	// Time Label
	void SetTimeLabelFontSize(int size, BOOL update = TRUE);
	int GetTimeLabelFontSize() const;
	void SetTimeLabelPosition(float x, float y, BOOL update = TRUE);
	const float *GetTimeLabelPosition() const;

	// Color Bar
	void SetColorBarEndPoints(float valueBlue, float valueRed);
	void UseLinearColorBar();
	void UseLogColorBar();
	void SetColorBarSize(int width, int height, int offset, BOOL update = TRUE);
	void SetColorBarFontSize(int fontSize, BOOL update = TRUE);
	void SetColorBarNumberOfLabels(int numLabels, BOOL update = TRUE);
	void SetColorBarLabelPrecision(int precision, BOOL update = TRUE);
	void SetColorBarTextColor(float red, float green, float blue, BOOL update = TRUE);
	void SetColorBarColorScheme(int Value);
	int GetColorBarWidth();
	int GetColorBarHeight();
	int GetColorBarOffset();
	int GetColorBarFontSize();
	int GetColorBarColorScheme();
	unsigned long GetColorBarFirstCustomColor();
	unsigned long GetColorBarLastCustomColor();
	void SetColorBarFirstCustomColor(unsigned long value);
	void SetColorBarLastCustomColor(unsigned long value);
	int GetColorBarSource();
	void SetColorBarSource(int value);
	float GetColorBarValueBlue() const;
	float GetColorBarValueRed() const;

	// Lighting
	void SetDiffuseLighting(float diffuse);
	void SetAmbientLighting(float ambient);
	void SetSpecularLighting(float specular);
	void SetSpecularPower(float specularPower);
	void SwitchOnHeadlight(BOOL switchOn);
	void SetHeadlightIntensity(float intensity);
	void SwitchOnAuxiliaryLight(BOOL switchOn);
	void SetAuxiliaryLightIntensity(float intensity);
	void SetAuxiliaryLightPosition(float x, float y, float z);
	void SetBackgroundColor(float red, float green, float blue);

	// Grid
	void ActivateGridLines(int slice, BOOL b);
	void SetGridLinePositions(int posX, int posY, int posZ);
	void SetGridLineColor(float red, float green, float blue);
	void SetGridShellColor(float red, float green, float blue);
	void SetGridShellOpacity(float opacity);
	void ActivateGridOutline(BOOL b);

	// Mesh
	void SetMeshOutlineChoice(int i);

	// Geometry
	void SetScale(float xScale, float yScale, float zScale);
	void SetAxesRepresentationToLine();
	void SetAxesRepresentationToTube();
	void SetAxesProperties(float xPos, float yPos, float zPos, 
					float axesSize, float tubeDiameter);
	void SetBoundingBoxColor(float red, float green, float blue);
	void ApplySubgrid(int imin, int imax, int jmin, int jmax, int kmin, int kmax);
	void SubgridOff();

	// Solid
	void SetSolidDisplayToBlocky();
	void SetSolidDisplayToSmooth();
	void SetSolidDisplayToBanded();
	void ApplySolidControl(BOOL threshold, float minValue, float maxValue, int numberOfColorBands);

	// Isosurfaces
	void SetRegularIsosurfaces(int count, float valueMin, float valueMax);
	void SetCustomIsosurfaces(int count, float *values);

	// Vector
	void SetVectorScaleFactor(float scaleFactor);
	float GetVectorScaleFactor();
	void SetVectorSizeToOptimal();
	void SubsampleVectors(int imin, int imax, int jmin, int jmax, 
			int kmin, int kmax, int irate, int jrate, int krate);
	void SetVectorColor(float red, float green, float blue);
	void ActivateVectorGlyph(BOOL b);
	void EnlargeVectorGlyph();
	void ShrinkVectorGlyph();
	void ApplyVectorThreshold(float minValue, float maxValue);
	void VectorThresholdOff();
	int GetLogTransformVector();
	void SetLogTransformVector(int Value);
	void SetVectorLineWidth(float width);

	// Particle pathlines
	void SetPathlineRepresentationToLine();
	void SetPathlineRepresentationToTube();
	void SetPathlineTubeDiameter(float diameter);
	void SetPathlineColorBarEndPoints(float valueBlue, float valueRed);
	void SetPathlineTimeClippingMode(int mode);
	void SetPathlineTimeClippingRange(float minTime, float maxTime);
	void GetPathlineTimeRange(float *range);
	void SetPathlineLogTransform(int Value);
	int HasPathlineData() const;

	// Cropping
	void Crop(float xmin, float xMax, float yMin, float yMax, 
				float zMin, float zMax, float cropAngle);
	void ShowCroppedAwayPieces();
	void HideCroppedAwayPieces();
	void SetCroppedAwayPiecesColor(float red, float green, float blue);
	void SetCroppedAwayPiecesOpacity(float opacity);

	// Animation
	void StartAnimation(float delay);
	void Animate();
	void AdvanceOneTimePoint();
	void SetTimePointTo(int timePointIndex);
	void StopAnimation();
	void UpdateAnimationWithSameTime();
	void SetAnimationType(AnimationType value);
	void SetAnimationSteps(int value);

	// Model Feature
	void SetModelFeatureDisplayOrder(int *displayOrder);
	void EnlargeModelFeatureGlyphs();
	void ShrinkModelFeatureGlyphs();
	void SetModelFeatureColor(char *featureName, float *rgba);
	void GetModelFeatureColor(char *featureName, float *rgba);

	// Particles
	void EnlargeParticleGlyphs();
	void ShrinkParticleGlyphs();
	void FilterParticles(float concMin, float concMax);
	float GetMaxDisplayedParticleConcentration() const;
	float GetMinDisplayedParticleConcentration() const;
	float GetMaxParticleConcentration() const;
	float GetMinParticleConcentration() const;
	void CropParticles(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax);
	float GetParticleClippingXMin();
	float GetParticleClippingXMax();
	float GetParticleClippingYMin();
	float GetParticleClippingYMax();
	float GetParticleClippingZMin();
	float GetParticleClippingZMax();

	// Overlay
	void ApplyOverlayControl(char *filename, int overlayType, double xorig, double yorig,
		double scale, double angle, int drape, int trim, int crop, double elev, double drapeGap);
	void RemoveOverlay();
	int HasOverlay();

	// Axes

	bool GetCanLogTransformXAxis();
	bool GetCanLogTransformYAxis();
	bool GetCanLogTransformZAxis();
	bool GetLogTransformXAxis();
	bool GetLogTransformYAxis();
	bool GetLogTransformZAxis();
	void SetLogTransformXAxis(BOOL transform);
	void SetLogTransformYAxis(BOOL transform);
	void SetLogTransformZAxis(BOOL transform);
	char *XAxisLabel();
	char *YAxisLabel();
	char *ZAxisLabel();

	void PrepareToClose();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMvDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	//}}AFX_VIRTUAL

// Implementation
public:
	int GetAnimationSteps();
	AnimationType GetAnimationType();
	void CropVectors(float xmin, float xmax, 
		float ymin, float ymax, float zmin, float zmax, int cropangle);
	void UpdateColorBarDlg();
	void UpdatePathlinesDlg();
	virtual ~CMvDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

	AnimationType m_AnimationType;
	int m_AnimationSteps;
	int m_InteractorStyle;
	int m_ProjectionMode;
	int m_NumberOfModels;
	int m_ReadyToClose;
	char **m_ModelNames;
	CString m_DefaultModel;
	BOOL m_Startup;
	BOOL m_IsAnimating;
	mvGUISettings *m_GUI;

	// The visualization pipeline manager
	mvManager *m_Manager;

	// Modeless dialog boxes
	CGridDlg *m_GridDlg;
	CColorBarDlg *m_ColorBarDlg;
	CLightingDlg *m_LightingDlg;
	CGeometryDlg *m_GeometryDlg;
	CDataDlg *m_DataDlg;
	CSolidDlg *m_SolidDlg;
	CIsosurfaceDlg *m_IsosurfaceDlg;
	CCropDlg *m_CropDlg;
	CAnimationDlg *m_AnimationDlg;
	CVectorDlg *m_VectorDlg;
	CPathlinesDlg *m_PathlinesDlg;
	CModelFeaturesDlg *m_ModelFeaturesDlg;
	CParticlesDlg *m_ParticleDlg;
	COverlayDlg *m_OverlayDlg;
	CAxesDlg *m_AxesDlg;

	// Protected methods;
	void LoadPreviousAppSettings();
	void SaveCurrentAppSettings();
	void UpdateToolDialogs(mvGUISettings *gui);
	void UpdateSolidDlg();
	void UpdateIsosurfaceDlg();
	void UpdateLightingDlg(mvGUISettings *gui);
	void UpdateGeometryDlg();
	void UpdateGridDlg();
	void UpdateVectorDlg();
	void UpdateCropDlg(mvGUISettings *gui);
	void UpdateDataDlg();
	void UpdateModelFeaturesDlg();
	void UpdateAnimationDlg(mvGUISettings *gui);
	void UpdateParticlesDlg(mvGUISettings *gui);
	void UpdateOverlayDlg();
	void UpdateAnimation();
	void UpdateAxesDlg();
	void UpdateAnimationPosition();

// Generated message map functions
protected:
	//{{AFX_MSG(CMvDoc)
	afx_msg void OnUpdateFileSave(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileSaveAs(CCmdUI* pCmdUI);
	afx_msg void OnParallelProjection();
	afx_msg void OnUpdateParallelProjection(CCmdUI* pCmdUI);
	afx_msg void OnPerspectiveProjection();
	afx_msg void OnUpdatePerspectiveProjection(CCmdUI* pCmdUI);
	afx_msg void OnPreferences();
	afx_msg void OnShowBoundingBox();
	afx_msg void OnUpdateShowBoundingBox(CCmdUI* pCmdUI);
	afx_msg void OnShowGridLines();
	afx_msg void OnUpdateShowGridLines(CCmdUI* pCmdUI);
	afx_msg void OnShowGridShell();
	afx_msg void OnUpdateShowGridShell(CCmdUI* pCmdUI);
	afx_msg void OnShowIsosurfaces();
	afx_msg void OnUpdateShowIsosurfaces(CCmdUI* pCmdUI);
	afx_msg void OnShowNone();
	afx_msg void OnUpdateShowNone(CCmdUI* pCmdUI);
	afx_msg void OnShowSolid();
	afx_msg void OnUpdateShowSolid(CCmdUI* pCmdUI);
	afx_msg void OnShowTime();
	afx_msg void OnUpdateShowTime(CCmdUI* pCmdUI);
	afx_msg void OnShowTitle();
	afx_msg void OnUpdateShowTitle(CCmdUI* pCmdUI);
	afx_msg void OnShowAxes();
	afx_msg void OnUpdateShowAxes(CCmdUI* pCmdUI);
	afx_msg void OnLightingTool();
	afx_msg void OnUpdateLightingTool(CCmdUI* pCmdUI);
	afx_msg void OnGridTool();
	afx_msg void OnUpdateGridTool(CCmdUI* pCmdUI);
	afx_msg void OnGeometryTool();
	afx_msg void OnUpdateGeometryTool(CCmdUI* pCmdUI);
	afx_msg void OnSolidTool();
	afx_msg void OnUpdateSolidTool(CCmdUI* pCmdUI);
	afx_msg void OnIsosurfaceTool();
	afx_msg void OnUpdateIsosurfaceTool(CCmdUI* pCmdUI);
	afx_msg void OnCropTool();
	afx_msg void OnUpdateCropTool(CCmdUI* pCmdUI);
	afx_msg void OnAnimationTool();
	afx_msg void OnUpdateAnimationTool(CCmdUI* pCmdUI);
	afx_msg void OnColorBarTool();
	afx_msg void OnUpdateColorBarTool(CCmdUI* pCmdUI);
	afx_msg void OnShowColorBar();
	afx_msg void OnUpdateShowColorBar(CCmdUI* pCmdUI);
	afx_msg void OnLoadData();
	afx_msg void OnShowVectors();
	afx_msg void OnUpdateShowVectors(CCmdUI* pCmdUI);
	afx_msg void OnVectorTool();
	afx_msg void OnUpdateVectorTool(CCmdUI* pCmdUI);
	afx_msg void OnDataTool();
	afx_msg void OnUpdateDataTool(CCmdUI* pCmdUI);
	afx_msg void OnAllActiveCells();
	afx_msg void OnUpdateAllActiveCells(CCmdUI* pCmdUI);
	afx_msg void OnShowModelFeatures();
	afx_msg void OnUpdateShowModelFeatures(CCmdUI* pCmdUI);
	afx_msg void OnModelFeaturesTool();
	afx_msg void OnUpdateModelFeaturesTool(CCmdUI* pCmdUI);
	afx_msg void OnShowPathlines();
	afx_msg void OnUpdateShowPathlines(CCmdUI* pCmdUI);
	afx_msg void OnPathlinesTool();
	afx_msg void OnUpdatePathlinesTool(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileNew(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileOpen(CCmdUI* pCmdUI);
	afx_msg void OnUpdateCustomFileClose(CCmdUI* pCmdUI);
	afx_msg void OnShowParticle();
	afx_msg void OnUpdateShowParticle(CCmdUI* pCmdUI);
	afx_msg void OnParticlesTool();
	afx_msg void OnUpdateParticlesTool(CCmdUI* pCmdUI);
	afx_msg void OnShowOverlay();
	afx_msg void OnUpdateShowOverlay(CCmdUI* pCmdUI);
	afx_msg void OnOverlayTool();
	afx_msg void OnUpdateOverlayTool(CCmdUI* pCmdUI);
	afx_msg void OnAxesTool();
	afx_msg void OnUpdateAxesTool(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MVDOC_H__D3A07DDA_F3BF_11D3_8105_00C04F61038F__INCLUDED_)
