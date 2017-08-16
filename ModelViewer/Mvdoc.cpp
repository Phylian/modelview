// MvDoc.cpp : implementation of the CMvDoc class
//

#include "stdafx.h"
#include "ModelViewer.h"
#include <time.h>
#include <direct.h>

#include "MvDoc.h"
#include "MvView.h"

#include "mvModelList.h"
#include "mvModelFeatures.h"
#include "mvManager.h"
#include "mvGUISettings.h"

#include "ModelSelectionDlg.h"
#include "DataFilesDialog.h"
#include "DataSelectionDlg.h"
#include "PreferencesDlg.h"
#include "ColorBarDlg.h"
#include "LightingDlg.h"
#include "GridDlg.h"
#include "GeometryDlg.h"
#include "DataDlg.h"
#include "SolidDlg.h"
#include "IsosurfaceDlg.h"
#include "VectorDlg.h"
#include "CropDlg.h"
#include "AnimationDlg.h"
#include "PathlinesDlg.h"
#include "ModelFeaturesDlg.h"
#include "ParticlesDlg.h"
#include "OverlayDlg.h"
#include "AxesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define MV_PERSPECTIVE_PROJECTION 0
#define MV_PARALLEL_PROJECTION 1

UINT ControlFunction(LPVOID pParam)
{
	((CMvDoc *) pParam)->Animate();
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CMvDoc

IMPLEMENT_DYNCREATE(CMvDoc, CDocument)

BEGIN_MESSAGE_MAP(CMvDoc, CDocument)
	//{{AFX_MSG_MAP(CMvDoc)
	ON_UPDATE_COMMAND_UI(ID_FILE_SAVE, OnUpdateFileSave)
	ON_UPDATE_COMMAND_UI(ID_FILE_SAVE_AS, OnUpdateFileSaveAs)
	ON_COMMAND(ID_PARALLEL_PROJECTION, OnParallelProjection)
	ON_UPDATE_COMMAND_UI(ID_PARALLEL_PROJECTION, OnUpdateParallelProjection)
	ON_COMMAND(ID_PERSPECTIVE_PROJECTION, OnPerspectiveProjection)
	ON_UPDATE_COMMAND_UI(ID_PERSPECTIVE_PROJECTION, OnUpdatePerspectiveProjection)
	ON_COMMAND(ID_PREFERENCES, OnPreferences)
	ON_COMMAND(ID_SHOW_BOUNDING_BOX, OnShowBoundingBox)
	ON_UPDATE_COMMAND_UI(ID_SHOW_BOUNDING_BOX, OnUpdateShowBoundingBox)
	ON_COMMAND(ID_SHOW_GRID_LINES, OnShowGridLines)
	ON_UPDATE_COMMAND_UI(ID_SHOW_GRID_LINES, OnUpdateShowGridLines)
	ON_COMMAND(ID_SHOW_GRID_SHELL, OnShowGridShell)
	ON_UPDATE_COMMAND_UI(ID_SHOW_GRID_SHELL, OnUpdateShowGridShell)
	ON_COMMAND(ID_SHOW_ISOSURFACES, OnShowIsosurfaces)
	ON_UPDATE_COMMAND_UI(ID_SHOW_ISOSURFACES, OnUpdateShowIsosurfaces)
	ON_COMMAND(ID_SHOW_NONE, OnShowNone)
	ON_UPDATE_COMMAND_UI(ID_SHOW_NONE, OnUpdateShowNone)
	ON_COMMAND(ID_SHOW_SOLID, OnShowSolid)
	ON_UPDATE_COMMAND_UI(ID_SHOW_SOLID, OnUpdateShowSolid)
	ON_COMMAND(ID_SHOW_TIME, OnShowTime)
	ON_UPDATE_COMMAND_UI(ID_SHOW_TIME, OnUpdateShowTime)
	ON_COMMAND(ID_SHOW_TITLE, OnShowTitle)
	ON_UPDATE_COMMAND_UI(ID_SHOW_TITLE, OnUpdateShowTitle)
	ON_COMMAND(ID_SHOW_AXES, OnShowAxes)
	ON_UPDATE_COMMAND_UI(ID_SHOW_AXES, OnUpdateShowAxes)
	ON_COMMAND(ID_LIGHTING_TOOL, OnLightingTool)
	ON_UPDATE_COMMAND_UI(ID_LIGHTING_TOOL, OnUpdateLightingTool)
	ON_COMMAND(ID_GRID_TOOL, OnGridTool)
	ON_UPDATE_COMMAND_UI(ID_GRID_TOOL, OnUpdateGridTool)
	ON_COMMAND(ID_GEOMETRY_TOOL, OnGeometryTool)
	ON_UPDATE_COMMAND_UI(ID_GEOMETRY_TOOL, OnUpdateGeometryTool)
	ON_COMMAND(ID_SOLID_TOOL, OnSolidTool)
	ON_UPDATE_COMMAND_UI(ID_SOLID_TOOL, OnUpdateSolidTool)
	ON_COMMAND(ID_ISOSURFACE_TOOL, OnIsosurfaceTool)
	ON_UPDATE_COMMAND_UI(ID_ISOSURFACE_TOOL, OnUpdateIsosurfaceTool)
	ON_COMMAND(ID_CROP_TOOL, OnCropTool)
	ON_UPDATE_COMMAND_UI(ID_CROP_TOOL, OnUpdateCropTool)
	ON_COMMAND(ID_ANIMATION_TOOL, OnAnimationTool)
	ON_UPDATE_COMMAND_UI(ID_ANIMATION_TOOL, OnUpdateAnimationTool)
	ON_COMMAND(ID_COLOR_BAR_TOOL, OnColorBarTool)
	ON_UPDATE_COMMAND_UI(ID_COLOR_BAR_TOOL, OnUpdateColorBarTool)
	ON_COMMAND(ID_SHOW_COLOR_BAR, OnShowColorBar)
	ON_UPDATE_COMMAND_UI(ID_SHOW_COLOR_BAR, OnUpdateShowColorBar)
	ON_COMMAND(ID_LOAD_DATA, OnLoadData)
	ON_COMMAND(ID_SHOW_VECTORS, OnShowVectors)
	ON_UPDATE_COMMAND_UI(ID_SHOW_VECTORS, OnUpdateShowVectors)
	ON_COMMAND(ID_VECTOR_TOOL, OnVectorTool)
	ON_UPDATE_COMMAND_UI(ID_VECTOR_TOOL, OnUpdateVectorTool)
	ON_COMMAND(ID_DATA_TOOL, OnDataTool)
	ON_UPDATE_COMMAND_UI(ID_DATA_TOOL, OnUpdateDataTool)
	ON_COMMAND(ID_ALL_ACTIVE_CELLS, OnAllActiveCells)
	ON_UPDATE_COMMAND_UI(ID_ALL_ACTIVE_CELLS, OnUpdateAllActiveCells)
	ON_COMMAND(ID_SHOW_MODEL_FEATURES, OnShowModelFeatures)
	ON_UPDATE_COMMAND_UI(ID_SHOW_MODEL_FEATURES, OnUpdateShowModelFeatures)
	ON_COMMAND(ID_MODEL_FEATURES_TOOL, OnModelFeaturesTool)
	ON_UPDATE_COMMAND_UI(ID_MODEL_FEATURES_TOOL, OnUpdateModelFeaturesTool)
	ON_COMMAND(ID_SHOW_PATHLINES, OnShowPathlines)
	ON_UPDATE_COMMAND_UI(ID_SHOW_PATHLINES, OnUpdateShowPathlines)
	ON_COMMAND(ID_PATHLINES_TOOL, OnPathlinesTool)
	ON_UPDATE_COMMAND_UI(ID_PATHLINES_TOOL, OnUpdatePathlinesTool)
	ON_UPDATE_COMMAND_UI(ID_FILE_NEW, OnUpdateFileNew)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPEN, OnUpdateFileOpen)
	ON_UPDATE_COMMAND_UI(ID_CUSTOM_FILE_CLOSE, OnUpdateCustomFileClose)
	ON_COMMAND(ID_SHOW_PARTICLE, OnShowParticle)
	ON_UPDATE_COMMAND_UI(ID_SHOW_PARTICLE, OnUpdateShowParticle)
	ON_COMMAND(ID_PARTICLES_TOOL, OnParticlesTool)
	ON_UPDATE_COMMAND_UI(ID_PARTICLES_TOOL, OnUpdateParticlesTool)
	ON_COMMAND(ID_SHOW_OVERLAY, OnShowOverlay)
	ON_UPDATE_COMMAND_UI(ID_SHOW_OVERLAY, OnUpdateShowOverlay)
	ON_COMMAND(ID_OVERLAY_TOOL, OnOverlayTool)
	ON_UPDATE_COMMAND_UI(ID_OVERLAY_TOOL, OnUpdateOverlayTool)
	ON_COMMAND(ID_AXES_TOOL, OnAxesTool)
	ON_UPDATE_COMMAND_UI(ID_AXES_TOOL, OnUpdateAxesTool)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMvDoc construction/destruction

CMvDoc::CMvDoc()
{
	// Initialize variables
	m_Startup = TRUE;
	m_ProjectionMode = MV_PERSPECTIVE_PROJECTION;
	m_IsAnimating = FALSE;
	m_GUI = NULL;
	m_ReadyToClose = 0;
	m_AnimationType = atTime;
	m_AnimationSteps = 10;

	// Get the models supported by this application
	m_NumberOfModels = mvModelList::GetNumberOfModels();
	m_ModelNames = new char *[m_NumberOfModels];
	for (int i=0; i<m_NumberOfModels; i++)
	{
		m_ModelNames[i] = new char[30];
	}
	mvModelList::GetModelNames(m_ModelNames);

	// Load presistent app settings from Windows registry
	LoadPreviousAppSettings();

	// Create the visualization pipeline manager
	m_Manager = new mvManager;

	// Set modeless dialog boxes to null. These cannot be created
	// until after the main frame window is created.
	m_ColorBarDlg = NULL;
	m_LightingDlg = NULL;
	m_GridDlg = NULL;
	m_GeometryDlg = NULL;
	m_DataDlg = NULL;
	m_SolidDlg = NULL;
	m_IsosurfaceDlg = NULL;
	m_VectorDlg = NULL;
	m_PathlinesDlg = NULL;
	m_ModelFeaturesDlg = NULL;
	m_CropDlg = NULL;
	m_AnimationDlg = NULL;
	m_ParticleDlg = NULL;
	m_OverlayDlg = NULL;
	m_AxesDlg = NULL;
}

CMvDoc::~CMvDoc()
{
	// Save persistent app settings to Windows registry
	SaveCurrentAppSettings();
	// Free memory
	delete m_Manager;
	for (int i=0; i<m_NumberOfModels; i++)
	{
		delete [] m_ModelNames[i];
	}
	delete [] m_ModelNames;
	// Note that modeless dialog boxes delete themselves.
}

BOOL CMvDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	if (!m_Startup)
	{
		m_Manager->ClearData();
		m_Manager->SetOverlayFileName(0);
		m_DataDlg->Reinitialize();
		m_ColorBarDlg->Reinitialize();
		m_LightingDlg->Reinitialize();
		m_GridDlg->Reinitialize();
		m_GeometryDlg->Reinitialize();
		m_SolidDlg->Reinitialize();
		m_IsosurfaceDlg->Reinitialize();
		m_VectorDlg->Reinitialize();
		m_VectorDlg->ShowWindow(SW_HIDE);
		m_PathlinesDlg->Reinitialize();
		m_PathlinesDlg->ShowWindow(SW_HIDE);
		m_ModelFeaturesDlg->Reinitialize();
		m_ModelFeaturesDlg->ShowWindow(SW_HIDE);
		m_ParticleDlg->Reinitialize();
		m_ParticleDlg->ShowWindow(SW_HIDE);
		m_OverlayDlg->Reinitialize();
		m_CropDlg->Reinitialize();
		m_AnimationDlg->Reinitialize();
		m_AxesDlg->Reinitialize();
		SetBackgroundColor(1, 1, 1);
		SetModifiedFlag(FALSE);
		if (m_ReadyToClose)
		{
			m_ReadyToClose = 0;
		}
		else
		{
			OnLoadData();
		}
		// Discard previously saved viewpoint
		POSITION pos = GetFirstViewPosition();
		CMvView *pView = (CMvView *) GetNextView(pos);
		pView->DiscardSavedViewpoint();
	}
	m_Startup = FALSE;
	return TRUE;
}

BOOL CMvDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
	// Note: The base class method CDocument::OnOpenDocument is not called
	// because deserialization is done by the visualization pipeline manager.
	// Thus, the method CMvDoc::Serialize is not used in this program.

	// Create a gui settings to store parameters for the doc and view.
	mvGUISettings *gui = new mvGUISettings;

	// Deserialization is done by the visualization pipeline manager
	char *errorMsg = m_Manager->Deserialize(lpszPathName, gui);
	if (errorMsg != NULL)
	{
		AfxMessageBox(errorMsg);
		delete gui;
		return FALSE;
	}

	// Apply the view settings
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->ApplyViewSettings(gui);
	if (gui->parallelProjection == 1)
	{
		m_ProjectionMode = MV_PARALLEL_PROJECTION;
	}
	else
	{
		m_ProjectionMode = MV_PERSPECTIVE_PROJECTION;
	}
	// Discard previously saved viewpoint
	pView->DiscardSavedViewpoint();

	// If we are at application startup (i.e., program is launched
	// by opening a document file, then tool dialogs don't exist
	// at this point, so we save the settings for now. They will
	// be applied when the view calls the CreateToolDialogs method.
	if (m_Startup)
	{
		m_GUI = gui;
		m_Startup = FALSE;
	}
	// If we are not at startup, then update the tool dialog boxes
	else
	{
		UpdateToolDialogs(gui);
		delete gui;
	}

	// Check for warning message.
	if (strlen(m_Manager->GetWarningMessage()))
	{
		AfxMessageBox(m_Manager->GetWarningMessage());
		m_Manager->ClearWarningMessage();
	}

	// Mark this document as unmodified.
	SetModifiedFlag(FALSE);
	return TRUE;
}

BOOL CMvDoc::OnSaveDocument(LPCTSTR lpszPathName) 
{
	// Note: The base class method CDocument::OnSaveDocument is not called
	// because serialization is done by the visualization pipeline manager.
	// Thus, the method CMvDoc::Serialize is not used in this program.

	// Copy the gui settings from the doc
	mvGUISettings *gui = new mvGUISettings;
	m_CropDlg->UpdateData(TRUE);
	gui->cropBoundsXDelta = m_CropDlg->m_ControlsPage->m_XDelta;
	gui->cropBoundsYDelta = m_CropDlg->m_ControlsPage->m_YDelta;
	gui->cropBoundsZDelta = m_CropDlg->m_ControlsPage->m_ZDelta;
	m_AnimationDlg->m_OptionsPage->UpdateData(TRUE);
	gui->animationRotate = m_AnimationDlg->m_OptionsPage->m_Rotate;
	gui->animationElevate = m_AnimationDlg->m_OptionsPage->m_Elevate;
	gui->animationDelay = m_AnimationDlg->m_OptionsPage->m_Delay;
	m_LightingDlg->m_LightsPage->UpdateData(TRUE);
	gui->headlightOn = m_LightingDlg->m_LightsPage->m_HeadlightOn;
	gui->auxiliaryLightOn = m_LightingDlg->m_LightsPage->m_AuxiliaryLightOn;
	gui->headlightIntensity = m_LightingDlg->m_LightsPage->m_HeadlightIntensity * 0.01f;
	gui->auxiliaryLightIntensity = m_LightingDlg->m_LightsPage->m_AuxiliaryLightIntensity * 0.01f;
	m_LightingDlg->m_BackgroundPage->UpdateData(TRUE);
	gui->customBackground =	m_LightingDlg->m_BackgroundPage->m_Background;
	m_ParticleDlg->UpdateData(TRUE);
	gui->particleCropBoundsXDelta = m_ParticleDlg->m_DeltaX;
	gui->particleCropBoundsYDelta = m_ParticleDlg->m_DeltaY;
	gui->particleCropBoundsZDelta = m_ParticleDlg->m_DeltaZ;

	// Copy the gui settings from the view
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->GetViewSettings(gui);

	// The visualization pipeline manager will serialize everything along
	// with the gui settings
	char *errorMsg = m_Manager->Serialize(lpszPathName, gui);
	delete gui;
	if (errorMsg != 0)
	{
		AfxMessageBox(errorMsg);
		return FALSE;
	}

	// Mark this document as saved
	SetModifiedFlag(FALSE);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CMvDoc serialization

void CMvDoc::Serialize(CArchive& ar)
{
	// Note: The visualization pipeline manager will write data to file,
	// so the serialization capability of CDocument is not used here.
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}

/////////////////////////////////////////////////////////////////////////////
// CMvDoc diagnostics

#ifdef _DEBUG
void CMvDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CMvDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// Load and save application settings

void CMvDoc::LoadPreviousAppSettings()
{
	CString directory = AfxGetApp()->GetProfileString("Settings", "Directory");
	m_InteractorStyle = AfxGetApp()->GetProfileInt("Settings", "Interactor Style", 0);
	m_DefaultModel = AfxGetApp()->GetProfileString("Settings", "Default Model");
	_chdir(directory.GetBuffer(10));
}

void CMvDoc::SaveCurrentAppSettings()
{
	if (m_strPathName.GetLength() > 0)
	{
		int count = m_strPathName.ReverseFind('\\');
		if (m_strPathName.GetAt(count-1) == ':')
		{
			count++;
		}
		CString directory = m_strPathName.Left(count);
		
		AfxGetApp()->WriteProfileString("Settings", "Directory", 
										(LPCTSTR) directory);		
	}
	AfxGetApp()->WriteProfileInt("Settings", "Interactor Style", m_InteractorStyle);
	AfxGetApp()->WriteProfileString("Settings", "Default Model", (LPCTSTR) m_DefaultModel);
}


/////////////////////////////////////////////////////////////////////////////
// "Get" methods. These are wrappers for "get" methods of the visualization
// pipeline manager.

void CMvDoc::GetScalarDataRange(float *range)
{
	m_Manager->GetScalarDataRange(range);
}

void CMvDoc::GetVectorMagnitudeRange(float *range)
{
	m_Manager->GetVectorMagnitudeRange(range);
}

vtkPropCollection *CMvDoc::GetPropCollection()
{
	return m_Manager->GetPropCollection();
}

char **CMvDoc::GetTimePointLabels()
{
	return m_Manager->GetTimePointLabels();
}

int CMvDoc::GetNumberOfTimePoints()
{
	return m_Manager->GetNumberOfTimePoints();
}

char *CMvDoc::GetModelName()
{
	return m_Manager->GetModelName();
}

char *CMvDoc::GetDataName()
{
	return m_Manager->GetActiveScalarDataName();
}


/////////////////////////////////////////////////////////////////////////////
// Creates the (modeless) tool dialog boxes. Note that these dialog boxes
// holds a pointer to the main frame window, and therefore cannot be created
// until after the main frame window is created. This method is called from 
// OnInitialUpdate method of CMvView.
void CMvDoc::CreateToolDialogs()
{
	CWnd *pMainWnd = AfxGetApp( )->m_pMainWnd;
	if (m_DataDlg == NULL)
	{
		m_DataDlg = new CDataDlg(pMainWnd, this);
		m_DataDlg->Create();
	}
	if (m_ColorBarDlg == NULL)
	{
		m_ColorBarDlg = new CColorBarDlg(pMainWnd, this);
		m_ColorBarDlg->Create();
	}
	if (m_LightingDlg == NULL)
	{
		m_LightingDlg = new CLightingDlg(pMainWnd, this);
		m_LightingDlg->Create();
	}
	if (m_GridDlg == NULL)
	{
		m_GridDlg = new CGridDlg(pMainWnd, this);
		m_GridDlg->Create();
	}
	if (m_GeometryDlg == NULL)
	{
		m_GeometryDlg = new CGeometryDlg(pMainWnd, this);
		m_GeometryDlg->Create();
	}
	if (m_SolidDlg == NULL)
	{
		m_SolidDlg = new CSolidDlg(pMainWnd, this);
		m_SolidDlg->Create();
	}
	if (m_IsosurfaceDlg == NULL)
	{
		m_IsosurfaceDlg = new CIsosurfaceDlg(pMainWnd, this);
		m_IsosurfaceDlg->Create();
	}
	if (m_VectorDlg == NULL)
	{
		m_VectorDlg = new CVectorDlg(pMainWnd, this);
		m_VectorDlg->Create();
	}
	if (m_PathlinesDlg == NULL)
	{
		m_PathlinesDlg = new CPathlinesDlg(pMainWnd, this);
		m_PathlinesDlg->Create();
	}
	if (m_ModelFeaturesDlg == NULL)
	{
		m_ModelFeaturesDlg = new CModelFeaturesDlg(pMainWnd, this);
		m_ModelFeaturesDlg->Create();
	}
	if (m_ParticleDlg == NULL)
	{
		m_ParticleDlg = new CParticlesDlg(pMainWnd, this);
		m_ParticleDlg->Create();
	}
	if (m_OverlayDlg == NULL)
	{
		m_OverlayDlg = new COverlayDlg(pMainWnd, this);
		m_OverlayDlg->Create();
	}
	if (m_CropDlg == NULL)
	{
		m_CropDlg = new CCropDlg(pMainWnd, this);
		m_CropDlg->Create();
	}
	if (m_AnimationDlg == NULL)
	{
		m_AnimationDlg = new CAnimationDlg(pMainWnd, this);
		m_AnimationDlg->Create();
	}
	if (m_AxesDlg == NULL)
	{
		m_AxesDlg = new CAxesDlg(pMainWnd, this);
		m_AxesDlg->Create();
	}
	// This is only performed if startup is initiated by double clicking 
	// on the document file
	if (m_GUI != NULL)
	{
		UpdateToolDialogs(m_GUI);
		delete m_GUI;
		m_GUI = NULL;
	}
	SetModifiedFlag(FALSE);
}


/////////////////////////////////////////////////////////////////////////////
// Command handlers -- File menu

void CMvDoc::OnLoadData() 
{
	// Display dialog box for user to select model
	CModelSelectionDlg dlg;
	dlg.m_NumberOfModels = m_NumberOfModels;
	dlg.m_ModelNames =	m_ModelNames;
	if (!m_DefaultModel.IsEmpty())
	{
		char *defaultModel = m_DefaultModel.GetBuffer(10);
		for (int i=0; i<m_NumberOfModels; i++)
		{
			if (stricmp(m_ModelNames[i], defaultModel) == 0)
			{
				dlg.m_RememberSelection = TRUE;
				dlg.m_InitialSelection = i;
				break;
			}
		}
	}
	if (dlg.DoModal() != IDOK)
	{
		return;  // User clicked the Cancel button
	}

	// Remember the selected model if user requests it
	char selectedModel[20];
	strcpy(selectedModel, dlg.m_SelectedModel);
	if (dlg.m_RememberSelection)
	{
		m_DefaultModel = selectedModel;
	}
	else
	{
		m_DefaultModel.Empty();
	}

	// Display dialog box for user to specify data files for the
	// selected model. Note that memory for dataFileList is allocated
	// by the dataFileList method
	char *dataFileList = DataFilesDialog::GetDataFileList(selectedModel);
	if (dataFileList == NULL)
	{
		return;   // User clicked the Cancel button
	}

	// Load data
	char *errorMsg = m_Manager->LoadData(selectedModel, dataFileList);
	delete [] dataFileList;

	// Check for error in reading data files
	if (errorMsg != 0)
	{
		AfxMessageBox(errorMsg);
		return;
	}

	// Display dialog box for user to select time and type of data to view
	CDataSelectionDlg dlg2;
	dlg2.m_NumberOfTimePoints = m_Manager->GetNumberOfTimePoints();
	dlg2.m_TimePointLabels = m_Manager->GetTimePointLabels();
	dlg2.m_NumberOfScalarDataTypes = m_Manager->GetNumberOfScalarDataTypes();
	dlg2.m_DataTypeLabels = m_Manager->GetDataTypeLabels();
	dlg2.m_TimeLabelOption = m_Manager->GetTimeLabelOption();
	dlg2.m_InitialDisplayTimePoint = m_Manager->GetInitialDisplayTimePoint();
	dlg2.DoModal();	// Cancel not allowed. 
	m_Manager->SetTimePointTo(dlg2.m_SelectedTimePoint);
	m_Manager->SetScalarDataTypeTo(dlg2.m_SelectedDataType);

	// Apply default settings and then turn on bounding box
	m_Manager->ApplyDefaultSettings();

	// If there are more than one time points, turn on the
	// time label
	if (m_Manager->GetNumberOfTimePoints() > 1)
	{
		m_Manager->ShowTimeLabel();
	}

	// Create a gui settings object with initial settings. These
	// are used to update the tool dialog boxes and view.
	mvGUISettings *gui = new mvGUISettings;

	// Update the tool (modeless) dialog boxes
	UpdateToolDialogs(gui);

	// Set the view parameters
	m_ProjectionMode = MV_PERSPECTIVE_PROJECTION;
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetProjectionToPerspective();
	pView->ApplyViewSettings(gui);
	pView->ResetViewpoint();
	delete gui;

	// Mark this document as modified
	SetModifiedFlag(TRUE);

	// Redraw the view;
	UpdateAllViews(NULL);
}

void CMvDoc::OnUpdateFileSave(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_Manager->GetDataFileList()!=NULL && !m_IsAnimating);
}

void CMvDoc::OnUpdateFileSaveAs(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_Manager->GetDataFileList()!=NULL && !m_IsAnimating);
}

void CMvDoc::OnPreferences() 
{
	CPreferencesDlg dlg;
	dlg.m_InteractorStyle = m_InteractorStyle;
	if (dlg.DoModal() == IDOK)
	{
		m_InteractorStyle = dlg.m_InteractorStyle;
		POSITION pos = GetFirstViewPosition();
		CMvView *pView = (CMvView *) GetNextView(pos);
		pView->SetInteractorStyle(m_InteractorStyle);
	}
}

void CMvDoc::UpdateToolDialogs(mvGUISettings *gui)
{
	UpdateColorBarDlg();
	UpdateSolidDlg();
	UpdateIsosurfaceDlg();
	UpdateLightingDlg(gui);
	UpdateGeometryDlg();
	UpdateGridDlg();
	UpdateAnimationDlg(gui);
	UpdateVectorDlg();
	UpdateCropDlg(gui);
	UpdateDataDlg();
	UpdatePathlinesDlg();
	UpdateModelFeaturesDlg();
	UpdateParticlesDlg(gui);
	UpdateOverlayDlg();
	UpdateAxesDlg();
}

void CMvDoc::UpdateColorBarDlg()
{
	CColorBarLimitsPage *lim = m_ColorBarDlg->m_LimitsPage;
	if (!lim->m_LogScaleCheckBox)
	{
		return;
	}

	
	CColorBarDataSource *data_source = m_ColorBarDlg->m_DataSource;
	data_source->m_DataSourceIndex = m_Manager->GetColorBarSource();
	data_source->CustomUpdateData(FALSE);

//	CColorBarLimitsPage *lim = m_ColorBarDlg->m_LimitsPage;
	lim->m_ValueBlue = m_Manager->GetColorBarValueBlue();
	lim->m_ValueRed = m_Manager->GetColorBarValueRed();
	lim->m_LogScaleCheckBox.SetCheck(!m_Manager->IsColorBarLinear());
	lim->CustomUpdateData(FALSE);

	CColorBarSizePage *size = m_ColorBarDlg->m_SizePage;
	size->m_Width = m_Manager->GetColorBarWidth();
	size->m_Height = m_Manager->GetColorBarHeight();
	size->m_Offset = m_Manager->GetColorBarOffset();
	size->CustomUpdateData(FALSE);

	CColorBarTextPage *text = m_ColorBarDlg->m_TextPage;
	text->m_FontSize = m_Manager->GetColorBarFontSize();
	text->m_NumLabels = m_Manager->GetColorBarNumberOfLabels();
	text->m_Precision = m_Manager->GetColorBarLabelPrecision();
	const float *rgb = m_Manager->GetColorBarTextColor();
	text->m_ColorOption = (int) (rgb[0]*2 + 0.1);
	text->CustomUpdateData(FALSE);

	CColorBarColorsPage *scheme = m_ColorBarDlg->m_ColorsPage;
	scheme->InitializeDialog();

	m_ColorBarDlg->m_PropertySheet->SetActivePage(0);
	m_ColorBarDlg->Activate(TRUE);
}
void CMvDoc::UpdateAxesDlg()
{
	m_AxesDlg->Update();
}

void CMvDoc::UpdateSolidDlg()
{
	float limits[2];
	m_Manager->GetSolidThresholdLimits(limits);
	m_SolidDlg->m_SolidMin = limits[0];
	m_SolidDlg->m_SolidMax = limits[1];
	m_SolidDlg->m_PrimaryScalarMode = m_Manager->GetPrimaryScalarMode();
	m_SolidDlg->m_ApplyThreshold = m_Manager->IsSolidThresholdOn();
	m_SolidDlg->m_SolidDisplayMode = m_Manager->GetSolidDisplayMode();
	m_SolidDlg->m_NumberOfColorBands = m_Manager->GetNumberOfColorBands();
	if (m_Manager->GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		m_SolidDlg->GetDlgItem(IDC_BLOCKY_SOLID)->ShowWindow(SW_SHOW);
	}
	else
	{
		m_SolidDlg->GetDlgItem(IDC_BLOCKY_SOLID)->ShowWindow(SW_HIDE);
	}
	m_SolidDlg->UpdateData(FALSE);
	m_SolidDlg->Activate(m_Manager->IsSolidVisible());
}

void CMvDoc::UpdateIsosurfaceDlg()
{
	// Regular Isosurface Page
	CRegularIsosurfacePage *reg = m_IsosurfaceDlg->m_RegularIsosurfacePage;
	float range[2];
	m_Manager->GetRegularIsosurfaceRange(range);
	reg->m_IsosurfaceCount = m_Manager->GetNumberOfRegularIsosurfaces();
	reg->m_ValueMin = range[0];
	reg->m_ValueMax = range[1];
	reg->CustomUpdateData(FALSE);

	// Custom Isosurface Page
	CCustomIsosurfacePage *custom = m_IsosurfaceDlg->m_CustomIsosurfacePage;
	const float *values = m_Manager->GetCustomIsosurfaceValues();
	CListBox *list = &(custom->m_IsosurfaceList); 
	list->ResetContent();
	char text[16];
	for (int i=0; i<m_Manager->GetNumberOfCustomIsosurfaces(); i++)
	{
		sprintf(text, "%g", values[i]);
		list->AddString(text);
	}
	custom->GetDlgItem(IDC_VALUE)->SetWindowText("");

	// Isosurface Dlg
	if (m_Manager->UsingRegularIsosurfaces())
	{
		m_IsosurfaceDlg->m_PropertySheet->SetActivePage(0);
	}
	else
	{
		m_IsosurfaceDlg->m_PropertySheet->SetActivePage(1);
	}
	m_IsosurfaceDlg->Activate(m_Manager->AreIsosurfacesVisible());
}

void CMvDoc::UpdateLightingDlg(mvGUISettings *gui)
{
	// Lights Page
	CLightsPage *lights = m_LightingDlg->m_LightsPage;
	lights->m_HeadlightOn = gui->headlightOn;
	lights->m_AuxiliaryLightOn = gui->auxiliaryLightOn;
	lights->m_HeadlightIntensity = (int) (gui->headlightIntensity*100 + 0.5);
	lights->m_AuxiliaryLightIntensity = (int) (gui->auxiliaryLightIntensity*100 + 0.5);
	lights->m_DirX = (int) (gui->auxiliaryLightDirection[0] * 10 + 10.5);
	lights->m_DirY = (int) (gui->auxiliaryLightDirection[1] * 10 + 10.5);
	lights->m_DirZ = (int) (gui->auxiliaryLightDirection[2] * 10 + 10.5);
	lights->UpdateData(FALSE);
	lights->UpdateLabels();
	lights->Activate(TRUE);

	// Surface Page
	CSurfacePage *surf = m_LightingDlg->m_SurfacePage;
	surf->m_Diffuse = (int) (m_Manager->GetDiffuseLighting()*100 + 0.5);
	surf->m_Ambient = (int) (m_Manager->GetAmbientLighting()*100 + 0.5);
	surf->m_Specular = (int) (m_Manager->GetSpecularLighting()*100 + 0.5);
	surf->m_SpecularPower = (int) (m_Manager->GetSpecularPower() + 0.5);
	surf->UpdateData(FALSE);
	surf->UpdateLabels();
	surf->Activate(TRUE);

	// Background Page
	CBackgroundPage *bg = m_LightingDlg->m_BackgroundPage;
	bg->m_Background = gui->customBackground;
	bg->m_Red = (int) (gui->background[0]*100 + 0.5);
	bg->m_Green = (int) (gui->background[1]*100 + 0.5);
	bg->m_Blue = (int) (gui->background[2]*100 + 0.5);
	bg->UpdateData(FALSE);
	bg->UpdateLabels();
	bg->Activate(TRUE);

	// Lighting Dlg
	m_LightingDlg->m_PropertySheet->SetActivePage(0);
	m_LightingDlg->m_DefaultButton.EnableWindow(TRUE);
}

void CMvDoc::UpdateGeometryDlg()
{
	// Scale page
	CScalePage *scale = m_GeometryDlg->m_ScalePage;
	const float *sc = m_Manager->GetScale();
	scale->m_XScale = sc[0];
	scale->m_YScale = sc[1];
	if ((m_Manager->GetScalarGridDimensions())[2] == 1)
	{
		scale->m_2D = TRUE;
		scale->m_ZScale = 1;
	}
	else
	{
		scale->m_2D = FALSE;
		scale->m_ZScale = sc[2];
	}
	scale->UpdateData(FALSE);
	scale->Activate(TRUE);

	// Axes page
	CAxesPage *axes = m_GeometryDlg->m_AxesPage;
	axes->m_Representation = m_Manager->GetAxesRepresentation();
	axes->m_AxesSize = m_Manager->GetAxesNormalizedSize();
	axes->m_TubeDiameter = m_Manager->GetAxesNormalizedTubeDiameter();
	const float *p = m_Manager->GetAxesNormalizedPosition();
	axes->m_XPos = p[0];
	axes->m_YPos = p[1];
	axes->m_ZPos = p[2];
	axes->CustomUpdateData(FALSE);
	axes->Activate(m_Manager->AreAxesVisible());

	// Bounding Box page
	CBoundingBoxPage *box = m_GeometryDlg->m_BoundingBoxPage;
	const float *rgb = m_Manager->GetBoundingBoxColor();
	box->m_ColorOption = (int) (rgb[0]*2 + 0.1);
	box->UpdateData(FALSE);
	box->Activate(m_Manager->IsBoundingBoxVisible());

	// Geometry dlg
	m_GeometryDlg->m_ApplyButton.EnableWindow(TRUE);
	m_GeometryDlg->m_PropertySheet->SetActivePage(0);
}

void CMvDoc::UpdateAnimationDlg(mvGUISettings *gui)
{
	// Controls Page
	CAnimationControlsPage *ctrl = m_AnimationDlg->m_ControlsPage;
	ctrl->m_NumberOfTimePoints = m_Manager->GetNumberOfTimePoints();
	ctrl->m_TimePointLabels = m_Manager->GetTimePointLabels();
	ctrl->SetAndDisplayCurrentTime(m_Manager->GetCurrentTimePointIndex());
	if (m_Manager->GetTimeLabelOption() == 0)
	{
		ctrl->GetDlgItem(IDC_CURRENT)->SetWindowText("Current time:");
		ctrl->GetDlgItem(IDC_SET_TO)->SetWindowText("Set to time:");
	}
	else
	{
		ctrl->GetDlgItem(IDC_CURRENT)->SetWindowText("Current step:");
		ctrl->GetDlgItem(IDC_SET_TO)->SetWindowText("Set to step:");
	}
	ctrl->SetAnimationType(m_AnimationType);
	ctrl->m_NumberOfSteps = m_AnimationSteps;
	ctrl->UpdateData(FALSE);

	ctrl->Reset();

	// Options Page
	CAnimationOptionsPage *opt = m_AnimationDlg->m_OptionsPage;
	opt->m_Rotate = gui->animationRotate;
	opt->m_Elevate = gui->animationElevate;
	opt->m_Delay = gui->animationDelay;
	opt->UpdateData(FALSE);
	opt->Activate(TRUE);

	// Animation Dlg
	m_AnimationDlg->m_PropertySheet->SetActivePage(0);
}

void CMvDoc::UpdateGridDlg()
{
	// Grid lines
	CGridLinesPage *lines = m_GridDlg->m_GridLinesPage;
	lines->m_2D = ((m_Manager->GetScalarGridDimensions())[2] == 1);
	lines->m_IrregularMesh = (m_Manager->GetGridType() == MV_UNSTRUCTED_GRID);
	lines->m_LayeredMesh = m_Manager->GetLayeredMesh();

	if (m_Manager->IsScalarSubgridOn())
	{
		const int *voi = m_Manager->GetScalarSubgridExtent();
		lines->m_XMin = voi[0]+1;
		lines->m_XMax = voi[1]+1;
		lines->m_YMin = voi[2]+1;
		lines->m_YMax = voi[3]+1;
		lines->m_ZMin = voi[4]+1;
		lines->m_ZMax = voi[5]+1;
	}
	else
	{
		const int *sdim = m_Manager->GetScalarGridDimensions();
		lines->m_XMin = 1;
		lines->m_XMax = sdim[0];
		lines->m_YMin = 1;
		lines->m_YMax = sdim[1];
		lines->m_ZMin = 1;
		lines->m_ZMax = sdim[2];
	}
	if (m_Manager->AreAllCellsActive() && m_Manager->GetPrimaryScalarMode() == MV_POINT_SCALARS)
	{
		lines->GetDlgItem(IDC_ACTIVATE_OUTLINE)->ShowWindow(SW_SHOW);
	}
	else
	{
		lines->GetDlgItem(IDC_ACTIVATE_OUTLINE)->ShowWindow(SW_HIDE);
	}
	int ibuff[3];
	m_Manager->GetGridLinePositions(ibuff);
	lines->m_PositionX = ibuff[0] + 1;
	lines->m_PositionY = ibuff[1] + 1;
	lines->m_PositionZ = ibuff[2] + 1;
	lines->m_ActivateXCheckBox.SetCheck(m_Manager->AreGridLinesActive(0));
	lines->m_ActivateYCheckBox.SetCheck(m_Manager->AreGridLinesActive(1));
	lines->m_ActivateZCheckBox.SetCheck(m_Manager->AreGridLinesActive(2));
	lines->m_ActivateOutlineCheckBox.SetCheck(m_Manager->IsGridOutlineActive());
	const float *rgb = m_Manager->GetGridLineColor();
	lines->m_ColorOption = (int) (rgb[0]*2 + 0.1);
	lines->CustomUpdateData(FALSE);
	BOOL Activate = m_Manager->AreActivatedGridLinesVisible();
	lines->SetMeshOutlineChoice(m_Manager->GetMeshOutlineChoice());

	lines->Activate(Activate);

	// Grid Shell
	CGridShellPage *shell = m_GridDlg->m_GridShellPage;
	rgb = m_Manager->GetGridShellColor();
	shell->m_Red = (int) (rgb[0] * 100 + 0.5);
	shell->m_Green = (int) (rgb[1] * 100 + 0.5);
	shell->m_Blue = (int) (rgb[2] * 100 + 0.5);
	shell->m_Opacity = (int) (m_Manager->GetGridShellOpacity() * 100 + 0.5);
	shell->UpdateData(FALSE);
	shell->UpdateLabels();

	// Subgrid page
	CSubgridPage *subgrid = m_GridDlg->m_SubgridPage;
	const int *voi = m_Manager->GetScalarSubgridExtent();
	const int *sdim = m_Manager->GetScalarGridDimensions();
	subgrid->m_2D =  ((m_Manager->GetScalarGridDimensions())[2] == 1);
	subgrid->m_IrregularMesh = (m_Manager->GetGridType() == MV_UNSTRUCTED_GRID);
	subgrid->m_ilow = voi[0]+1;
	if (subgrid->m_IrregularMesh)
	{
		subgrid->m_ihigh = subgrid->m_ilow;
	}
	else
	{
		subgrid->m_ihigh = voi[1];
	}
	subgrid->m_jlow = voi[2]+1;
	if (subgrid->m_IrregularMesh)
	{
		subgrid->m_jhigh = subgrid->m_jlow;
	}
	else
	{
		subgrid->m_jhigh = voi[3];
	}
	
	subgrid->m_klow = voi[4]+1;
	subgrid->m_khigh = subgrid->m_2D ? 1 : voi[5];
	if (subgrid->m_IrregularMesh)
	{
		subgrid->m_imax = 1;
		subgrid->m_jmax = 1;
	}
	else
	{
		subgrid->m_imax = sdim[0]-1;
		subgrid->m_jmax = sdim[1]-1;
	}
	subgrid->m_kmax = subgrid->m_2D ? 1 : sdim[2]-1;
	subgrid->m_ActivateSubgrid = m_Manager->IsScalarSubgridOn();
	subgrid->CustomUpdateData(FALSE);
	subgrid->Activate(TRUE);

	// Grid Dlg
	m_GridDlg->m_GridShellPage->Activate(m_Manager->IsGridShellVisible());
	m_GridDlg->m_PropertySheet->SetActivePage(0);
}

void CMvDoc::UpdateVectorDlg()
{
	if (m_Manager->HasVectorData())
	{
		// Controls Page
		CVectorControlsPage *ctrl = m_VectorDlg->m_ControlsPage;
		const int *vsub = m_Manager->GetVectorSubsampleExtents();
		const int *srate = m_Manager->GetVectorSubsampleRate();
		ctrl->m_imin = vsub[0] + 1;
		ctrl->m_imax = vsub[1] + 1;
		ctrl->m_jmin = vsub[2] + 1;
		ctrl->m_jmax = vsub[3] + 1;
		ctrl->m_kmin = vsub[4] + 1;
		ctrl->m_kmax = vsub[5] + 1;
		ctrl->m_irate = srate[0];
		ctrl->m_jrate = srate[1];
		ctrl->m_krate = srate[2];
		ctrl->m_IrregularMesh = (m_Manager->GetGridType() == MV_UNSTRUCTED_GRID);
		if (m_Manager->IsScalarSubgridOn())
		{
			const int *voi = m_Manager->GetScalarSubgridExtent();
			ctrl->m_ilimit0 = voi[0]+1;
			ctrl->m_ilimit1 = voi[1];
			ctrl->m_jlimit0 = voi[2]+1;
			ctrl->m_jlimit1 = voi[3];
			ctrl->m_klimit0 = voi[4]+1;
			ctrl->m_klimit1 = voi[5];
		}
		else
		{
			const int *vdim = m_Manager->GetVectorGridDimensions();
			ctrl->m_ilimit0 = 1;
			ctrl->m_ilimit1 = vdim[0];
			ctrl->m_jlimit0 = 1;
			ctrl->m_jlimit1 = vdim[1];
			ctrl->m_klimit0 = 1;
			ctrl->m_klimit1 = vdim[2];

		}
		ctrl->CustomUpdateData(FALSE);

		// Options Page
		CVectorOptionsPage *opt = m_VectorDlg->m_OptionsPage;
		const float *rgb = m_Manager->GetVectorColor();
		opt->m_ScaleFactor = m_Manager->GetVectorScaleFactor();
		opt->m_ShowGlyph = m_Manager->IsVectorGlyphActivated();
		opt->m_ColorOption = (int) (rgb[0]*2 + 0.1);
		opt->m_LogTransform = m_Manager->GetLogTransformVector();
		opt->m_LineWidth= m_Manager->GetVectorLineWidth();
		opt->CustomUpdateData(FALSE);
		
		// Threshold Page
		CVectorThresholdPage *threshold = m_VectorDlg->m_ThresholdPage;
		float limits[2];
		m_Manager->GetVectorThresholdLimits(limits);
		threshold->m_VectorMin = limits[0];
		threshold->m_VectorMax = limits[1];
		threshold->m_ApplyThreshold = m_Manager->IsVectorThresholdOn();
		threshold->CustomUpdateData(FALSE);

		CCropVectorsPage *crop = m_VectorDlg->m_CropVectorsPage;
		crop->m_CropAngle = m_Manager->GetVectorCroppingAngle();
		const float *CropBounds = m_Manager->GetVectorCropBounds();
		crop->m_XMin = CropBounds[0];
		crop->m_XMax = CropBounds[1]; 
		crop->m_YMin = CropBounds[2];
		crop->m_YMax = CropBounds[3];
		crop->m_ZMin = CropBounds[4];
		crop->m_ZMax = CropBounds[5];
		crop->CustomUpdateData(FALSE);

		// Vector Dlg
		m_VectorDlg->m_PropertySheet->SetActivePage(0);
		m_VectorDlg->Activate(m_Manager->AreVectorsVisible());
	}
	else
	{
		m_VectorDlg->Reinitialize();
		m_VectorDlg->ShowWindow(SW_HIDE);
	}
}

void CMvDoc::UpdateCropDlg(mvGUISettings *gui)
{
	// Control Page
	CCropControlsPage *ctrl = m_CropDlg->m_ControlsPage;
	const float *cropBounds = m_Manager->GetCropBounds();
	ctrl->m_XMin = cropBounds[0];
	ctrl->m_XMax = cropBounds[1];
	ctrl->m_YMin = cropBounds[2];
	ctrl->m_YMax = cropBounds[3];
	ctrl->m_ZMin = cropBounds[4];
	ctrl->m_ZMax = cropBounds[5];
	ctrl->m_XDelta = gui->cropBoundsXDelta;
	ctrl->m_YDelta = gui->cropBoundsYDelta;
	ctrl->m_ZDelta = gui->cropBoundsZDelta;
	ctrl->m_XSync = (cropBounds[0] == cropBounds[1]);
	ctrl->m_YSync = (cropBounds[2] == cropBounds[3]);
	ctrl->m_ZSync = (cropBounds[4] == cropBounds[5]);
	ctrl->m_CropAngle = (int) (m_Manager->GetHorizontalCropAngle() + 0.5f);
	ctrl->CustomUpdateData(FALSE);

	// Options Page
	CCropOptionsPage *opt = m_CropDlg->m_OptionsPage;
	const float *rgb = m_Manager->GetCroppedAwayPiecesColor();
	opt->m_ShowCroppedAwayPieces = m_Manager->AreCroppedAwayPiecesShown();
	opt->m_Red = (int) (rgb[0] * 100 + 0.5);
	opt->m_Green = (int) (rgb[1] * 100 + 0.5);
	opt->m_Blue = (int) (rgb[2] * 100 + 0.5);
	opt->m_Opacity = (int) (m_Manager->GetCroppedAwayPiecesOpacity() * 100 + 0.5);
	opt->UpdateData(FALSE);
	opt->UpdateLabels();

	// Crop Dlg
	m_CropDlg->m_PropertySheet->SetActivePage(0);
	m_CropDlg->Activate(m_Manager->IsSolidVisible() || m_Manager->AreIsosurfacesVisible());
}

void CMvDoc::UpdateDataDlg()
{
	float range[2];
	m_Manager->GetScalarDataRange(range);
	m_DataDlg->m_ScalarPage->SetRange(range);
	if (m_Manager->HasVectorData())
	{
		m_Manager->GetVectorMagnitudeRange(range);
		m_DataDlg->m_VectorPage->SetRange(range);
	}
	else
	{
		m_DataDlg->m_VectorPage->SetRange(0);
	}
	if (m_Manager->HasPathlineData())
	{
		m_Manager->GetPathlineTimeRange(range);
		m_DataDlg->m_PathlinePage->SetRange(range);
	}
	else
	{
		m_DataDlg->m_PathlinePage->SetRange(0);
	}
	m_DataDlg->m_PropertySheet->SetActivePage(0);
	char ** labels = m_Manager->GetDataTypeLabels();
	int numScalarDataTypes = m_Manager->GetNumberOfScalarDataTypes();
	m_DataDlg->m_ScalarPage->m_DataTypeChooser.ResetContent();
	if (numScalarDataTypes > 0)
	{
		for (int i=0; i<numScalarDataTypes; i++)
		{
			m_DataDlg->m_ScalarPage->m_DataTypeChooser.AddString(labels[i]);
		}
		m_DataDlg->m_ScalarPage->m_DataTypeChooser.SetCurSel(m_Manager->GetActiveScalarDataType());
	}
	m_DataDlg->Activate(TRUE);
}

void CMvDoc::UpdatePathlinesDlg()
{
	if (!m_PathlinesDlg)
	{
		return;
	}
	CPathlinesDisplayPage *display = m_PathlinesDlg->m_DisplayPage;
	if (m_Manager->GetPathlineRepresentation() == MV_TUBE)
	{
		display->m_PathlineRepresentation = 0;
	}
	else
	{
		display->m_PathlineRepresentation = 1;
	}
	display->m_TubeDiameter = m_Manager->GetPathlineTubeDiameter();
	display->CustomUpdateData(FALSE);

	CPathlinesColorPage *color = m_PathlinesDlg->m_ColorPage;
	color->m_ValueBlue = m_Manager->GetPathlineTimeBlue();
	color->m_ValueRed = m_Manager->GetPathlineTimeRed();
	color->m_LogTransport = m_Manager->GetPathlineLogTransform();

	color->CustomUpdateData(FALSE);

	CPathlinesClippingPage *clip = m_PathlinesDlg->m_ClipPage;
	clip->m_ClippingMode = m_Manager->GetPathlineTimeClippingMode();
	clip->m_MinimumTime = m_Manager->GetPathlineClipTimeMin();
	clip->m_MaximumTime = m_Manager->GetPathlineClipTimeMax();
	clip->CustomUpdateData(FALSE);

	m_PathlinesDlg->m_PropertySheet->SetActivePage(0);
	m_PathlinesDlg->Activate(m_Manager->ArePathlinesVisible());
	if (!m_Manager->HasPathlineData())
	{
		m_PathlinesDlg->ShowWindow(SW_HIDE);
	}
}

void CMvDoc::UpdateModelFeaturesDlg()
{
	int i;
	int numFeatureTypes = m_Manager->GetNumberOfModelFeatureTypes();
	m_ModelFeaturesDlg->SetNumberOfModelFeatureTypes(numFeatureTypes);
	int *displayOrder = m_Manager->GetModelFeatureDisplayOrder();
	m_ModelFeaturesDlg->SetDisplayOrder(displayOrder);
	char **features = new char *[numFeatureTypes];
	const char *fstring = m_Manager->GetModelFeatureLabels();
	for (i=0; i<numFeatureTypes; i++)
	{
		features[i] = new char[41];
		strncpy(features[i], fstring+(i*40), 40);
		features[i][40] = '\0';
	}
	m_ModelFeaturesDlg->m_HideListBox.ResetContent();
	m_ModelFeaturesDlg->m_ShowListBox.ResetContent();
	int *temp = new int[numFeatureTypes];
	for (i=0; i<numFeatureTypes; i++)
	{
		temp[i] = -1;
	}
	for (i=0; i<numFeatureTypes; i++)
	{
		if (displayOrder[i] == -1)
		{
			m_ModelFeaturesDlg->m_HideListBox.AddString(features[i]);
		}
		else if (displayOrder[i] > -1 && displayOrder[i] < numFeatureTypes)
		{
			temp[displayOrder[i]] = i;
		}
	}	
	for (i=0; i<numFeatureTypes; i++)
	{
		if (temp[i] > -1)
		{
			m_ModelFeaturesDlg->m_ShowListBox.AddString(features[temp[i]]);
		}
	}
	delete [] temp;
	m_ModelFeaturesDlg->Activate(m_Manager->AreModelFeaturesVisible());
	if (!m_Manager->HasModelFeatures())
	{
		m_ModelFeaturesDlg->ShowWindow(SW_HIDE);
	}
	for (i=0; i<numFeatureTypes; i++)
	{
		delete [] features[i];
	}
	delete [] features;

	m_ModelFeaturesDlg->ShowGlyphSizeControl(m_Manager->GetModelFeatureDisplayMode() == 
			MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS);
}

void CMvDoc::UpdateParticlesDlg(mvGUISettings *gui)
{
	if (gui->m_version  > 1.099)
	{
		m_ParticleDlg->m_DeltaX = gui->particleCropBoundsXDelta;
		m_ParticleDlg->m_DeltaY = gui->particleCropBoundsYDelta;
		m_ParticleDlg->m_DeltaZ = gui->particleCropBoundsZDelta;
		m_ParticleDlg->Reinitialize();
	}
	if (!m_Manager->HasParticles())
	{
		m_ParticleDlg->ShowWindow(SW_HIDE);
	}
}

void CMvDoc::UpdateOverlayDlg()
{
	double x, y;
	m_Manager->GetOverlayCoordinatesAtGridOrigin(x, y); 
	m_OverlayDlg->m_ControlsPage->m_XOrig = x;
	m_OverlayDlg->m_ControlsPage->m_YOrig = y;
	m_OverlayDlg->m_ControlsPage->m_Scale = m_Manager->GetOverlayToGridScale();
	m_OverlayDlg->m_ControlsPage->m_Angle = m_Manager->GetOverlayAngle();
	m_OverlayDlg->m_ControlsPage->m_Drape = m_Manager->GetOverlayDrape();	
	m_OverlayDlg->m_ControlsPage->m_Trim = m_Manager->GetOverlayTrim();
	m_OverlayDlg->m_ControlsPage->m_Crop = m_Manager->GetOverlayCrop();
	m_OverlayDlg->m_ControlsPage->m_Elev = m_Manager->GetOverlayElevation();
	m_OverlayDlg->m_ControlsPage->m_DrapeGap = m_Manager->GetOverlayDrapeGap();
	m_OverlayDlg->m_ControlsPage->m_StructuredGrid = m_Manager->GetIsStructuredGrid();
	m_OverlayDlg->m_ControlsPage->UpdateData(FALSE);
	m_OverlayDlg->m_ControlsPage->Activate(m_Manager->GetDataFileList() != 0);
	if (m_Manager->HasOverlay())
	{
		m_OverlayDlg->m_FilePage->m_Filename = m_Manager->GetOverlayFileName();
		m_OverlayDlg->m_FilePage->m_TypeComboBox.SetCurSel(m_Manager->GetOverlayType() - 1);
		m_OverlayDlg->m_FilePage->UpdateData(FALSE);
		double xmin, xmax, ymin, ymax;
		m_Manager->GetOverlayBounds(xmin, xmax, ymin, ymax);
		m_OverlayDlg->m_BoundsPage->SetBounds(xmin, xmax, ymin, ymax);
		m_OverlayDlg->m_PropertySheet->SetActivePage(0);
		m_OverlayDlg->m_RemoveButton.EnableWindow(TRUE);
	}
	else
	{
		m_OverlayDlg->m_FilePage->m_Filename = _T("");
		m_OverlayDlg->m_FilePage->m_TypeComboBox.SetCurSel(0);
		m_OverlayDlg->m_FilePage->UpdateData(FALSE);
		m_OverlayDlg->m_BoundsPage->Reinitialize();
		m_OverlayDlg->m_PropertySheet->SetActivePage(1);
		m_OverlayDlg->m_RemoveButton.EnableWindow(FALSE);
	}
	m_OverlayDlg->m_FilePage->Activate(m_Manager->GetDataFileList() != 0);
	m_OverlayDlg->m_ApplyButton.EnableWindow(TRUE);
}

/////////////////////////////////////////////////////////////////////
// Command handlers -- Show menu

void CMvDoc::OnShowNone() 
{
	m_Manager->HideScalarData();
	m_IsosurfaceDlg->Activate(FALSE);
	m_SolidDlg->Activate(FALSE);
	m_CropDlg->Activate(FALSE);
	UpdateAllViews(NULL);	
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowNone(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(!m_Manager->IsSolidVisible() && !m_Manager->AreIsosurfacesVisible() 
						&& m_Manager->GetDataFileList());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowSolid() 
{
	m_Manager->ShowScalarDataAsSolid();
	m_IsosurfaceDlg->Activate(FALSE);
	m_SolidDlg->Activate(TRUE);
	m_CropDlg->Activate(TRUE);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowSolid(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->IsSolidVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowIsosurfaces() 
{
	m_Manager->ShowScalarDataAsIsosurfaces();
	m_IsosurfaceDlg->Activate(TRUE);
	if (m_Manager->UsingRegularIsosurfaces())
	{
		m_IsosurfaceDlg->m_PropertySheet->SetActivePage(0);
	}
	else
	{
		m_IsosurfaceDlg->m_PropertySheet->SetActivePage(1);
	}
	m_SolidDlg->Activate(FALSE);
	m_CropDlg->Activate(TRUE);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowIsosurfaces(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreIsosurfacesVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);	
}

void CMvDoc::OnShowVectors() 
{
	if (m_Manager->AreVectorsVisible()) 
	{
		m_Manager->HideVectors();
		m_VectorDlg->Activate(FALSE);
	}
	else
	{
		if (m_Manager->GetVectorScaleFactor() == 0)
		{
			AfxMessageBox("Warning--Vector scale factor is currently set to zero!");
		}
		m_Manager->ShowVectors();
		m_VectorDlg->Activate(TRUE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowVectors(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreVectorsVisible());
	pCmdUI->Enable(m_Manager->HasVectorData() && !m_IsAnimating);
}

void CMvDoc::OnShowPathlines() 
{
	if (m_Manager->ArePathlinesVisible()) 
	{
		m_Manager->HidePathlines();
		m_PathlinesDlg->Activate(FALSE);
	}
	else
	{
		m_Manager->ShowPathlines();
		m_PathlinesDlg->Activate(TRUE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowPathlines(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->ArePathlinesVisible());
	pCmdUI->Enable(m_Manager->HasPathlineData() && !m_IsAnimating);
}

void CMvDoc::OnShowModelFeatures() 
{
	// TODO: Add your command handler code here
	if (m_Manager->AreModelFeaturesVisible()) 
	{
		m_Manager->HideModelFeatures();
		m_ModelFeaturesDlg->Activate(FALSE);
	}
	else
	{
		m_Manager->ShowModelFeatures();
		m_ModelFeaturesDlg->Activate(TRUE);
		if ((!m_ModelFeaturesDlg->IsWindowVisible()) &&
			(m_ModelFeaturesDlg->m_ShowListBox.GetCount() == 0))
		{
			m_ModelFeaturesDlg->ShowWindow(SW_SHOW);
		}
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowModelFeatures(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreModelFeaturesVisible());
	pCmdUI->Enable(m_Manager->HasModelFeatures() && !m_IsAnimating);
}

void CMvDoc::OnShowGridShell() 
{
	if (m_Manager->IsGridShellVisible()) 
	{
		m_Manager->HideGridShell();
		m_GridDlg->m_GridShellPage->Activate(FALSE);
	}
	else
	{
		m_Manager->ShowGridShell();
		m_GridDlg->m_GridShellPage->Activate(TRUE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowGridShell(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->IsGridShellVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowGridLines() 
{
	if (m_Manager->GetGridType() == MV_UNSTRUCTED_GRID)
	{
		if (m_Manager->AreMeshLinesVisible())
		{
			m_Manager->HideMeshLines();
		}
		else
		{
			m_Manager->ShowMeshLines();
		}
	}
	else
	{
		m_Manager->HideMeshLines();
	}

	if (m_Manager->AreActivatedGridLinesVisible()) 
	{
		m_Manager->HideGridLines();
		m_GridDlg->m_GridLinesPage->Activate(FALSE);
		m_GridDlg->m_ApplyButton.EnableWindow(FALSE);
	}
	else
	{
		m_Manager->ShowActivatedGridLines();
		BOOL ActivateLines = (m_Manager->GetGridType() == MV_UNSTRUCTED_GRID);
		BOOL ActivateShell = (m_Manager->GetGridType() == MV_UNSTRUCTED_GRID);
		m_GridDlg->m_GridLinesPage->Activate(TRUE);
		m_GridDlg->m_ApplyButton.EnableWindow(
			m_GridDlg->m_PropertySheet->GetActiveIndex() == 0);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowGridLines(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreActivatedGridLinesVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowBoundingBox() 
{
	if (m_Manager->IsBoundingBoxVisible()) 
	{
		m_Manager->HideBoundingBox();
		m_GeometryDlg->m_BoundingBoxPage->Activate(FALSE);
	}
	else
	{
		m_Manager->ShowBoundingBox();
		m_GeometryDlg->m_BoundingBoxPage->Activate(TRUE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowBoundingBox(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->IsBoundingBoxVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowAxes() 
{
	if (m_Manager->AreAxesVisible()) 
	{
		m_Manager->HideAxes();
		m_GeometryDlg->m_AxesPage->Activate(FALSE);
	}
	else
	{
		m_Manager->ShowAxes();
		m_GeometryDlg->m_AxesPage->Activate(TRUE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowAxes(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreAxesVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowTime() 
{
	if (m_Manager->IsTimeLabelVisible()) 
	{
		m_Manager->HideTimeLabel();
	}
	else
	{
		m_Manager->ShowTimeLabel();
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowTime(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->IsTimeLabelVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnShowTitle() 
{
	if (m_Manager->IsTitleVisible()) 
	{
		m_Manager->HideTitle();
	}
	else
	{
		m_Manager->ShowTitle();
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowTitle(CCmdUI* pCmdUI) 
{
	//pCmdUI->SetCheck(m_Manager->IsTitleVisible());
	//pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
	pCmdUI->Enable(FALSE);
}

void CMvDoc::OnShowColorBar() 
{
	if (m_Manager->IsColorBarVisible()) 
	{
		m_Manager->HideColorBar();
	}
	else
	{
		m_Manager->ShowColorBar();
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowColorBar(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->IsColorBarVisible());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

/////////////////////////////////////////////////////////////////////////////
// Command handlers -- Action Menu

void CMvDoc::OnPerspectiveProjection() 
{
	m_ProjectionMode = MV_PERSPECTIVE_PROJECTION;
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetProjectionToPerspective();
	UpdateAllViews(NULL);
}

void CMvDoc::OnUpdatePerspectiveProjection(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_ProjectionMode == MV_PERSPECTIVE_PROJECTION);
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::OnParallelProjection() 
{
	m_ProjectionMode = MV_PARALLEL_PROJECTION;
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetProjectionToParallel();
	UpdateAllViews(NULL);
}

void CMvDoc::OnUpdateParallelProjection(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_ProjectionMode == MV_PARALLEL_PROJECTION);
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}


void CMvDoc::OnAllActiveCells() 
{
	if (m_Manager->AreAllCellsActive())
	{
		m_Manager->AssumeAllCellsAreActive(0);
	}
	else
	{
		m_Manager->AssumeAllCellsAreActive(1);
	}
	UpdateDataDlg();
	SetModifiedFlag(TRUE);
	UpdateAllViews(NULL);
}

void CMvDoc::OnUpdateAllActiveCells(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreAllCellsActive());
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating &&
		m_Manager->GetGridType() == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS);
}

/////////////////////////////////////////////////////////////////////////////
// Command handlers -- Tools Menu

void CMvDoc::OnColorBarTool() 
{
	if (m_ColorBarDlg->IsWindowVisible())
	{
		m_ColorBarDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_ColorBarDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateColorBarTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_ColorBarDlg->IsWindowVisible());
}

void CMvDoc::OnLightingTool() 
{
	if (m_LightingDlg->IsWindowVisible())
	{
		m_LightingDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_LightingDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateLightingTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_LightingDlg->IsWindowVisible());
}

void CMvDoc::OnGridTool() 
{
	if (m_GridDlg->IsWindowVisible())
	{
		m_GridDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_GridDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateGridTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_GridDlg->IsWindowVisible());
}

void CMvDoc::OnGeometryTool() 
{
	if (m_GeometryDlg->IsWindowVisible())
	{
		m_GeometryDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_GeometryDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateGeometryTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_GeometryDlg->IsWindowVisible());
}

void CMvDoc::OnDataTool() 
{
	if (m_DataDlg->IsWindowVisible())
	{
		m_DataDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_DataDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateDataTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_DataDlg->IsWindowVisible());
}

void CMvDoc::OnSolidTool() 
{
	if (m_SolidDlg->IsWindowVisible())
	{
		m_SolidDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_SolidDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateSolidTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_SolidDlg->IsWindowVisible());
}

void CMvDoc::OnIsosurfaceTool() 
{
	if (m_IsosurfaceDlg->IsWindowVisible())
	{
		m_IsosurfaceDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_IsosurfaceDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateIsosurfaceTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_IsosurfaceDlg->IsWindowVisible());
}

void CMvDoc::OnVectorTool() 
{
	if (m_VectorDlg->IsWindowVisible())
	{
		m_VectorDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_VectorDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateVectorTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_VectorDlg->IsWindowVisible());
	pCmdUI->Enable(m_Manager->HasVectorData());
}

void CMvDoc::OnPathlinesTool() 
{
	if (m_PathlinesDlg->IsWindowVisible())
	{
		m_PathlinesDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_PathlinesDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdatePathlinesTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_PathlinesDlg->IsWindowVisible());
	pCmdUI->Enable(m_Manager->HasPathlineData());
}

void CMvDoc::OnModelFeaturesTool() 
{
	if (m_ModelFeaturesDlg->IsWindowVisible())
	{
		m_ModelFeaturesDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_ModelFeaturesDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateModelFeaturesTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_ModelFeaturesDlg->IsWindowVisible());
	pCmdUI->Enable(m_Manager->HasModelFeatures());
}

void CMvDoc::OnCropTool() 
{
	if (m_CropDlg->IsWindowVisible())
	{
		m_CropDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_CropDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateCropTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_CropDlg->IsWindowVisible());
}

void CMvDoc::OnAnimationTool() 
{
	if (m_AnimationDlg->IsWindowVisible())
	{
		m_AnimationDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_AnimationDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateAnimationTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_AnimationDlg->IsWindowVisible());
}


/////////////////////////////////////////////////////////////////////////////
// Callback functions from tools dialog boxes
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// Data

void CMvDoc::SetScalarDataTypeTo(int index)
{
	m_Manager->SetScalarDataTypeTo(index);

	UpdateColorBarDlg();
	UpdateSolidDlg();
	UpdateIsosurfaceDlg();
	float range[2];
	m_Manager->GetScalarDataRange(range);
	m_DataDlg->m_ScalarPage->SetRange(range);
	/*
	if (m_Manager->HasVectorData())
	{
		m_Manager->GetVectorMagnitudeRange(range);
		m_DataDlg->m_VectorPage->SetRange(range);
	}
	else
	{
		m_DataDlg->m_VectorPage->SetRange(0);
	}
	*/
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Time Label

void CMvDoc::SetTimeLabelFontSize(int size, BOOL update /* = TRUE */)
{
	m_Manager->SetTimeLabelFontSize(size);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

int CMvDoc::GetTimeLabelFontSize() const
{
	return m_Manager->GetTimeLabelFontSize();
}

void CMvDoc::SetTimeLabelPosition(float x, float y, BOOL update /* = TRUE */)
{
	m_Manager->SetTimeLabelPosition(x, y);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

const float *CMvDoc::GetTimeLabelPosition() const
{
	return m_Manager->GetTimeLabelPosition();
}


/////////////////////////////////////////////////////////////////////////////
// Color Bar

void CMvDoc::SetColorBarEndPoints(float valueBlue, float valueRed)
{
	m_Manager->SetColorBarEndPoints(valueBlue, valueRed);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::UseLinearColorBar()
{
	m_Manager->UseLinearColorBar();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::UseLogColorBar()
{
	m_Manager->UseLogColorBar();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetColorBarSize(int width, int height, int offset, BOOL update /*= TRUE*/)
{
	m_Manager->SetColorBarWidth(width);
	m_Manager->SetColorBarHeight(height);
	m_Manager->SetColorBarOffset(offset);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

void CMvDoc::SetColorBarFontSize(int fontSize, BOOL update /*= TRUE*/)
{
	m_Manager->SetColorBarFontSize(fontSize);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

void CMvDoc::SetColorBarNumberOfLabels(int numLabels, BOOL update /*= TRUE*/)
{
	m_Manager->SetColorBarNumberOfLabels(numLabels);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

void CMvDoc::SetColorBarLabelPrecision(int precision, BOOL update /*= TRUE*/)
{
	m_Manager->SetColorBarLabelPrecision(precision);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

void CMvDoc::SetColorBarTextColor(float red, float green, float blue, BOOL update /*= TRUE*/)
{
	m_Manager->SetColorBarTextColor(red, green, blue);
	if (update)
	{
		UpdateAllViews(NULL);
		SetModifiedFlag(TRUE);
	}
}

void CMvDoc::SetColorBarColorScheme(int Value)
{
	m_Manager->SetColorBarColorScheme(Value);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

int CMvDoc::GetColorBarWidth()
{
	return m_Manager->GetColorBarWidth();
}

int CMvDoc::GetColorBarHeight()
{
	return m_Manager->GetColorBarHeight();
}

int CMvDoc::GetColorBarOffset()
{
	return m_Manager->GetColorBarOffset();
}

int CMvDoc::GetColorBarFontSize()
{
	return m_Manager->GetColorBarFontSize();
}

int CMvDoc::GetColorBarColorScheme()
{
	return m_Manager->GetColorBarColorScheme();
}

unsigned long CMvDoc::GetColorBarFirstCustomColor()
{
	return m_Manager->GetColorBarFirstCustomColor();
}

unsigned long CMvDoc::GetColorBarLastCustomColor()
{
	return m_Manager->GetColorBarLastCustomColor();
}

void CMvDoc::SetColorBarFirstCustomColor(unsigned long value)
{
	m_Manager->SetColorBarFirstCustomColor(value);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetColorBarLastCustomColor(unsigned long value)
{
	m_Manager->SetColorBarLastCustomColor(value);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

int CMvDoc::GetColorBarSource()
{
	return m_Manager->GetColorBarSource();
}

void CMvDoc::SetColorBarSource(int value)
{
	m_Manager->SetColorBarSource(value);
}

float CMvDoc::GetColorBarValueBlue() const
{
	return m_Manager->GetColorBarValueBlue();
}

float CMvDoc::GetColorBarValueRed() const
{
	return m_Manager->GetColorBarValueRed();
}

/////////////////////////////////////////////////////////////////////////////
// Lighting

void CMvDoc::SetDiffuseLighting(float diffuse)
{
	m_Manager->SetDiffuseLighting(diffuse);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetAmbientLighting(float ambient)
{
	m_Manager->SetAmbientLighting(ambient);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetSpecularLighting(float specular)
{
	m_Manager->SetSpecularLighting(specular);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetSpecularPower(float specularPower)
{
	m_Manager->SetSpecularPower(specularPower);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SwitchOnHeadlight(BOOL switchOn)
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SwitchOnHeadlight(switchOn);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetHeadlightIntensity(float intensity)
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetHeadlightIntensity(intensity);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SwitchOnAuxiliaryLight(BOOL switchOn)
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SwitchOnAuxiliaryLight(switchOn);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetAuxiliaryLightIntensity(float intensity)
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetAuxiliaryLightIntensity(intensity);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetAuxiliaryLightPosition(float x, float y, float z)
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetAuxiliaryLightPosition(x, y, z);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetBackgroundColor(float red, float green, float blue)
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->SetBackgroundColor(red, green, blue);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Grid Lines

void CMvDoc::ActivateGridLines(int slice, BOOL b)
{
	if (b)
	{
		m_Manager->ActivateGridLines(slice);
	}
	else
	{
		m_Manager->DeactivateGridLines(slice);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetGridLinePositions(int posX, int posY, int posZ)
{
	m_Manager->SetGridLinePositions(posX, posY, posZ);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetGridLineColor(float red, float green, float blue)
{
	m_Manager->SetGridLineColor(red, green, blue);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ActivateGridOutline(BOOL b)
{
	if (b)
	{
		m_Manager->ActivateGridOutline();
	}
	else
	{
		m_Manager->DeactivateGridOutline();
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}
////////////////////////////////////////////////////////////////////////////
// Mesh
void CMvDoc::SetMeshOutlineChoice(int i)
{
	m_Manager->SetMeshOutlineChoice(i);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Grid Shell

void CMvDoc::SetGridShellColor(float red, float green, float blue)
{
	m_Manager->SetGridShellColor(red, green, blue);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetGridShellOpacity(float opacity)
{
	m_Manager->SetGridShellOpacity(opacity);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Scale

void CMvDoc::SetScale(float xScale, float yScale, float zScale)
{
	m_Manager->SetScale(xScale, yScale, zScale);
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->ResetCameraClippingRange();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Axes

void CMvDoc::SetAxesRepresentationToLine()
{
	m_Manager->SetAxesRepresentationToLine();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetAxesRepresentationToTube()
{
	m_Manager->SetAxesRepresentationToTube();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetAxesProperties(float xPos, float yPos, float zPos, 
							   float axesSize, float tubeDiameter)
{
	m_Manager->SetAxesNormalizedSize(axesSize);
	m_Manager->SetAxesNormalizedPosition(xPos, yPos, zPos);
	m_Manager->SetAxesNormalizedTubeDiameter(tubeDiameter);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Bounding Box

void CMvDoc::SetBoundingBoxColor(float red, float green, float blue)
{
	m_Manager->SetBoundingBoxColor(red, green, blue);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Subgrid

void CMvDoc::ApplySubgrid(int imin, int imax, int jmin, int jmax, int kmin, int kmax)
{
	m_Manager->SetScalarSubgridExtent(imin, imax, jmin, jmax, kmin, kmax);
	m_Manager->ScalarSubgridOn();

	// update the grid lines dlg box
	int ibuff[3];
	m_Manager->GetGridLinePositions(ibuff);
	m_GridDlg->m_GridLinesPage->m_XMin = imin + 1;
	m_GridDlg->m_GridLinesPage->m_XMax = imax + 1;
	m_GridDlg->m_GridLinesPage->m_YMin = jmin + 1;
	m_GridDlg->m_GridLinesPage->m_YMax = jmax + 1;
	m_GridDlg->m_GridLinesPage->m_ZMin = kmin + 1;
	m_GridDlg->m_GridLinesPage->m_ZMax = kmax + 1;
	m_GridDlg->m_GridLinesPage->m_PositionX = ibuff[0] + 1;
	m_GridDlg->m_GridLinesPage->m_PositionY = ibuff[1] + 1;
	m_GridDlg->m_GridLinesPage->m_PositionZ = ibuff[2] + 1;
	m_GridDlg->m_GridLinesPage->CustomUpdateData(FALSE);

	// Update the vector dialog box
	if (m_Manager->HasVectorData())
	{
		const int *vdim = m_Manager->GetVectorGridDimensions();
		const int *voi = m_Manager->GetVectorSubsampleExtents();
		m_VectorDlg->m_ControlsPage->m_imin = voi[0] + 1;
		m_VectorDlg->m_ControlsPage->m_imax = voi[1] + 1;
		m_VectorDlg->m_ControlsPage->m_jmin = voi[2] + 1;
		m_VectorDlg->m_ControlsPage->m_jmax = voi[3] + 1;
		m_VectorDlg->m_ControlsPage->m_kmin = voi[4] + 1;
		m_VectorDlg->m_ControlsPage->m_kmax = voi[5] + 1;
		m_VectorDlg->m_ControlsPage->m_ilimit0 = imin+1;
		m_VectorDlg->m_ControlsPage->m_ilimit1 = imax;
		m_VectorDlg->m_ControlsPage->m_jlimit0 = jmin+1;
		m_VectorDlg->m_ControlsPage->m_jlimit1 = jmax;
		m_VectorDlg->m_ControlsPage->m_klimit0 = kmin+1;
		m_VectorDlg->m_ControlsPage->m_klimit1 = kmax;
		m_VectorDlg->m_ControlsPage->CustomUpdateData(FALSE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SubgridOff()
{
	m_Manager->ScalarSubgridOff();

	// update the grid lines dlg box
	const int *sdim = m_Manager->GetScalarGridDimensions();
	m_GridDlg->m_GridLinesPage->m_XMin = 1;
	m_GridDlg->m_GridLinesPage->m_XMax = sdim[0];
	m_GridDlg->m_GridLinesPage->m_YMin = 1;
	m_GridDlg->m_GridLinesPage->m_YMax = sdim[1];
	m_GridDlg->m_GridLinesPage->m_ZMin = 1;
	m_GridDlg->m_GridLinesPage->m_ZMax = sdim[2];

	// Update the vector dlg box
	if (m_Manager->HasVectorData())
	{
		const int *vdim = m_Manager->GetVectorGridDimensions();
		const int *voi = m_Manager->GetVectorSubsampleExtents();
		m_VectorDlg->m_ControlsPage->m_imin = voi[0] + 1;
		m_VectorDlg->m_ControlsPage->m_imax = voi[1] + 1;
		m_VectorDlg->m_ControlsPage->m_jmin = voi[2] + 1;
		m_VectorDlg->m_ControlsPage->m_jmax = voi[3] + 1;
		m_VectorDlg->m_ControlsPage->m_kmin = voi[4] + 1;
		m_VectorDlg->m_ControlsPage->m_kmax = voi[5] + 1;
		m_VectorDlg->m_ControlsPage->m_ilimit0 = 1;
		m_VectorDlg->m_ControlsPage->m_ilimit1 = vdim[0];
		m_VectorDlg->m_ControlsPage->m_jlimit0 = 1;
		m_VectorDlg->m_ControlsPage->m_jlimit1 = vdim[1];
		m_VectorDlg->m_ControlsPage->m_klimit0 = 1;
		m_VectorDlg->m_ControlsPage->m_klimit1 = vdim[2];
		m_VectorDlg->m_ControlsPage->CustomUpdateData(FALSE);
	}

	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Solid

void CMvDoc::SetSolidDisplayToBlocky()
{
	m_Manager->SetSolidDisplayToBlocky();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetSolidDisplayToSmooth()
{
	m_Manager->SetSolidDisplayToSmooth();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetSolidDisplayToBanded()
{
	m_Manager->SetSolidDisplayToBanded();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ApplySolidControl(BOOL threshold, float minValue, float maxValue, int numberOfColorBands)
{
	m_Manager->SetSolidThresholdLimits(minValue, maxValue);
	m_Manager->SetNumberOfColorBands(numberOfColorBands);
	if (threshold)
	{
		m_Manager->SolidThresholdOn();
	}
	else
	{
		m_Manager->SolidThresholdOff();
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Isosurface

void CMvDoc::SetRegularIsosurfaces(int count, float valueMin, float valueMax)
{
	m_Manager->SetRegularIsosurfaces(count, valueMin, valueMax);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetCustomIsosurfaces(int count, float *values)
{
	m_Manager->SetCustomIsosurfaces(count, values);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Vector

void CMvDoc::SetVectorScaleFactor(float scaleFactor)
{
	m_Manager->SetVectorScaleFactor(scaleFactor);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

float CMvDoc::GetVectorScaleFactor()
{
	return m_Manager->GetVectorScaleFactor();
}

void CMvDoc::SetVectorSizeToOptimal()
{
	m_Manager->SetVectorSizeToOptimal();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SubsampleVectors(int imin, int imax, int jmin, int jmax, 
			int kmin, int kmax, int irate, int jrate, int krate)
{
	m_Manager->SubsampleVectors(imin, imax, jmin, jmax, kmin, kmax, 
								irate, jrate, krate);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetVectorLineWidth(float width)
{
	m_Manager->SetVectorLineWidth(width);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetVectorColor(float red, float green, float blue)
{
	m_Manager->SetVectorColor(red, green, blue);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ActivateVectorGlyph(BOOL b)
{
	m_Manager->ActivateVectorGlyph(b);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::EnlargeVectorGlyph()
{
	m_Manager->EnlargeVectorGlyph();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ShrinkVectorGlyph()
{
	m_Manager->ShrinkVectorGlyph();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ApplyVectorThreshold(float minValue, float maxValue)
{
	m_Manager->SetVectorThresholdLimits(minValue, maxValue);
	m_Manager->VectorThresholdOn();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::VectorThresholdOff()
{
	m_Manager->VectorThresholdOff();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

int CMvDoc::GetLogTransformVector()
{
	return m_Manager->GetLogTransformVector();
}

void CMvDoc::SetLogTransformVector(int Value)
{
	m_Manager->SetLogTransformVector(Value);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}


void CMvDoc::SetPathlineRepresentationToLine()
{
	m_Manager->SetPathlineRepresentationToLine();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetPathlineRepresentationToTube()
{
	m_Manager->SetPathlineRepresentationToTube();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}
void CMvDoc::SetPathlineTubeDiameter(float diameter)
{
	m_Manager->SetPathlineTubeDiameter(diameter);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetPathlineColorBarEndPoints(float valueBlue, float valueRed)
{
	m_Manager->SetPathlineColorBarEndPoints(valueBlue, valueRed);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}
void CMvDoc::SetPathlineTimeClippingMode(int mode)
{
	m_Manager->SetPathlineTimeClippingMode(mode);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}
void CMvDoc::SetPathlineLogTransform(int Value)
{
	m_Manager->SetPathlineLogTransform(Value);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetPathlineTimeClippingRange(float minTime, float maxTime)
{
	m_Manager->SetPathlineTimeClippingRange(minTime, maxTime);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::GetPathlineTimeRange(float *range)
{
	m_Manager->GetPathlineTimeRange(range);
}

int CMvDoc::HasPathlineData() const
{
	return m_Manager->HasPathlineData();
}


/////////////////////////////////////////////////////////////////////////////
// Cropping

void CMvDoc::Crop(float xMin, float xMax, float yMin, float yMax, 
					float zMin, float zMax, float cropAngle)
{
	m_Manager->Crop(xMin, xMax, yMin, yMax, zMin, zMax, cropAngle);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ShowCroppedAwayPieces()
{
	m_Manager->ShowCroppedAwayPieces();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::HideCroppedAwayPieces()
{
	m_Manager->HideCroppedAwayPieces();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetCroppedAwayPiecesColor(float red, float green, float blue)
{
	m_Manager->SetCroppedAwayPiecesColor(red, green, blue);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetCroppedAwayPiecesOpacity(float opacity)
{
	m_Manager->SetCroppedAwayPiecesOpacity(opacity);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// Model feature

void CMvDoc::SetModelFeatureDisplayOrder(int *displayOrder)
{
	m_Manager->SetModelFeatureDisplayOrder(displayOrder);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::EnlargeModelFeatureGlyphs()
{
	m_Manager->EnlargeModelFeatureGlyphs();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ShrinkModelFeatureGlyphs()
{
	m_Manager->ShrinkModelFeatureGlyphs();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetModelFeatureColor(char *modelFeatureName, float *rgba)
{
	m_Manager->SetModelFeatureColor(modelFeatureName, rgba);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::GetModelFeatureColor(char *modelFeatureName, float *rgb)
{
	m_Manager->GetModelFeatureColor(modelFeatureName, rgb);
}

/////////////////////////////////////////////////////////////////////////////
// Animation

void CMvDoc::StartAnimation(float delay)
{
	m_DataDlg->Activate(FALSE);
	m_ColorBarDlg->Activate(FALSE);
	m_LightingDlg->Activate(FALSE);
	m_GridDlg->Activate(FALSE);
	m_GeometryDlg->Activate(FALSE);
	m_SolidDlg->Activate(FALSE);
	m_IsosurfaceDlg->Activate(FALSE);
	m_VectorDlg->Activate(FALSE);
	m_PathlinesDlg->Activate(FALSE);
	m_ModelFeaturesDlg->Activate(FALSE);
	m_CropDlg->Activate(FALSE);
	m_ParticleDlg->Activate(FALSE);
	m_OverlayDlg->Activate(FALSE);
	m_IsAnimating = TRUE;
	AfxBeginThread(ControlFunction, this);
}

void CMvDoc::Animate()
{
	clock_t start, finish;
	float  duration, wait;

	// Animation loop
	if (m_AnimationType == atTime)
	{
		while ((m_Manager->GetCurrentTimePointIndex() 
			< m_Manager->GetNumberOfTimePoints() - 1) && m_IsAnimating)
		{
			start = clock();
			AdvanceOneTimePoint();
			finish = clock();
			duration = (float)(finish - start) / CLOCKS_PER_SEC;
			wait = m_AnimationDlg->m_OptionsPage->m_Delay - duration;
			if (wait > 0)
			{
				Sleep((DWORD) (wait*1000));
			}
		}
	}
	else
	{
		for (int index = 0; index < m_AnimationSteps; index++)
		{
			start = clock();
			UpdateAnimationWithSameTime();
			finish = clock();
			duration = (float)(finish - start) / CLOCKS_PER_SEC;
			wait = m_AnimationDlg->m_OptionsPage->m_Delay - duration;
			if (wait > 0)
			{
				Sleep((DWORD) (wait*1000));
			}
		}
	}
	if (m_IsAnimating)
	{
		m_IsAnimating = FALSE;
		m_AnimationDlg->m_ControlsPage->
				GetDlgItem(IDC_STOP_ANIMATION)->EnableWindow(FALSE);
	}
	if (m_AnimationType == atTime)
	{
		if (m_Manager->GetCurrentTimePointIndex() 
					< m_Manager->GetNumberOfTimePoints() - 1)
		{
			m_AnimationDlg->m_ControlsPage->
					GetDlgItem(IDC_RUN_ANIMATION)->EnableWindow(TRUE);
			m_AnimationDlg->m_ControlsPage->
					GetDlgItem(IDC_ADVANCE_ANIMATION)->EnableWindow(TRUE);
		}
	}
	else
	{
		m_AnimationDlg->m_ControlsPage->
					GetDlgItem(IDC_RUN_ANIMATION)->EnableWindow(TRUE);
		m_AnimationDlg->m_ControlsPage->
					GetDlgItem(IDC_ADVANCE_ANIMATION)->EnableWindow(TRUE);
	}
	m_AnimationDlg->m_ControlsPage->
					GetDlgItem(IDC_SET_TO_TIME_POINT)->EnableWindow(TRUE);
	m_AnimationDlg->m_ControlsPage->
					GetDlgItem(IDC_TIME_POINT_LABELS)->EnableWindow(TRUE);
	m_AnimationDlg->m_OptionsPage->Activate(TRUE);

	m_DataDlg->Activate(TRUE);
	m_ColorBarDlg->Activate(TRUE);
	m_LightingDlg->Activate(TRUE);
	m_GridDlg->m_GridShellPage->Activate(m_Manager->IsGridShellVisible());
	m_GridDlg->m_GridLinesPage->Activate(m_Manager->AreActivatedGridLinesVisible());
	m_GridDlg->m_SubgridPage->Activate(TRUE);
	m_GeometryDlg->m_ScalePage->Activate(TRUE);
	m_GeometryDlg->m_AxesPage->Activate(m_Manager->AreAxesVisible());
	m_GeometryDlg->m_BoundingBoxPage->Activate(m_Manager->IsBoundingBoxVisible());
	m_SolidDlg->Activate(m_Manager->IsSolidVisible());
	m_IsosurfaceDlg->Activate(m_Manager->AreIsosurfacesVisible());
	m_VectorDlg->Activate(m_Manager->AreVectorsVisible());
	m_PathlinesDlg->Activate(m_Manager->ArePathlinesVisible());
	m_ModelFeaturesDlg->Activate(m_Manager->AreModelFeaturesVisible());
	m_CropDlg->Activate(m_Manager->IsSolidVisible() || m_Manager->AreIsosurfacesVisible());
	m_ParticleDlg->Activate(m_Manager->AreParticlesVisible() || m_Manager->HasParticles());
	m_OverlayDlg->Activate(TRUE);
	m_OverlayDlg->m_RemoveButton.EnableWindow(m_Manager->HasOverlay());
}

void CMvDoc::UpdateAnimationPosition()
{
	POSITION pos = GetFirstViewPosition();
	CMvView *pView = (CMvView *) GetNextView(pos);
	pView->RotateCamera(m_AnimationDlg->m_OptionsPage->m_Rotate);
	pView->ElevateCamera(m_AnimationDlg->m_OptionsPage->m_Elevate);
	UpdateAnimation();
}

void CMvDoc::UpdateAnimationWithSameTime()
{
	if (!m_IsAnimating)
	{
		BeginWaitCursor();
	}
	UpdateAnimationPosition();
}

void CMvDoc::AdvanceOneTimePoint()
{
	if (!m_IsAnimating)
	{
		BeginWaitCursor();
	}
    m_Manager->AdvanceOneTimePoint();
	UpdateAnimationPosition();
}

void CMvDoc::SetTimePointTo(int timePointIndex) 
{
	BeginWaitCursor();
	m_Manager->SetTimePointTo(timePointIndex);
	UpdateAnimation();
}

void CMvDoc::StopAnimation()
{
	m_IsAnimating = FALSE;
}

void CMvDoc::UpdateAnimation()
{
	POSITION pos = GetFirstViewPosition();
	GetNextView(pos)->SendMessage(WM_PAINT);

	float range[2];
	m_Manager->GetScalarDataRange(range);
	m_DataDlg->m_ScalarPage->SetRange(range);
	m_Manager->GetVectorMagnitudeRange(range);
	m_DataDlg->m_VectorPage->SetRange(range);
	m_AnimationDlg->m_ControlsPage->
			SetAndDisplayCurrentTime(m_Manager->GetCurrentTimePointIndex());

	if (!m_IsAnimating)
	{
		BOOL b;
		AnimationType at = GetAnimationType();
		switch (at)
		{
			case atTime:
				b = m_Manager->GetCurrentTimePointIndex() 
					< m_Manager->GetNumberOfTimePoints() - 1;
				break;
			case atSpace:
				b = TRUE;
				break;
		}

		m_AnimationDlg->m_ControlsPage->
				GetDlgItem(IDC_RUN_ANIMATION)->EnableWindow(b);
		m_AnimationDlg->m_ControlsPage->
				GetDlgItem(IDC_ADVANCE_ANIMATION)->EnableWindow(b);
		EndWaitCursor();
	}

	if (m_Manager->HasParticles())
	{
		m_ParticleDlg->Reinitialize();
	}
}

void CMvDoc::SetAnimationType(AnimationType value)
{
	m_AnimationType = value;
}

void CMvDoc::SetAnimationSteps(int value)
{
	m_AnimationSteps = value;
}

//********************************************

void CMvDoc::OnUpdateFileNew(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(!m_IsAnimating);
}

void CMvDoc::OnUpdateFileOpen(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(!m_IsAnimating);
}

void CMvDoc::OnUpdateCustomFileClose(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_Manager->GetDataFileList() && !m_IsAnimating);
}

void CMvDoc::PrepareToClose()
{
	m_ReadyToClose = 1;
}

void CMvDoc::EnlargeParticleGlyphs()
{
	m_Manager->EnlargeParticleGlyphs();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::ShrinkParticleGlyphs()
{
	m_Manager->ShrinkParticleGlyphs();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}


void CMvDoc::OnShowParticle() 
{
	// TODO: Add your command handler code here
	if (m_Manager->AreParticlesVisible()) 
	{
		m_Manager->HideParticles();
		m_ParticleDlg->Activate(FALSE);
	}
	else
	{
		m_Manager->ShowParticles();
		m_ParticleDlg->Activate(TRUE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowParticle(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->AreParticlesVisible());
	pCmdUI->Enable(m_Manager->HasParticles() && !m_IsAnimating);
}

void CMvDoc::OnParticlesTool() 
{
	if (m_ParticleDlg->IsWindowVisible())
	{
		m_ParticleDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_ParticleDlg->Reinitialize();
		m_ParticleDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateParticlesTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_ParticleDlg->IsWindowVisible());
	pCmdUI->Enable(m_Manager->HasParticles());
}

void CMvDoc::FilterParticles(float concMin, float concMax)
{
	m_Manager->FilterParticles(concMin, concMax);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

float CMvDoc::GetMaxDisplayedParticleConcentration() const
{
	return m_Manager->GetMaxDisplayedParticleConcentration();
}

float CMvDoc::GetMinDisplayedParticleConcentration() const
{
	return m_Manager->GetMinDisplayedParticleConcentration();
}

float CMvDoc::GetMaxParticleConcentration() const
{
	return m_Manager->GetMaxParticleConcentration();
}

float CMvDoc::GetMinParticleConcentration() const
{
	return m_Manager->GetMinParticleConcentration();
}

void CMvDoc::CropParticles(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax)
{
	m_Manager->CropParticles(xmin, xmax, ymin, ymax, zmin, zmax);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

float CMvDoc::GetParticleClippingXMin()
{
	return m_Manager->GetParticleClippingXMin();
}

float CMvDoc::GetParticleClippingXMax()
{
	return m_Manager->GetParticleClippingXMax();
}

float CMvDoc::GetParticleClippingYMin()
{
	return m_Manager->GetParticleClippingYMin();
}

float CMvDoc::GetParticleClippingYMax()
{
	return m_Manager->GetParticleClippingYMax();
}

float CMvDoc::GetParticleClippingZMin()
{
	return m_Manager->GetParticleClippingZMin();
}

float CMvDoc::GetParticleClippingZMax()
{
	return m_Manager->GetParticleClippingZMax();
}

void CMvDoc::OnShowOverlay() 
{
	if (m_Manager->IsOverlayVisible()) 
	{
		m_Manager->HideOverlay();
	}
	else
	{
		m_Manager->ShowOverlay();
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateShowOverlay(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_Manager->IsOverlayVisible());
	pCmdUI->Enable(m_Manager->HasOverlay() && !m_IsAnimating);
}

void CMvDoc::OnOverlayTool() 
{
	if (m_OverlayDlg->IsWindowVisible())
	{
		m_OverlayDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_OverlayDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::ApplyOverlayControl(char *filename, int overlayType, double xorig, double yorig,
		double scale, double angle, int drape, int trim, int crop, double elev, double drapeGap)
{
	m_Manager->SetOverlayFileName(filename);
	m_Manager->SetOverlayType(overlayType);
	m_Manager->SetOverlayCoordinatesAtGridOrigin(xorig, yorig);
	m_Manager->SetOverlayToGridScale(scale);
	m_Manager->SetOverlayAngle(angle);
	m_Manager->SetOverlayDrape(drape);
	m_Manager->SetOverlayTrim(trim);
	m_Manager->SetOverlayCrop(crop);
	m_Manager->SetOverlayElevation(elev);
	m_Manager->SetOverlayDrapeGap(drapeGap);
	char *errMsg = 0;
	if (!m_Manager->UpdateOverlay(errMsg))
	{
		AfxMessageBox(errMsg);
		m_Manager->HideOverlay();
		m_OverlayDlg->m_RemoveButton.EnableWindow(FALSE);
		m_OverlayDlg->m_BoundsPage->Reinitialize();
		return;
	}
	else
	{
		double xmin, xmax, ymin, ymax;
		m_Manager->GetOverlayBounds(xmin, xmax, ymin, ymax);
		m_OverlayDlg->m_BoundsPage->SetBounds(xmin, xmax, ymin, ymax);
	}
	if (!m_Manager->IsOverlayVisible())
	{
		m_Manager->ShowOverlay();
	}
	m_OverlayDlg->m_RemoveButton.EnableWindow(TRUE);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

int CMvDoc::HasOverlay()
{
	return m_Manager->HasOverlay();
}

void CMvDoc::RemoveOverlay()
{
	m_Manager->RemoveOverlay();
	UpdateOverlayDlg();
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::OnUpdateOverlayTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_OverlayDlg->IsWindowVisible());
}

void CMvDoc::OnAxesTool() 
{
	if (m_AxesDlg->IsWindowVisible())
	{
		m_AxesDlg->ShowWindow(SW_HIDE);
	}
	else
	{
		m_AxesDlg->ShowWindow(SW_SHOW);
	}
}

void CMvDoc::OnUpdateAxesTool(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_AxesDlg->IsWindowVisible());
	bool CanShow = m_Manager->GetCanLogTransformXAxis() 
		|| m_Manager->GetCanLogTransformYAxis()
		|| m_Manager->GetCanLogTransformZAxis();
	pCmdUI->Enable(CanShow);
	
}

	// Axes

bool CMvDoc::GetCanLogTransformXAxis()
{
	return m_Manager->GetCanLogTransformXAxis();
}

bool CMvDoc::GetCanLogTransformYAxis()
{
	return m_Manager->GetCanLogTransformYAxis();
}

bool CMvDoc::GetCanLogTransformZAxis()
{
	return m_Manager->GetCanLogTransformZAxis();
}

bool CMvDoc::GetLogTransformXAxis()
{
	return m_Manager->GetLogTransformXAxis();
}

bool CMvDoc::GetLogTransformYAxis()
{
	return m_Manager->GetLogTransformYAxis();
}

bool CMvDoc::GetLogTransformZAxis()
{
	return m_Manager->GetLogTransformZAxis();
}

void CMvDoc::SetLogTransformXAxis(BOOL transform)
{
	if (transform)
	{
		m_Manager->SetLogTransformXAxis(TRUE);
	}
	else
	{
		m_Manager->SetLogTransformXAxis(FALSE);
	}
	
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetLogTransformYAxis(BOOL transform)
{
	if (transform)
	{
		m_Manager->SetLogTransformYAxis(TRUE);
	}
	else
	{
		m_Manager->SetLogTransformYAxis(FALSE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

void CMvDoc::SetLogTransformZAxis(BOOL transform)
{
	if (transform)
	{
		m_Manager->SetLogTransformZAxis(TRUE);
	}
	else
	{
		m_Manager->SetLogTransformZAxis(FALSE);
	}
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

char *CMvDoc::XAxisLabel()
{
	return m_Manager->XAxisLabel();
}

char *CMvDoc::YAxisLabel()
{
	return m_Manager->YAxisLabel();
}

char *CMvDoc::ZAxisLabel()
{
	return m_Manager->ZAxisLabel();
}


void CMvDoc::CropVectors(float xmin, float xmax, 
	float ymin, float ymax, float zmin, float zmax, int cropangle)
{
	m_Manager->CropVectors(xmin, xmax, ymin, ymax, zmin, zmax, cropangle);
	UpdateAllViews(NULL);
	SetModifiedFlag(TRUE);
}

AnimationType CMvDoc::GetAnimationType()
{
	return m_AnimationType;
}

int CMvDoc::GetAnimationSteps()
{
	return m_AnimationSteps;
}
