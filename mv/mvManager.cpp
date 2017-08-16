#include "mvManager.h"
#include "mvColorBandFilter.h"
#include "mvCustomAppendPolyData.h"
#include "mvExtractGrid.h"
#include "mvModelList.h"
#include "mvGUISettings.h"
#include "mvPathlines.h"
#include "mvGridLines.h"
#include "mvMeshLines.h"
#include "mvExternalMesh.h"
#include "mvExternalMeshVector.h"
#include "mvGridOutline.h"
#include "mvGridShell.h"
#include "mvHashTable.h"
#include "mvBoundingBox.h"
#include "mvAxes.h"
#include "mvModelFeatures.h"
#include "mvColorBar.h"
#include "mvDisplayText.h"
#include "mvParticles.h"
#include "mvOverlay.h"
#include "mvColorTable.h"
#include "mvLogColorTable.h"
#include "mvClipParticles.h"

#include "vtkActor.h"
#include "vtkClipPolyData.h"
#include "vtkContourFilter.h"
#include "vtkCubeSource.h"
#include "vtkCutter.h"
#include "vtkDataSetMapper.h"
#include "vtkExtractGrid.h"
#include "vtkFloatArray.h"
#include "vtkGeometryFilter.h"
#include "vtkGlyph3D.h"
#include "vtkHedgeHog.h"
#include "vtkLookupTable.h"
#include "vtkLogLookupTable.h"
#include "vtkPlane.h"
#include "vtkPolyData.h"
#include "vtkPolyDataMapper.h"
#include "vtkPropCollection.h"
#include "vtkStructuredGrid.h"
#include "vtkThreshold.h"
#include "vtkThresholdPoints.h"
#include "vtkMaskPoints.h"
#include "vtkExtractGeometry.h"

#include <direct.h>

mvManager::mvManager()
{
	int i;

	m_version = MV_VERSION;

	m_ExternalMeshActorsAdded = 0;
	m_WarningMessage[0] = '\0';
	m_TimePointIndex = 0;
	m_ActiveScalarRange[0] = 0;
	m_ActiveScalarRange[1] = 0;
	m_CropBounds[0] = 0;
	m_CropBounds[1] = 1;
	m_CropBounds[2] = 0;
	m_CropBounds[3] = 1;
	m_CropBounds[4] = 0;
	m_CropBounds[5] = 1;

	m_VectorGlyphActivated = 0;

	m_CropAngle = 0;
	m_ScaledVectorArray = 0;
	m_VectorMagnitudeArray = 0;
	m_VectorLogMagnitudeArray = 0;
	m_VectorMagnitudeRange[0] = 0;
	m_VectorMagnitudeRange[1] = 0;
	m_VectorLogMagnitudeRange[0] = 0;
	m_VectorLogMagnitudeRange[1] = 0;
	m_DoVectorThreshold = 0;
	m_ShowCroppedAwayPieces = 0;

	m_NumScalarDataTypes = 0;
	m_PathlineTimeClippingMode = 0;
	m_PathlineClipTimeMin = 0;
	m_PathlineClipTimeMax = 0;

	m_SolidDisplayMode = 0;
	m_UseLogColorBar = 0;
	m_ColorBarValueBlue = 0;
	m_ColorBarValueRed = 0;
	m_NumColorBarLabels = 0;
	m_ColorBarLabelPrecision = 0;


	m_DoSolidThreshold = 0;
	m_SolidThresholdMax = 0;
	m_SolidThresholdMin = 0;
	m_NumberOfColorBands = 0;

	m_UseRegularIsosurface = 0;
	m_NumberOfRegularIsosurfaces = 0;
	m_RegularIsosurfaceMin = 0;
	m_RegularIsosurfaceMax = 0;
	m_NumberOfCustomIsosurfaces = 0;
	m_CustomIsosurfaceValues = 0;

	// Data source will be created when data are loaded
	m_DataSource = 0;

	// Scalar Data Set
//	m_ScalarDataSet will be created when data are loaded
	m_ScalarDataSet = 0;
//	m_ScalarDataSet = vtkStructuredGrid::New();
//	m_ScalarGridPoints = vtkPoints::New();
//	m_PointScalars = vtkFloatArray::New();
//	m_PointScalars->SetNumberOfComponents(1);
//	m_CellScalars = vtkFloatArray::New();
//	m_CellScalars->SetNumberOfComponents(1);
//	m_ScalarDataSet->SetPoints(m_ScalarGridPoints);
//	m_ScalarDataSet->GetPointData()->SetScalars(m_PointScalars);
//	m_ScalarDataSet->GetCellData()->SetScalars(m_CellScalars);
//	m_ScalarGridPoints->Delete();
//	m_PointScalars->Delete();
//	m_CellScalars->Delete();

	// Vector Data Set
	m_VectorDataSet = 0;
/*	m_VectorDataSet = vtkStructuredGrid::New();
	m_VectorGridPoints = vtkPoints::New();
	m_Vectors = vtkFloatArray::New();
	m_Vectors->SetNumberOfComponents(3);
	m_VectorMagnitudes = vtkFloatArray::New();
	m_VectorMagnitudes->SetNumberOfComponents(1);
	m_VectorDataSet->SetPoints(m_VectorGridPoints);
	m_VectorDataSet->GetPointData()->SetVectors(m_Vectors);
	m_VectorDataSet->GetPointData()->SetScalars(m_VectorMagnitudes);
	m_VectorGridPoints->Delete();
	m_Vectors->Delete();
	m_VectorMagnitudes->Delete();*/
	m_VectorLog10Transform = 0;

	// Paths Data Set
	m_PathlineDataSet = vtkPolyData::New();
	m_PathlinePoints = vtkPoints::New();
	m_PathlineLines = vtkCellArray::New();
	m_PathlineScalars = vtkFloatArray::New();
	m_PathlineScalars->SetNumberOfComponents(1);
	m_PathlineDataSet->SetPoints(m_PathlinePoints);
	m_PathlineDataSet->SetLines(m_PathlineLines);
	m_PathlineDataSet->GetPointData()->SetScalars(m_PathlineScalars);
	m_PathlinePoints->Delete();
	m_PathlineLines->Delete();
	m_PathlineScalars->Delete();


//	int iStructuredGrid = m_DataSource->GetGridType();
//	int iStructuredGrid = 0;
	bool StructuredGrid = 0;
//	bool StructuredGrid = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
//		|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
//		|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);

	// debugging code
	//StructuredGrid = TRUE;

	m_ScalarSubDataSet = vtkExtractGrid::New();
	if (StructuredGrid)
	{
		m_ScalarSubDataSet->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
	}
	else
	{
		m_ScalarSubDataSet->SetInput(0);
	}

	// The data set consisting of active cells only.
	m_ActiveScalarDataSet = vtkThreshold::New();
//	m_ActiveScalarDataSet->SetInput(m_ScalarDataSet);
	m_ActiveScalarDataSet->SetAttributeModeToUseCellData();

	// The outer shell of the active scalar data set
	m_GridShell = new mvGridShell;
	m_GridShell->SetInput(m_ActiveScalarDataSet->GetOutput());

	// Filter to create color bands
	m_ColorBandFilter = mvColorBandFilter::New();

	// The filter to create isosurfaces.
	m_Isosurface = vtkContourFilter::New();
	m_Isosurface->SetInput(m_ActiveScalarDataSet->GetOutput());

	// Lookup tables
	m_LutBlueToRed = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutBlueToRed)->SetDefaultColorScheme();
	m_LutRedToBlue = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutRedToBlue)->SetReversedDefaultColorScheme();

	m_LutModifiedBlueToRed = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutModifiedBlueToRed)->SetModifiedColorScheme();
	m_LutModifiedRedToBlue = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutModifiedRedToBlue)->SetReversedModifiedColorScheme();

	m_LutCustomScale = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetFirstCustomColor(0x5A5A5A);
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetLastCustomColor(0xf0f0f0);
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetCustomColorScheme();

	m_LutReversedCustomScale = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetFirstCustomColor(0x5A5A5a);
	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetLastCustomColor(0xf0f0f0);
	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetReversedCustomColorScheme();
	
	// Log Lookup tables
	// Despite appearances to the contrary, the reversed color scheme is correct for
	// m_LogLutBlueToRed and the unreversed color scheme is correct for m_LogLutRedToBlue
	m_LogLutBlueToRed = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutBlueToRed)->SetReversedDefaultColorScheme();
	m_LogLutRedToBlue = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutRedToBlue)->SetDefaultColorScheme();

	m_LogLutModifiedBlueToRed = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutModifiedBlueToRed)->SetModifiedColorScheme();
	m_LogLutModifiedRedToBlue = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutModifiedRedToBlue)->SetReversedModifiedColorScheme();

	m_LogLutCustomScale = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetFirstCustomColor(0x5A5A5a);
	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetLastCustomColor(0xf0f0f0);
	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetCustomColorScheme();

	m_LogLutReversedCustomScale = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetFirstCustomColor(0x5A5A5a);
	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetLastCustomColor(0xf0f0f0);
	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetReversedCustomColorScheme();
	
	// Solid mapper and actor. Note that input to the solid mapper is 
	// determined by the method BuildPipelineForSolid.
	m_SolidMapper = vtkPolyDataMapper::New();
	m_SolidMapper->SetLookupTable(m_LutBlueToRed);
	m_SolidActor = vtkActor::New();
	m_SolidActor->SetMapper(m_SolidMapper);
	m_SolidActor->VisibilityOff();


	// Isosurface Mapper and Actor. Note that the input to the isosurface 
	// mapper is determined by the method BuildPipelineForIsosurface
	m_IsosurfaceMapper = vtkPolyDataMapper::New();
	m_IsosurfaceMapper->SetLookupTable(m_LutBlueToRed);
	m_IsosurfaceActor = vtkActor::New();
	m_IsosurfaceActor->SetMapper(m_IsosurfaceMapper);
	m_IsosurfaceActor->VisibilityOff();

	// Cropped away pieces mapper and actor
	m_CroppedAwayPieces = mvCustomAppendPolyData::New();
	m_CroppedAwayPiecesMapper = vtkPolyDataMapper::New();
	m_CroppedAwayPiecesMapper->SetInput(m_CroppedAwayPieces->GetOutput());
	m_CroppedAwayPiecesMapper->ScalarVisibilityOff();
	m_CroppedAwayPiecesActor = vtkActor::New();
	m_CroppedAwayPiecesActor->SetMapper(m_CroppedAwayPiecesMapper);
	m_CroppedAwayPiecesActor->GetProperty()->SetColor(1, .8f, .6f);
	m_CroppedAwayPiecesActor->GetProperty()->SetOpacity(.2);
	m_CroppedAwayPiecesActor->VisibilityOff();

	// Pathlines
	m_Pathlines = new mvPathlines;
	m_Pathlines->SetInput(m_PathlineDataSet);

	// Flow or velocity vectors
	m_ExtractVector = mvExtractGrid::New();
	m_ExtractIrregularMeshVector = vtkMaskPoints::New();
	m_CropVectors = vtkExtractGeometry::New();
	m_CropVectors->ExtractInsideOff();
	m_ClipVectors = mvClipParticles::New();
	m_CropVectors->	SetImplicitFunction(m_ClipVectors);
	//m_ExtractVector->SetInput(m_VectorDataSet);
	if (StructuredGrid)
	{
		m_ExtractVector->SetInput(dynamic_cast<vtkStructuredGrid*>(m_VectorDataSet));
		m_ExtractIrregularMeshVector->SetInput(0);
		m_CropVectors->SetInput(m_ExtractVector->GetOutput());
	}
	else
	{
		m_ExtractVector->SetInput(0);
		m_ExtractIrregularMeshVector->SetInput(dynamic_cast<vtkUnstructuredGrid*>(m_VectorDataSet));
		m_CropVectors->SetInput(m_ExtractIrregularMeshVector->GetOutput());
	}

	m_ActiveVectorDataSet = vtkThresholdPoints::New();
	if (StructuredGrid)
	{
		m_ActiveVectorDataSet->SetInput(m_ExtractVector->GetOutput());
	}
	else
	{
		m_ActiveVectorDataSet->SetInput(m_ExtractIrregularMeshVector->GetOutput());
	}
	m_ActiveVectorDataSet->ThresholdByUpper(-1);
	m_VectorThreshold = vtkThresholdPoints::New();
	m_VectorThreshold->SetInput(m_ActiveVectorDataSet->GetOutput());
	m_HedgeHog = vtkHedgeHog::New();
	m_HedgeHog->SetInput(m_ActiveVectorDataSet->GetOutput());
	m_VectorMapper = vtkPolyDataMapper::New();
	m_VectorMapper->ScalarVisibilityOff();
	m_VectorMapper->SetInput(m_HedgeHog->GetOutput());
	m_VectorActor = vtkActor::New();
	m_VectorActor->SetMapper(m_VectorMapper);
	m_VectorActor->GetProperty()->SetAmbient(1.0);
	m_VectorActor->GetProperty()->SetDiffuse(0.0);
	m_VectorActor->GetProperty()->SetColor(0, 0, 0);
	m_VectorActor->VisibilityOff();

	// Vector Base
	m_CubeSource = vtkCubeSource::New();
	m_VectorGlyph = vtkGlyph3D::New();
	m_VectorGlyph->SetSource(m_CubeSource->GetOutput());
	m_VectorGlyph->SetInput(m_ActiveVectorDataSet->GetOutput());
	m_VectorGlyph->ScalingOff();
	m_VectorGlyph->OrientOff();
	m_VectorGlyphMapper = vtkPolyDataMapper::New();
	m_VectorGlyphMapper->SetInput(m_VectorGlyph->GetOutput());
	m_VectorGlyphMapper->ScalarVisibilityOff();
	m_VectorGlyphActor = vtkActor::New();
	m_VectorGlyphActor->SetMapper(m_VectorGlyphMapper);
	m_VectorGlyphActor->GetProperty()->SetColor(0, 0, 0);
	m_VectorGlyphActor->VisibilityOff();

	// Vector crop
	m_VectorClippingXMin = 0;
	m_VectorClippingXMax = 1;
	m_VectorClippingYMin = 0;
	m_VectorClippingYMax = 1;
	m_VectorClippingZMin = 0;
	m_VectorClippingZMax = 1;
	m_VectorClippingAngle = 0;


	// Grid Lines
	for (i=0; i<3; i++)
	{
		m_GridLines[i] = new mvGridLines;
		m_GridLinesActivated[i] = 1;
		if (StructuredGrid)
		{
			m_GridLines[i]->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
		}
		else
		{
			m_GridLines[i]->SetInput(0);
		}
	}
	m_ActivatedGridLinesVisibility = 0;

	// Mesh Lines
	m_MeshLines = new mvMeshLines;
	m_ExternalMeshVector = new mvExternalMeshVector;
	m_ExternalLinesVector = new mvExternalMeshVector;


	// Grid outline
	m_GridOutline = new mvGridOutline;
	m_GridOutlineActivated = 0;
	//m_GridOutline->SetInput(m_ScalarDataSet);
	if (StructuredGrid)
	{
		m_GridOutline->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
	}
	else
	{
		m_GridOutline->SetInput(0);
	}

	// Bounding box
	m_BoundingBox = new mvBoundingBox;

	// Time Label
	m_TimeLabel = new mvDisplayText;

	// Title
	m_Title = new mvDisplayText;

	// Color Bar
	m_ColorBar = mvColorBar::New();
	m_ColorBar->SetLookupTable(m_LutBlueToRed);

	// Axes
	m_Axes = new mvAxes;

	// ModelFeatures
	m_ModelFeatures = new mvModelFeatures;

	// Particles (GWT and MOC3D)
	m_ParticleClippingXMin = 0;
	m_ParticleClippingXMax = 1;
	m_ParticleClippingYMin = 0;
	m_ParticleClippingYMax = 1;
	m_ParticleClippingZMin = 0;
	m_ParticleClippingZMax = 1;
	m_Particles = new mvParticles;
	m_Particles->SetParticleCount(0);
	m_Particles->Build();

	// Overlay
	m_Overlay = new mvOverlay;
	//m_Overlay->SetFullGrid(m_ScalarDataSet);
	if (StructuredGrid)
	{
		m_Overlay->SetFullGrid(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
	}
	else
	{
		m_Overlay->SetFullGrid(0);
	}
	m_Overlay->SetSubgrid(m_ScalarSubDataSet->GetOutput());

	// All the actors and actor2D's
	m_PropCollection = vtkPropCollection::New();
	m_PropCollection->AddItem(m_SolidActor);
	m_PropCollection->AddItem(m_IsosurfaceActor);
	m_PropCollection->AddItem(m_VectorActor);
	m_PropCollection->AddItem(m_VectorGlyphActor);
	m_PropCollection->AddItem(m_Pathlines->GetActor());
	m_PropCollection->AddItem(m_GridLines[0]->GetActor());
	m_PropCollection->AddItem(m_GridLines[1]->GetActor());
	m_PropCollection->AddItem(m_GridLines[2]->GetActor());
	m_PropCollection->AddItem(m_GridOutline->GetActor());
	m_PropCollection->AddItem(m_MeshLines->GetActor());
//	m_PropCollection->AddItem(m_ExternalMesh->GetActor());
//	m_PropCollection->AddItem(m_ExternalLines->GetActor());
	m_PropCollection->AddItem(m_ModelFeatures->GetActor());
	m_PropCollection->AddItem(m_BoundingBox->GetActor());
	m_PropCollection->AddItem(m_Axes->GetActor());
	m_PropCollection->AddItem(m_CroppedAwayPiecesActor);
	m_PropCollection->AddItem(m_GridShell->GetActor());
	m_PropCollection->AddItem(m_TimeLabel->GetActor2D());
	m_PropCollection->AddItem(m_Title->GetActor2D());
	m_PropCollection->AddItem(m_ColorBar);
	m_PropCollection->AddItem(m_Particles->GetActor());
	m_PropCollection->AddItem(m_Overlay->GetActor());


	// Smooth solid. A series of filters to create the solid after 
	// values outside the min-max range are clipped. The Smooth Solid
	// consists of the clipped grid shell (values outside the min-max
	// range are removed) and the 2 isosurfaces corresponding to the
	// min and max values.
	m_GridShellClipMin = vtkClipPolyData::New();
	m_GridShellClipMin->SetInput(m_GridShell->GetShell());
	m_GridShellClipMax = vtkClipPolyData::New();
	m_GridShellClipMax->SetInput(m_GridShellClipMin->GetOutput());
	m_GridShellClipMax->InsideOutOn();
	m_SmoothSolidIsosurface = vtkContourFilter::New();
	m_SmoothSolidIsosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
	m_SmoothSolid = mvCustomAppendPolyData::New();
	m_SmoothSolid->AddInput(m_GridShellClipMax->GetOutput());
	m_SmoothSolid->AddInput(m_SmoothSolidIsosurface->GetOutput());

	// Blocky Solid. Consists of entire cells whose scalar values are
	// within the min-max range. Cells having scalar values outside the
	// range are removed.
	m_BlockySolidThreshold = vtkThreshold::New();
//	m_BlockySolidThreshold->SetInput(m_ScalarDataSet);
	m_BlockySolidThreshold->SetAttributeModeToUseCellData();
	m_BlockySolid = vtkGeometryFilter::New();
	m_BlockySolid->SetInput(m_BlockySolidThreshold->GetOutput());

	// The cropping planes. 2 are normal to x, at min-max positions.
	// Similar for y and z.
	for (i=0; i<6; i++)
	{
		m_Plane[i] = vtkPlane::New();
	}
	m_Plane[0]->SetNormal(1, 0, 0);
	m_Plane[1]->SetNormal(-1, 0, 0);
	m_Plane[2]->SetNormal(0, 1, 0);
	m_Plane[3]->SetNormal(0, -1, 0);
	m_Plane[4]->SetNormal(0, 0, 1);
	m_Plane[5]->SetNormal(0, 0, -1);

	// The cropper is a series of filters to crop either solid or the
	// isosurfaces. Cropping means removal of portions that lie beyond
	// cropping planes, which are normal to the x, y and z directions.  
	for (i=0; i<6; i++)
	{
		m_Cropper[i] = vtkClipPolyData::New();
		m_Cropper[i]->SetClipFunction(m_Plane[i]);
		m_Cropper[i]->GenerateClippedOutputOn();
	}

	// Cropped Solid. The cropped solid is either the smooth or blocky
	// solid that has been cropped.
	m_CroppedSolid = mvCustomAppendPolyData::New();

	// Filters to extract "faces" corresponding to the cropping planes.
	for (i=0; i<6; i++)
	{
		m_ExtractFace[i] = vtkCutter::New();
		m_ExtractFace[i]->SetCutFunction(m_Plane[i]);
		m_ExtractFace[i]->SetInput(m_ActiveScalarDataSet->GetOutput());
	}

	// Filters to crop the extracted faces. Each extracted face must be
	// cropped 4 times. For example, the face extracted on a plane normal
	// to x must be cropped in the y and z extents at the min and max positions.
	for (i=0; i<24; i++)
	{
		m_FaceCrop[i] = vtkClipPolyData::New();
		m_FaceCrop[i]->SetClipFunction(m_Plane[i/4]);
	}

	// The cropped faces are appended into a single data set (m_Faces) for
	// clipping.
	m_Faces = mvCustomAppendPolyData::New();

	// If smooth solid is displayed, and solid thresholding is turned on,
	// then the extracted faces must be clipped (values outside the min-max
	// range removed).
	m_FacesClipMin = vtkClipPolyData::New();
	m_FacesClipMax = vtkClipPolyData::New();
	m_FacesClipMax->SetInput(m_FacesClipMin->GetOutput());
	m_FacesClipMax->InsideOutOn();

	// If blocky solid is displayed, and solid thresholding is turned on,
	// we keep only those face cells within the max-min range.
	m_FacesThreshold = vtkThreshold::New();
	m_FacesThreshold->SetAttributeModeToUseCellData(); 
	m_FacesThresholdGeometry = vtkGeometryFilter::New();
	m_FacesThresholdGeometry->SetInput(m_FacesThreshold->GetOutput());
	

	for (i=0; i<3; i++)
	{
		m_IsosurfaceCutter[i] = vtkCutter::New();
	}

	SetImmediateModeRendering(1);
}

mvManager::~mvManager()
{
	int i;
	if (m_DataSource != 0)
	{
		delete m_DataSource;
	}
	if (m_ScalarDataSet != 0)
	{
		m_ScalarDataSet->Delete();
	}
	if (m_VectorDataSet != 0)
	{
		m_VectorDataSet->Delete();
	}
	m_ActiveScalarDataSet->Delete();
	m_ScalarSubDataSet->Delete();
	m_Isosurface->Delete();
	m_SolidMapper->Delete();
	m_SolidActor->Delete();
	m_IsosurfaceMapper->Delete();
	m_IsosurfaceActor->Delete();
	m_PathlineDataSet->Delete();
	m_CroppedAwayPieces->Delete();
	m_CroppedAwayPiecesMapper->Delete();
	m_CroppedAwayPiecesActor->Delete();
	m_LutBlueToRed->Delete();
	m_LutRedToBlue->Delete();
	m_LutModifiedRedToBlue->Delete();
	m_LutModifiedBlueToRed->Delete();
	m_LutCustomScale->Delete();
	m_LutReversedCustomScale->Delete();
	m_LogLutBlueToRed->Delete();
	m_LogLutRedToBlue->Delete();
	m_LogLutModifiedRedToBlue->Delete();
	m_LogLutModifiedBlueToRed->Delete();
	m_LogLutCustomScale->Delete();
	m_LogLutReversedCustomScale->Delete();

	m_BlockySolidThreshold->Delete();
	m_BlockySolid->Delete();
	m_GridShellClipMin->Delete();
	m_GridShellClipMax->Delete();
	m_SmoothSolidIsosurface->Delete();
	m_SmoothSolid->Delete();
	m_ExtractVector->Delete();
	m_ExtractIrregularMeshVector->Delete();
	m_ActiveVectorDataSet->Delete();
	m_VectorThreshold->Delete();
	m_HedgeHog->Delete();
	m_VectorMapper->Delete();
	m_VectorActor->Delete();
	m_CubeSource->Delete();
	m_VectorGlyph->Delete();
	m_VectorGlyphMapper->Delete();
	m_VectorGlyphActor->Delete();
	m_CroppedSolid->Delete();
	m_FacesClipMin->Delete();
	m_FacesClipMax->Delete();
	m_Faces->Delete();
	m_FacesThreshold->Delete();
	m_FacesThresholdGeometry->Delete();
	m_ColorBandFilter->Delete();
	m_CropVectors->Delete();
	m_ClipVectors->Delete();
//	m_TransformVectorFilter->Delete();

		
	for (i=0; i<3; i++)
	{
		m_IsosurfaceCutter[i]->Delete();
	}
	for (i=0; i<24; i++)
	{
		m_FaceCrop[i]->Delete();
	}
	for (i=0; i<6; i++)
	{
		m_Cropper[i]->Delete();
		m_Plane[i]->Delete();
		m_ExtractFace[i]->Delete();
	}
	m_PropCollection->Delete();

	// mv display objects
	delete m_Pathlines;
	for (i=0; i<3; i++)
	{
		delete m_GridLines[i];
	}
	delete m_MeshLines;
	delete m_ExternalMeshVector;
	delete m_ExternalLinesVector;
	delete m_GridOutline;
	delete m_GridShell;
	delete m_BoundingBox;
	delete m_Axes;
	delete m_TimeLabel;
	delete m_Title;
	m_ColorBar->Delete();

	delete m_ModelFeatures;

	delete m_Particles;

	delete m_Overlay;

	ReleaseArrayMemory();
}

void mvManager::ResetParticleCropping()
{
	m_ParticleClippingXMin = 0;
	m_ParticleClippingXMax = 1;
	m_ParticleClippingYMin = 0;
	m_ParticleClippingYMax = 1;
	m_ParticleClippingZMin = 0;
	m_ParticleClippingZMax = 1;
	m_Particles->Reset();
}

void mvManager::ReleaseArrayMemory()
{
	if (m_ColorBarValueBlue)
	{
		delete [] m_ColorBarValueBlue;
		m_ColorBarValueBlue = 0;
	}
	if (m_ColorBarValueRed)
	{
		delete [] m_ColorBarValueRed;
		m_ColorBarValueRed = 0;
	}
	if (m_UseLogColorBar)
	{
		delete [] m_UseLogColorBar;
		m_UseLogColorBar = 0;
	}
	if (m_NumColorBarLabels)
	{
		delete [] m_NumColorBarLabels;
		m_NumColorBarLabels = 0;
	}
	if (m_ColorBarLabelPrecision)
	{
		delete [] m_ColorBarLabelPrecision;
		m_ColorBarLabelPrecision = 0;
	}
	if (m_SolidDisplayMode)
	{
		delete [] m_SolidDisplayMode;
		m_SolidDisplayMode = 0;
	}
	if (m_DoSolidThreshold)
	{
		delete [] m_DoSolidThreshold;
		m_DoSolidThreshold = 0;
	}
	if (m_SolidThresholdMax)
	{
		delete [] m_SolidThresholdMax;
		m_SolidThresholdMax = 0;
	}
	if (m_SolidThresholdMin)
	{
		delete [] m_SolidThresholdMin;
		m_SolidThresholdMin = 0;
	}
	if (m_NumberOfColorBands)
	{
		delete [] m_NumberOfColorBands;
		m_NumberOfColorBands = 0;
	}
	if (m_UseRegularIsosurface)
	{
		delete [] m_UseRegularIsosurface;
		m_UseRegularIsosurface = 0;
	}
	if (m_NumberOfRegularIsosurfaces)
	{
		delete [] m_NumberOfRegularIsosurfaces;
		m_NumberOfRegularIsosurfaces = 0;
	}
	if (m_RegularIsosurfaceMax)
	{
		delete [] m_RegularIsosurfaceMax;
		m_RegularIsosurfaceMax = 0;
	}
	if (m_RegularIsosurfaceMin)
	{
		delete [] m_RegularIsosurfaceMin;
		m_RegularIsosurfaceMin = 0;
	}
	if (m_NumberOfCustomIsosurfaces)
	{
		delete [] m_NumberOfCustomIsosurfaces;
		m_NumberOfCustomIsosurfaces = 0;
	}
	if (m_CustomIsosurfaceValues)
	{
		for (int i=0; i<m_NumScalarDataTypes; i++)
		{
			if (m_CustomIsosurfaceValues != 0)
			{
				delete [] m_CustomIsosurfaceValues[i];
			}
		}
		delete [] m_CustomIsosurfaceValues;
		m_CustomIsosurfaceValues = 0;
	}
	if (m_ScaledVectorArray != 0)
	{
		delete [] m_ScaledVectorArray;
		m_ScaledVectorArray = 0;
	}
	if (m_VectorMagnitudeArray != 0)
	{
		delete [] m_VectorMagnitudeArray;
		m_VectorMagnitudeArray = 0;
	}
	if (m_VectorLogMagnitudeArray != 0)
	{
		delete [] m_VectorLogMagnitudeArray;
		m_VectorLogMagnitudeArray = 0;
	}
}

void mvManager::ClearData()
{
	if (m_DataSource != 0)
	{
		delete m_DataSource;
	}
	m_DataSource = 0;
	m_ExternalMeshActorsAdded = 0;

	HideScalarData();
	HideCroppedAwayPieces();
	HideVectors();
	ActivateVectorGlyph(0); 
	HidePathlines();
	HideGridShell();
	HideGridLines();
	HideAxes();
	HideBoundingBox();
	HideModelFeatures();
	HideTimeLabel();
	HideTitle();
	HideColorBar();
	AssumeAllCellsAreActive(0);
	HideParticles();
	HideOverlay();
	ReleaseArrayMemory();
	ResetParticleCropping();
	ClearOverlayData();
	SetOverlayFileName(0);
	//ShowModelFeatureTypes(0);
	m_ActiveDataType = 0;
	m_WarningMessage[0] = '\0';
}

char *mvManager::LoadData(char *modelName, char *dataFileList)
{
	// Reading data from a file is done in "Deserialize".

	mvDataSource *newDataSource = mvModelList::CreateDataSource(modelName, m_version);
	if (newDataSource == 0)
	{
		return "Model is not supported by this program";
	}

	// At this point, we delete the previous data source object, if it
	// exists. This will also close all fortran io units. This is needed
	// to avoid conflicts in opening fortran io units for the new data source.
	ClearData();

	m_WarningMessage[0] = '\0';

	char *errMsg = newDataSource->LoadData(dataFileList);

	// Check for error in loading data files
	if (errMsg != 0)
	{
		delete newDataSource;
		return errMsg;
	}

	// Data files are loaded OK. We define the scalar data set

	m_DataSource = newDataSource;
	AfterNewDataSource();
	m_Pathlines->SetMinPositiveValue(m_DataSource->GetMinPositiveTime());
	const int *sdim = m_DataSource->GetScalarGridDimensions();
	int numPoints = m_DataSource->GetNumPoints();
	int numCells = m_DataSource->GetNumCells();

//	int iStructuredGrid = m_DataSource->GetGridType();
	bool StructuredGrid = GetIsStructuredGrid();


	// debugging code
	 //StructuredGrid = FALSE;

	//m_ScalarDataSet->SetDimensions(sdim[0], sdim[1], sdim[2]);
	if (StructuredGrid)
	{
		dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet)->SetDimensions(sdim[0], sdim[1], sdim[2]);
	}
	else
	{
		// create data sets for connectivity

		vtkCellArray *connectivity = vtkCellArray::New();
		int *types = new int[numCells];
		m_DataSource->AssignConnectivity(types, connectivity, 
			m_MeshLines->DataSet(), m_ExternalMeshVector, m_ExternalLinesVector);
		dynamic_cast<vtkUnstructuredGrid*>(m_ScalarDataSet)->SetCells(types, connectivity);
		m_GridOutline->SetInput(0);
		delete [] types;
		connectivity->Delete();
	}

	// debugging code
	 //StructuredGrid = TRUE;

	// Set the point coordinates
	vtkFloatArray *floatArray = vtkFloatArray::New();
	floatArray->SetNumberOfComponents(3);
	floatArray->SetArray(m_DataSource->GetScalarGridCoordinates(),
						 3*numPoints, 1);
	m_ScalarGridPoints->SetData(floatArray);
	floatArray->Delete();

	// Set Point Data
	m_PointScalars->SetArray(m_DataSource->GetScalarArray(), numPoints, 1);
	m_PointScalars->Modified();

	// Set Cell data
	if (m_DataSource->GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		m_CellScalars->SetArray(m_DataSource->GetScalarArray()+numPoints, numCells, 1);
		m_CellScalars->Modified();
		m_ActiveScalarDataSet->SetAttributeModeToUseCellData();
		m_GridLines[0]->DoThresholdUsingCellData();
		m_GridLines[1]->DoThresholdUsingCellData();
		m_GridLines[2]->DoThresholdUsingCellData();
	}
	else
	{
		// if primary scalar mode is point scalars, then we don't need
		// cell scalars but we have to define them so vtk won't crash.
		// so we just use the point scalars defined earlier.
		m_CellScalars->SetArray(m_DataSource->GetScalarArray(), numPoints, 1);
		m_CellScalars->Modified();
		m_ActiveScalarDataSet->SetAttributeModeToUsePointData();
		m_GridLines[0]->DoThresholdUsingPointData();
		m_GridLines[1]->DoThresholdUsingPointData();
		m_GridLines[2]->DoThresholdUsingPointData();
	}
	ComputeActiveScalarRange();

	m_ScalarDataSet->Modified();

	// Define the cutoff value that indicates inactive cells.
	float cutoff = m_DataSource->GetInactiveCellValue()*0.999;
	m_ActiveScalarDataSet->ThresholdByLower(cutoff);

	// Bounding box 
	float bounds[6];
	SetBoundingBoxBounds();
/*	if (m_DataSource->GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		m_ActiveScalarDataSet->Update();
		m_ActiveScalarDataSet->GetOutput()->ComputeBounds();
		m_ActiveScalarDataSet->GetOutput()->GetBounds(bounds);
		m_BoundingBox->SetBounds(bounds);
	}
	else
	{
		m_ScalarDataSet->ComputeBounds();
		m_ScalarDataSet->GetBounds(bounds);
		m_BoundingBox->SetBounds(bounds);
	}*/

	// Grid lines
	m_GridLines[0]->SetThresholdMin(cutoff);
	m_GridLines[1]->SetThresholdMin(cutoff);
	m_GridLines[2]->SetThresholdMin(cutoff);

	// default Axes
	m_ScalarDataSet->ComputeBounds();
	m_ScalarDataSet->GetBounds(bounds);
	float defaultAxesSize = (bounds[1]-bounds[0] + bounds[3]-bounds[2] + bounds[5]-bounds[4])/12;
	m_Axes->SetDefaultPositions(bounds);
	m_Axes->SetDefaultSize(defaultAxesSize);
	m_Axes->SetDefaultTubeDiameter(defaultAxesSize * 0.1);

	m_Pathlines->SetDefaultTubeDiameter(defaultAxesSize * 0.1);

	// Vector data
	if (m_DataSource->GetVectorArray() != 0)
	{
		// Get vector data
		const int *vdim = m_DataSource->GetVectorGridDimensions();
		//m_VectorDataSet->SetDimensions(vdim[0], vdim[1], vdim[2]);
		int np;
		if (StructuredGrid)
		{
			dynamic_cast<vtkStructuredGrid*>(m_VectorDataSet)->SetDimensions(vdim[0], vdim[1], vdim[2]);
			np = vdim[0] * vdim[1] * vdim[2];
		}
		else
		{
			np = m_DataSource->GetNumCells();
			vtkCellArray *connectivity = vtkCellArray::New();
			int *types = new int[numCells];
			m_DataSource->AssignConnectivity(types, connectivity, 
				m_MeshLines->DataSet(), m_ExternalMeshVector, m_ExternalLinesVector);
			dynamic_cast<vtkUnstructuredGrid*>(m_VectorDataSet)->SetCells(types, connectivity);
			m_GridOutline->SetInput(0);
			delete [] types;
			connectivity->Delete();
			AddExternalMeshActors();
			
		}

		//int np = vdim[0] * vdim[1] * vdim[2];

		// Set the point coordinates
		floatArray = vtkFloatArray::New();
		floatArray->SetNumberOfComponents(3);
		floatArray->SetArray(m_DataSource->GetVectorGridCoordinates(), 3*np, 1);
		m_VectorGridPoints->SetData(floatArray);
		floatArray->Delete();

		// Set the scaled vector data
		m_ScaledVectorArray = new float [3*np];
		m_Vectors->SetArray(m_ScaledVectorArray, 3*np, 1);
		m_Vectors->Modified();

		// The scalar point data are the vector magnitudes. These will
		// be used for thresholding
		m_VectorMagnitudeArray = new float [np];
		m_VectorLogMagnitudeArray = new float [np];
		m_VectorMagnitudes->SetArray(m_VectorMagnitudeArray, np, 1);
		m_VectorMagnitudes->Modified();
	}

	// Pathlines
	int np = m_DataSource->GetNumberOfPathlines();
	int nc = m_DataSource->GetNumberOfPathlineCoordinates();
	if (np > 0)
	{
		floatArray = vtkFloatArray::New();
		floatArray->SetNumberOfComponents(3);
		floatArray->SetArray(m_DataSource->GetPathlineCoordinates(), 3*nc, 1);
		m_PathlinePoints->SetData(floatArray);
		floatArray->Delete();

		UpdatePathlineScalars();

		float *range = m_PathlineScalars->GetRange();
		m_Pathlines->SetScalarRange(range[0], range[1]);

		vtkIdTypeArray *intArray = vtkIdTypeArray::New();
		intArray->SetNumberOfTuples(1);
		intArray->SetArray(m_DataSource->GetPathlinePointArray(), np+nc, 1);
		m_PathlineLines->SetCells(np, intArray);
		intArray->Delete();
	}

	m_ModelFeatures->SetNumberOfModelFeatureTypes(m_DataSource->GetNumberOfModelFeatureTypes());
	if (m_DataSource->GetModelFeatureArray())
	{
		float rgba[4];
		m_ModelFeatures->SetGridType(GetGridType());
		if (StructuredGrid)
		{
			m_ModelFeatures->SetFullGridDimensions(m_DataSource->GetScalarGridDimensions());
		}
		else
		{
			vtkCellArray *connectivity = vtkCellArray::New();
			int *types = new int[numCells];
			m_DataSource->AssignConnectivity(types, connectivity, 
				m_MeshLines->DataSet(), m_ExternalMeshVector, m_ExternalLinesVector);
			m_ModelFeatures->AssignConnectivity(types, connectivity);
			m_GridOutline->SetInput(0);
			delete [] types;
			connectivity->Delete();
			AddExternalMeshActors();
		}
		m_ModelFeatures->SetDisplayMode(m_DataSource->GetModelFeatureDisplayMode());
		for (int i=0; i<m_DataSource->GetNumberOfModelFeatureTypes(); i++)
		{
			m_DataSource->GetDefaultModelFeatureColor(i, rgba);
			m_ModelFeatures->SetColor(i, rgba);
		}
		m_ModelFeatures->SetModelFeatureArray(m_DataSource->GetModelFeatureArray());
		m_ModelFeatures->SetGridPoints(m_ScalarGridPoints);
		m_ModelFeatures->Build();

		float bounds[6];
		m_BoundingBox->GetBounds(bounds);
		float dx = bounds[1] - bounds[0];
		float dy = bounds[3] - bounds[2];
		float dz = bounds[5] - bounds[4];
		float diag = sqrt(dx*dx + dy*dy + dz*dz);
		m_ModelFeatures->SetDefaultGlyphSize(diag/300);
	}

	// Particles. Note: The Build method of m_Particles is invoked in the method OnDataModified()
	m_Particles->SetParticleCount(m_DataSource->GetParticleCount());

	// Create arrays to hold tool parameters for each scalar data type
	m_NumScalarDataTypes = m_DataSource->GetNumberOfScalarDataTypes();
	m_ColorBarValueBlue = new float[m_NumScalarDataTypes];
	m_ColorBarValueRed = new float[m_NumScalarDataTypes];
	m_UseLogColorBar = new int[m_NumScalarDataTypes];
	m_NumColorBarLabels = new int[m_NumScalarDataTypes];
	m_ColorBarLabelPrecision = new int[m_NumScalarDataTypes];
	m_SolidDisplayMode = new int[m_NumScalarDataTypes];
	m_DoSolidThreshold = new int[m_NumScalarDataTypes];
	m_SolidThresholdMax = new float[m_NumScalarDataTypes];
	m_SolidThresholdMin = new float[m_NumScalarDataTypes];
	m_NumberOfColorBands = new int[m_NumScalarDataTypes];
	m_UseRegularIsosurface = new int[m_NumScalarDataTypes];
	m_NumberOfRegularIsosurfaces = new int[m_NumScalarDataTypes];
	m_RegularIsosurfaceMax = new float[m_NumScalarDataTypes];
	m_RegularIsosurfaceMin = new float[m_NumScalarDataTypes];
	m_NumberOfCustomIsosurfaces = new int[m_NumScalarDataTypes];
	m_CustomIsosurfaceValues = new float *[m_NumScalarDataTypes];

	// Assign default tool parameters
	for (int i=0; i<m_NumScalarDataTypes; i++)
	{
		m_ColorBarValueBlue[i] = 0;
		m_ColorBarValueRed[i] = 1;
		m_UseLogColorBar[i] = 0;
		m_NumColorBarLabels[i] = 5;
		m_ColorBarLabelPrecision[i] = 3;
		m_SolidDisplayMode[i] = MV_SMOOTH;
		m_DoSolidThreshold[i] = 0;
		m_SolidThresholdMax[i] = 1;
		m_SolidThresholdMin[i] = 0;
		m_NumberOfColorBands[i] = 10;
		m_UseRegularIsosurface[i] = 1;
		m_NumberOfRegularIsosurfaces[i] = 0;
		m_RegularIsosurfaceMax[i] = 1;
		m_RegularIsosurfaceMin[i] = 0;
		m_NumberOfCustomIsosurfaces[i] = 0;
		m_CustomIsosurfaceValues[i] = 0;
	}

	return 0;
}

void mvManager::ApplyDefaultSettings()
{
//	int iStructuredGrid = m_DataSource->GetGridType();
	bool StructuredGrid = GetIsStructuredGrid();
//	bool StructuredGrid = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
//		|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
//		|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);

	// debugging code
	// StructuredGrid = FALSE;

	
	AssumeAllCellsAreActive(GetGridType() != MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS);

	// Hide Everything
	HideScalarData();
	HideCroppedAwayPieces();
	HideVectors();
	ActivateVectorGlyph(0);
	SetLogTransformVector(0);
	HidePathlines();
	HideGridShell();
	HideGridLines();
	HideAxes();
	HideBoundingBox();
	HideModelFeatures();
	HideTimeLabel();
	HideTitle();
	HideColorBar();
	HideParticles();
	HideOverlay();

	// Color Bar
	SetColorBarWidth(20);
	SetColorBarHeight(200);
	SetColorBarOffset(100);
	SetColorBarFontSize(14);

	// Surface lighting
	SetDiffuseLighting(1);
	SetAmbientLighting(0);
	SetSpecularLighting(0);
	SetSpecularPower(1);

	// Grid Shell
	SetGridShellColor(1, 0.8f, 0.6f);
	SetGridShellOpacity(0.2f);

	// Scale
	SetScale(1, 1, 1);

	// Axes
	SetAxesRepresentationToTube();
	SetAxesNormalizedSize(1);
	SetAxesNormalizedPosition(0, 0, 0);
	SetAxesNormalizedTubeDiameter(1);

	// Bounding Box
	SetBoundingBoxColor(0, 0, 0);

	// Crop
	Crop(0, 1, 0, 1, 0, 1, 0);
	SetCroppedAwayPiecesColor(1, 0.8f, 0.6f);
	SetCroppedAwayPiecesOpacity(0.2f);

	// Subgrid
	int *sdim = m_DataSource->GetScalarGridDimensions();
	SetScalarSubgridExtent(0, sdim[0]-1, 0, sdim[1]-1, 0, sdim[2]-1);
	ScalarSubgridOff();

	// Grid Lines, bounding box
	SetGridLineColor(0, 0, 0);
	if (m_DataSource->GetInitialGridDisplayPolicy() ==
			MV_INITIAL_DISPLAY_GRID_OUTLINE)
	{
		SetGridLinePositions(0, 0, 0);
		DeactivateGridLines(0);
		DeactivateGridLines(1);
		DeactivateGridLines(2);

		if (StructuredGrid)
		{
			ActivateGridOutline();
			HideMeshLines();
		}
		else
		{
			DeactivateGridOutline();
			ShowMeshLines();
		}
		ShowActivatedGridLines();
	}
	else
	{
		SetGridLinePositions(sdim[0]/2, sdim[1]/2, sdim[2]/2);
		ActivateGridLines(0);
		ActivateGridLines(1);
		ActivateGridLines(2);
		DeactivateGridOutline();
		HideMeshLines();
		ShowBoundingBox();
	}

	int temp = m_ActiveDataType;
	for (int i=0; i<m_NumScalarDataTypes; i++)
	{
		SetScalarDataTypeTo(i);

		// Color Bar
		float range[2];
		GetScalarDataRange(range);
		if (range[0] == range[1])
		{
			range[0]--;
			range[1]++;
		}
		SetColorBarEndPoints(range[0], range[1]);
		UseLinearColorBar();
		SetColorBarNumberOfLabels(5);
		SetColorBarLabelPrecision(3);

		// Solid
		SetSolidDisplayToSmooth();
		SetSolidThresholdLimits(range[0], range[1]);
		SolidThresholdOff();

		// Isosurface
		float value = (range[0] + range[1])/2;
		SetCustomIsosurfaces(1, &value);
		SetRegularIsosurfaces(5, range[0], range[1]);
	}
	SetScalarDataTypeTo(temp);

	// Vector
	if (m_DataSource->GetVectorArray() != 0)
	{
		const int *vdim = m_DataSource->GetVectorGridDimensions();
		SubsampleVectors(0, vdim[0]-1, 0, vdim[1]-1, 0, vdim[2]-1, 1, 1, 1);
		SetVectorSizeToOptimal();
		if (m_VectorLog10Transform)
		{
			SetVectorThresholdLimits(m_VectorLogMagnitudeRange[0], m_VectorLogMagnitudeRange[1]);
		}
		{
			SetVectorThresholdLimits(m_VectorMagnitudeRange[0], m_VectorMagnitudeRange[1]);
		}
		VectorThresholdOff();
	}
	else
	{
		SubsampleVectors(0, 0, 0, 0, 0, 0, 1, 1, 1);
		SetVectorScaleFactor(0);
		m_CubeSource->SetXLength(0);
		m_CubeSource->SetYLength(0);
		m_CubeSource->SetZLength(0);
	}
	SetVectorColor(0, 0, 0);
	m_VectorBounds[0] = 0;
	m_VectorBounds[1] = 1;
	m_VectorBounds[2] = 0;
	m_VectorBounds[3] = 1;
	m_VectorBounds[4] = 0;
	m_VectorBounds[5] = 1;
	m_VectorClippingAngle = 0;

	// Pathlines
	SetPathlineRepresentationToLine();
	SetPathlineTubeDiameter(1);
	SetPathlineTimeClippingMode(0);
	if (m_DataSource->GetNumberOfPathlines() > 0)
	{
		float range[2];
		GetPathlineTimeRange(range);
		SetPathlineColorBarEndPoints(range[0], range[1]);
		SetPathlineTimeClippingRange(range[0], range[1]);
	}
	else
	{
		SetPathlineColorBarEndPoints(0, 1);
		SetPathlineTimeClippingRange(0, 0);
	}

	// Particles
	float bounds[6];
	m_BoundingBox->GetBounds(bounds);
	float dx = bounds[1] - bounds[0];
	float dy = bounds[3] - bounds[2];
	float dz = bounds[5] - bounds[4];
	float diag = sqrt(dx*dx + dy*dy + dz*dz);
	m_Particles->SetDefaultGlyphSize(diag/600);

	// Overlay
	SetOverlayType(MV_ESRI_SHAPEFILE_OVERLAY);
	SetOverlayCoordinatesAtGridOrigin(0, 0);
	SetOverlayToGridScale(1);
	SetOverlayAngle(0);
	SetOverlayDrape(0);
	SetOverlayTrim(0);
	SetOverlayCrop(0);
	SetOverlayElevation(0);
	SetOverlayDrapeGap(0);

}

void mvManager::SetImmediateModeRendering(int b)
{
	m_SolidMapper->SetImmediateModeRendering(b);
	m_IsosurfaceMapper->SetImmediateModeRendering(b);
	m_VectorMapper->SetImmediateModeRendering(b);
	m_VectorGlyphMapper->SetImmediateModeRendering(b);
	m_GridShell->SetImmediateModeRendering(b);
	m_MeshLines->SetImmediateModeRendering(b);
	m_ExternalMeshVector->SetImmediateModeRendering(b);
	m_ExternalLinesVector->SetImmediateModeRendering(b);
	m_BoundingBox->SetImmediateModeRendering(b);
	m_Axes->SetImmediateModeRendering(b);
	for (int i=0; i<3; i++)
	{
		m_GridLines[i]->SetImmediateModeRendering(b);
	}
	m_GridOutline->SetImmediateModeRendering(b);
	m_Particles->SetImmediateModeRendering(b);
}

void mvManager::SetReleaseDataFlag(int b)
{
	int i;
	m_GridShellClipMin->SetReleaseDataFlag(b);
	m_GridShellClipMax->SetReleaseDataFlag(b);
	m_SmoothSolid->SetReleaseDataFlag(b);
	m_CroppedSolid->SetReleaseDataFlag(b);
	m_Faces->SetReleaseDataFlag(b);
	m_FacesClipMin->SetReleaseDataFlag(b);
	m_FacesClipMax->SetReleaseDataFlag(b);
	for (i=0; i<3; i++)
	{
		m_IsosurfaceCutter[i]->SetReleaseDataFlag(b);
	}
	for (i=0; i<6; i++)
	{
		m_Cropper[i]->SetReleaseDataFlag(b);
	}
	for (i=0; i<24; i++)
	{
		m_FaceCrop[i]->SetReleaseDataFlag(b);
	}
}

void mvManager::AssumeAllCellsAreActive(BOOL b)
{
	bool StructuredGrid;
	if (m_DataSource == 0)
	{
		StructuredGrid = FALSE;
	}
	else
	{
//		int iStructuredGrid = m_DataSource->GetGridType();
		StructuredGrid = GetIsStructuredGrid();
//		StructuredGrid = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
//			|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
//			|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);
	}

	// debugging code
	// StructuredGrid = FALSE;

	int i;
	if (b)   // all cells active
	{
		if (IsScalarSubgridOn())
		{
			m_GridShell->SetInput(m_ScalarSubDataSet->GetOutput());
			if (StructuredGrid)
			{
				m_GridOutline->SetInput(m_ScalarSubDataSet->GetOutput());
			}
			else
			{
				m_GridOutline->SetInput(0);
			}
			m_Isosurface->SetInput(m_ScalarSubDataSet->GetOutput());
			m_SmoothSolidIsosurface->SetInput(m_ScalarSubDataSet->GetOutput());
			for (i=0; i<6; i++)
			{
				m_ExtractFace[i]->SetInput(m_ScalarSubDataSet->GetOutput());
			}
		}
		else
		{
			m_GridShell->SetInput(m_ScalarDataSet);
			//m_GridOutline->SetInput(m_ScalarDataSet);
			if (StructuredGrid)
			{
				m_GridOutline->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
			}
			else
			{
				m_GridOutline->SetInput(0);
			}
			m_Isosurface->SetInput(m_ScalarDataSet);
			m_SmoothSolidIsosurface->SetInput(m_ScalarDataSet);
			for (i=0; i<6; i++)
			{
				m_ExtractFace[i]->SetInput(m_ScalarDataSet);
			}
		}
	}
	else
	{
		// not all cells are active
		m_GridShell->SetInput(m_ActiveScalarDataSet->GetOutput());
		m_Isosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
		m_SmoothSolidIsosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
		for (i=0; i<6; i++)
		{
			m_ExtractFace[i]->SetInput(m_ActiveScalarDataSet->GetOutput());
		}
	}
	m_GridLines[0]->AssumeAllCellsAreActive(b);
	m_GridLines[1]->AssumeAllCellsAreActive(b);
	m_GridLines[2]->AssumeAllCellsAreActive(b);
}

int mvManager::AreAllCellsActive() const
{
	return 	(m_GridShell->GetInput() == m_ScalarDataSet ||
		     m_GridShell->GetInput() == m_ScalarSubDataSet->GetOutput());
}

int mvManager::GetGridType() const
{
	if (m_DataSource != 0)
	{
		return m_DataSource->GetGridType();
	}
	else
	{
		return 0;
	}
}

bool mvManager::GetLayeredMesh() const
{
	if (m_DataSource != 0)
	{
		return m_DataSource->GetLayeredMesh();
	}
	else
	{
		return FALSE;
	}
}

const int *mvManager::GetScalarGridDimensions() const
{
	if (m_DataSource != 0)
	{
		return m_DataSource->GetScalarGridDimensions();
	}
	else
	{
		return 0;
	}
}

const int *mvManager::GetVectorGridDimensions() const
{
	if (m_DataSource != 0)
	{
		return m_DataSource->GetVectorGridDimensions();
	}
	else
	{
		return 0;
	}
}

int mvManager::GetNumberOfTimePoints() const
{
	if (!m_DataSource)
	{
		return 0;
	}
	else
	{
		return m_DataSource->GetNumberOfTimePoints();
	}
}

char **mvManager::GetTimePointLabels()
{
	if (!m_DataSource)
	{
		return 0;
	}
	else
	{
		return m_DataSource->GetTimePointLabels();
	}
}

int mvManager::GetNumberOfScalarDataTypes() const
{
	if (!m_DataSource)
	{
		return 0;
	}
	else
	{
		return m_DataSource->GetNumberOfScalarDataTypes();
	}
}

char **mvManager::GetDataTypeLabels() const
{
	if (!m_DataSource)
	{
		return 0;
	}
	else
	{
		return m_DataSource->GetDataTypeLabels();
	}
}

char *mvManager::GetActiveScalarDataName() const
{
	if (!m_DataSource)
	{
		return "";
	}
	else
	{
		return (m_DataSource->GetDataTypeLabels())[m_ActiveDataType];
	}
}

int mvManager::GetPrimaryScalarMode() const
{
	if (m_DataSource)
	{
		return m_DataSource->GetPrimaryScalarMode();
	}
	else
	{
		return 0;
	}
}

char *mvManager::GetModelName() const
{
	if (m_DataSource)
	{
		return m_DataSource->GetName();
	}
	else
	{
		return "";
	}
}

int mvManager::GetTimeLabelOption() const
{
	if (m_DataSource)
	{
		return m_DataSource->GetTimeLabelOption();
	}
	else
	{
		return 0;
	}
}

void mvManager::HideScalarData()
{
	m_SolidActor->VisibilityOff();
	m_IsosurfaceActor->VisibilityOff();
	m_CroppedAwayPiecesActor->VisibilityOff();
}

void mvManager::ShowScalarDataAsSolid()
{
	m_SolidActor->VisibilityOn();
	m_IsosurfaceActor->VisibilityOff();
	if (m_ShowCroppedAwayPieces)
	{
		m_CroppedAwayPiecesActor->VisibilityOn();
	}
	BuildPipelineForSolid();
}

int mvManager::IsSolidVisible() const
{
	return m_SolidActor->GetVisibility();
}

void mvManager::ShowScalarDataAsIsosurfaces()
{
	m_SolidActor->VisibilityOff();
	m_IsosurfaceActor->VisibilityOn();
	if (m_ShowCroppedAwayPieces)
	{
		m_CroppedAwayPiecesActor->VisibilityOn();
	}
	BuildPipelineForIsosurface();
}

int mvManager::AreIsosurfacesVisible() const
{
	return m_IsosurfaceActor->GetVisibility();
}

void mvManager::SetNumberOfColorBands(int numColorBands)
{
	m_NumberOfColorBands[m_ActiveDataType] = numColorBands;
	UpdateColorBands();
}

int mvManager::GetNumberOfColorBands() const
{
	if (m_NumberOfColorBands != 0) 
	{
		return m_NumberOfColorBands[m_ActiveDataType];
	}
	else
	{
		return 0;
	}
}

void mvManager::UpdateColorBands()
{
	if (m_NumberOfColorBands[m_ActiveDataType] == 0)
	{
		m_ColorBandFilter->SetValues(0, 0);
	}
	else if (m_ColorBarDataSource == 0)
	{
		float *values = new float[m_NumberOfColorBands[m_ActiveDataType] + 1];
		float v1 = GetColorBarValueBlue();
		float v2 = GetColorBarValueRed();
		if (v1 > v2)
		{
			float temp = v1;
			v1 = v2;
			v2 = temp;
		}
		if (!IsColorBarLinear())
		{
			v1 = log(v1)/log(10);
			v2 = log(v2)/log(10);
		}
		float delta = (v2 - v1) / m_NumberOfColorBands[m_ActiveDataType];
		values[0] = v1;
		for (int i=1; i<=m_NumberOfColorBands[m_ActiveDataType]; i++)
		{
			values[i] = v1 + i*delta;
		}
		if (!IsColorBarLinear())
		{
			for (int i=0; i<=m_NumberOfColorBands[m_ActiveDataType]; i++)
			{
				values[i] = pow(10, values[i]);
			}
		}
		m_ColorBandFilter->SetValues(values, m_NumberOfColorBands[m_ActiveDataType] + 1);
		delete [] values;
	}


	if (m_SolidDisplayMode[m_ActiveDataType] == MV_SMOOTH)
	{
		m_SolidMapper->SetScalarModeToUsePointData();
	}
	else  // For both block and banded display modes, the mapper scalar mode uses cell data.
	{
		m_SolidMapper->SetScalarModeToUseCellData();
	}
	BuildPipelineForSolid();
}

void mvManager::SetScalarSubgridExtent(int imin, int imax, int jmin, int jmax, int kmin, int kmax)
{
	m_ScalarSubDataSet->SetVOI(imin, imax, jmin, jmax, kmin, kmax);
	m_ModelFeatures->SetSubgridExtent(imin, imax, jmin, jmax, kmin, kmax);
}

const int *mvManager::GetScalarSubgridExtent()
{
	return m_ScalarSubDataSet->GetVOI();
}

void mvManager::ScalarSubgridOn()
{
	bool StructuredGrid;
	if (m_DataSource == 0)
	{
		StructuredGrid = FALSE;
	}
	else
	{
//		int iStructuredGrid = m_DataSource->GetGridType();
		StructuredGrid = GetIsStructuredGrid();
//		StructuredGrid = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
//			|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
//			|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);
	}

	// Debugging code
	// StructuredGrid = FALSE;

	if(!StructuredGrid)
	{
		return;
	}

	const int *sdim = m_DataSource->GetScalarGridDimensions();
	int *v = m_ScalarSubDataSet->GetVOI();
	const int *g0 = m_GridLines[0]->GetExtent();
	const int *g1 = m_GridLines[1]->GetExtent();
	const int *g2 = m_GridLines[2]->GetExtent();
	if (v[0] == g2[0] && v[1] == g2[1] && v[2] == g2[2] && v[3] == g2[3]
		&& v[4] == g1[4] && v[5] == g1[5]) 
	{
		return;
	}
	

	// Work with the sub data set.
	m_ActiveScalarDataSet->SetInput(m_ScalarSubDataSet->GetOutput());
	m_BlockySolidThreshold->SetInput(m_ScalarSubDataSet->GetOutput());
	if (AreAllCellsActive())
	{
		m_GridShell->SetInput(m_ScalarSubDataSet->GetOutput());
		m_GridOutline->SetInput(m_ScalarSubDataSet->GetOutput());
		m_Isosurface->SetInput(m_ScalarSubDataSet->GetOutput());
		m_SmoothSolidIsosurface->SetInput(m_ScalarSubDataSet->GetOutput());
		for (int i=0; i<6; i++)
		{
			m_ExtractFace[i]->SetInput(m_ScalarSubDataSet->GetOutput());
		}
	}
	else
	{
		m_GridShell->SetInput(m_ActiveScalarDataSet->GetOutput());
		m_Isosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
		m_SmoothSolidIsosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
		for (int i=0; i<6; i++)
		{
			m_ExtractFace[i]->SetInput(m_ActiveScalarDataSet->GetOutput());
		}
	}
	int p = max(v[0], min(g0[0], v[1]));
	m_GridLines[0]->SetExtent(p, p, v[2], v[3], v[4], v[5]);
	p = max(v[2], min(g1[2], v[3]));
	m_GridLines[1]->SetExtent(v[0], v[1], p, p, v[4], v[5]);
	p = max(v[4], min(g2[4], v[5]));
	m_GridLines[2]->SetExtent(v[0], v[1], v[2], v[3], p, p);

	// Recompute bounds for the bounding box
	float bounds[6];
	m_ActiveScalarDataSet->Update();
	m_ActiveScalarDataSet->GetOutput()->ComputeBounds();
	m_ActiveScalarDataSet->GetOutput()->GetBounds(bounds);
	m_BoundingBox->SetBounds(bounds);

	// Crop the pathlines
	if (HasPathlineData())
	{
		m_Pathlines->DoCrop(bounds);
	}

	// Update the axes position.
	const float *s = GetScale();
	bounds[0] *= s[0];
	bounds[1] *= s[0];
	bounds[2] *= s[1];
	bounds[3] *= s[1];
	bounds[4] *= s[2];
	bounds[5] *= s[2];
	m_Axes->SetDefaultPositions(bounds);

	// Update the cropping positions
	UpdateCrop();

	// Limit the vector display
	if (HasVectorData())
	{
		int *vectorVOI = m_ExtractVector->GetVOI();
		int *scalarVOI = m_ScalarSubDataSet->GetVOI();
		int v0, v1, v2, v3, v4, v5;
		if (vectorVOI[0] > scalarVOI[1]-1)
		{
			v0 = scalarVOI[1]-1;
			v1 = v0;
		}
		else if (vectorVOI[1] < scalarVOI[0])
		{
			v0 = scalarVOI[0];
			v1 = v0;
		}
		else
		{
			v0 = max(scalarVOI[0], vectorVOI[0]);
			v1 = min(scalarVOI[1]-1, vectorVOI[1]);
		}
		if (vectorVOI[2] > scalarVOI[3]-1)
		{
			v2 = scalarVOI[3]-1;
			v3 = v2;
		}
		else if (vectorVOI[3] < scalarVOI[2])
		{
			v2 = scalarVOI[2];
			v3 = v2;
		}
		else
		{
			v2 = max(scalarVOI[2], vectorVOI[2]);
			v3 = min(scalarVOI[3]-1, vectorVOI[3]);
		}
		if (vectorVOI[4] > scalarVOI[5]-1)
		{
			v4 = scalarVOI[5]-1;
			v5 = v4;
		}
		else if (vectorVOI[5] < scalarVOI[4])
		{
			v4 = scalarVOI[4];
			v5 = v4;
		}
		else
		{
			v4 = max(scalarVOI[4], vectorVOI[4]);
			v5 = min(scalarVOI[5]-1, vectorVOI[5]);
		}

		m_ExtractVector->SetVOI(v0, v1, v2, v3, v4, v5);
	}

	// Limit the model features
	m_ModelFeatures->SubgridOn();

	// Crop the overlay
	m_Overlay->SubgridOn();

	if (m_SolidActor->GetVisibility())
	{
		BuildPipelineForSolid();
	}
	else if (m_IsosurfaceActor->GetVisibility())
	{
		BuildPipelineForIsosurface();
	}
}


void mvManager::ScalarSubgridOff()
{
	
//	int iStructuredGrid = m_DataSource->GetGridType();
	bool StructuredGrid = GetIsStructuredGrid();
//	bool StructuredGrid = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
//		|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
//		|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);

	// Debugging code
	// StructuredGrid = FALSE;

	// Work with the full scalar data set
	m_ActiveScalarDataSet->SetInput(m_ScalarDataSet);
	m_BlockySolidThreshold->SetInput(m_ScalarDataSet);
	if (AreAllCellsActive())
	{
		m_GridShell->SetInput(m_ScalarDataSet);
		//m_GridOutline->SetInput(m_ScalarDataSet);
		if (StructuredGrid)
		{
			m_GridOutline->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
		}
		else
		{
			m_GridOutline->SetInput(0);
		}

		m_Isosurface->SetInput(m_ScalarDataSet);
		m_SmoothSolidIsosurface->SetInput(m_ScalarDataSet);
		for (int i=0; i<6; i++)
		{
			m_ExtractFace[i]->SetInput(m_ScalarDataSet);
		}
	}
	else
	{
		m_GridShell->SetInput(m_ActiveScalarDataSet->GetOutput());
		m_Isosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
		m_SmoothSolidIsosurface->SetInput(m_ActiveScalarDataSet->GetOutput());
		for (int i=0; i<6; i++)
		{
			m_ExtractFace[i]->SetInput(m_ActiveScalarDataSet->GetOutput());
		}
	}


	const int *sdim = m_DataSource->GetScalarGridDimensions();
	const int *g = m_GridLines[0]->GetExtent();
	m_GridLines[0]->SetExtent(g[0], g[0], 0, sdim[1]-1, 0, sdim[2]-1);
	g = m_GridLines[1]->GetExtent();
	m_GridLines[1]->SetExtent(0, sdim[0]-1, g[2], g[2], 0, sdim[2]-1);
	g = m_GridLines[2]->GetExtent();
	m_GridLines[2]->SetExtent(0, sdim[0]-1, 0, sdim[1]-1, g[4], g[4]);

	// Recompute the bounds for the bounding box
	float bounds[6];
	if (AreAllCellsActive())
	{
		m_ScalarDataSet->GetBounds(bounds);
		m_BoundingBox->SetBounds(bounds);
	}
	else
	{
		m_ActiveScalarDataSet->Update();
		m_ActiveScalarDataSet->GetOutput()->ComputeBounds();
		m_ActiveScalarDataSet->GetOutput()->GetBounds(bounds);
		m_BoundingBox->SetBounds(bounds);
	}

	// Update the axes position.
	const float *s = GetScale();
	bounds[0] *= s[0];
	bounds[1] *= s[0];
	bounds[2] *= s[1];
	bounds[3] *= s[1];
	bounds[4] *= s[2];
	bounds[5] *= s[2];
	m_Axes->SetDefaultPositions(bounds);

	m_Pathlines->CropOff();

	m_ModelFeatures->SubgridOff();
	m_Overlay->SubgridOff();

	// Update the cropping bounds
	UpdateCrop();

	if (m_SolidActor->GetVisibility())
	{
		BuildPipelineForSolid();
	}
	else if (m_IsosurfaceActor->GetVisibility())
	{
		BuildPipelineForIsosurface();
	}
}

int mvManager::IsScalarSubgridOn() const
{
	return (m_ActiveScalarDataSet->GetInput() == m_ScalarSubDataSet->GetOutput());
}

void mvManager::ShowVectors()
{
	m_VectorActor->VisibilityOn();
	if (m_VectorGlyphActivated)
	{
		m_VectorGlyphActor->VisibilityOn();
	}
}

void mvManager::HideVectors()
{
	m_VectorActor->VisibilityOff();
	m_VectorGlyphActor->VisibilityOff();
}

int mvManager::AreVectorsVisible() const
{
	return m_VectorActor->GetVisibility();
}

float mvManager::GetVectorLineWidth()
{
  return m_VectorActor->GetProperty()->GetLineWidth();
}

void mvManager::SetVectorLineWidth(float width)
{
	m_VectorActor->GetProperty()->SetLineWidth(width);
}

void mvManager::SetVectorColor(float red, float green, float blue)
{
	m_VectorActor->GetProperty()->SetColor(red, green, blue);
	m_VectorGlyphActor->GetProperty()->SetColor(red, green, blue);
}

const float *mvManager::GetVectorColor() const
{
	return m_VectorActor->GetProperty()->GetColor();
}

void mvManager::SetVectorScaleFactor(float scaleFactor)
{
	m_HedgeHog->SetScaleFactor(scaleFactor);
}

float mvManager::GetVectorScaleFactor() const

{
	return m_HedgeHog->GetScaleFactor();
}

float mvManager::ComputeOptimalVectorSize()
{
	float bounds[6];
	m_BoundingBox->GetBounds(bounds);
	float dx = bounds[1] - bounds[0];
	float dy = bounds[3] - bounds[2];
	float dz = bounds[5] - bounds[4];
	float diag = sqrt(dx*dx + dy*dy + dz*dz);
	float upperRange;
	if (m_VectorLog10Transform)
	{
		upperRange = m_VectorLogMagnitudeRange[1];
	}
	else
	{
		upperRange = m_VectorMagnitudeRange[1];
	}
	if (upperRange > 0)
	{
		float f = diag / upperRange / 10;
		int e = floor(log(f)/log(10));
		int d = ceil(f*pow(10, -e));
		return d*pow(10, e);
	}
	else
	{
		return 0;
	}
}

void mvManager::SetVectorSizeToOptimal()
{
	SetVectorScaleFactor(ComputeOptimalVectorSize());

	float bounds[6];
	m_BoundingBox->GetBounds(bounds);
	float dx = bounds[1] - bounds[0];
	float dy = bounds[3] - bounds[2];
	float dz = bounds[5] - bounds[4];
	float diag = sqrt(dx*dx + dy*dy + dz*dz);

	float cubeSize = diag/300;
	float *s = m_SolidActor->GetScale();
	m_CubeSource->SetXLength(cubeSize/s[0]);
	m_CubeSource->SetYLength(cubeSize/s[1]);
	m_CubeSource->SetZLength(cubeSize/s[2]);
}

void mvManager::VectorThresholdOn()
{
	m_DoVectorThreshold = 1;
	m_HedgeHog->SetInput(m_VectorThreshold->GetOutput());
	m_VectorGlyph->SetInput(m_VectorThreshold->GetOutput());
}

void mvManager::VectorThresholdOff()
{
	m_DoVectorThreshold = 0;
	m_HedgeHog->SetInput(m_ActiveVectorDataSet->GetOutput());
	m_VectorGlyph->SetInput(m_ActiveVectorDataSet->GetOutput());
}

int mvManager::IsVectorThresholdOn() const
{
	return m_DoVectorThreshold;
}

void mvManager::SetVectorThresholdLimits(float minValue, float maxValue)
{
	if (minValue < 0)
	{
		minValue = 0;
	}
	// a hack to make the pipeline work
	m_VectorThreshold->ThresholdBetween(-20, -10);
	if (m_VectorLog10Transform)
	{
		float minRange = m_MinPositiveVector/2;
		if (minRange <= 0)
		{
			minRange = 1;
		}
		if (minValue > 0)
		{
			minValue = log10(minValue/minRange);
		}
		if (maxValue > 0)
		{
			maxValue = log10(maxValue/minRange);
		}
	}
	m_VectorThreshold->ThresholdBetween(minValue, maxValue);
}

void mvManager::GetVectorThresholdLimits(float *limits) const
{
	limits[0] = m_VectorThreshold->GetLowerThreshold();
	limits[1] = m_VectorThreshold->GetUpperThreshold();
}

void mvManager::CropVectors(float xmin, float xmax, 
	float ymin, float ymax, float zmin, float zmax, int cropAngle)
{
	int ShouldCrop = ((xmin != 0.) || (xmax != 1.) || (ymin != 0.) || (ymax != 1.) || (zmin != 0.) || (zmax != 1.));

	float bounds[6];
	m_BoundingBox->GetBounds(bounds);

	m_VectorClippingXMin = xmin;
	m_VectorClippingXMax = xmax;
	m_VectorClippingYMin = ymin;
	m_VectorClippingYMax = ymax;
	m_VectorClippingZMin = zmin;
	m_VectorClippingZMax = zmax;
	if (cropAngle > 45)
	{
		m_VectorClippingAngle = 45;
	}
	else if (cropAngle < -45)
	{
		m_VectorClippingAngle = -45;
	}
	else
	{
		m_VectorClippingAngle = cropAngle;
	}

	m_VectorBounds[0] = xmin;
	m_VectorBounds[1] = xmax;
	m_VectorBounds[2] = ymin;
	m_VectorBounds[3] = ymax;
	m_VectorBounds[4] = zmin;
	m_VectorBounds[5] = zmax;

	bool StructuredGrid = GetIsStructuredGrid();
	if (ShouldCrop)
	{
		m_ClipVectors->SetModelBounds(bounds);
		m_ClipVectors->SetBounds(m_VectorBounds);
		m_ClipVectors->SetAngle(m_VectorClippingAngle);
		if (StructuredGrid)
		{
			m_CropVectors->SetInput(m_ExtractVector->GetOutput());
		}
		else
		{
			m_CropVectors->SetInput(m_ExtractIrregularMeshVector->GetOutput());
		}
		m_ActiveVectorDataSet->SetInput(m_CropVectors->GetOutput());
	}
	else
	{
		if (StructuredGrid)
		{
			m_ActiveVectorDataSet->SetInput(m_ExtractVector->GetOutput());
		}
		else
		{
			m_ActiveVectorDataSet->SetInput(m_ExtractIrregularMeshVector->GetOutput());
		}
	};
}


void mvManager::SubsampleVectors(int imin, int imax, int jmin, int jmax,
					int kmin, int kmax, int irate, int jrate, int krate)
{
	m_ExtractVector->SetVOI(imin, imax, jmin, jmax, kmin, kmax);
	m_ExtractVector->SetSampleRate(irate, jrate, krate);

	m_ExtractIrregularMeshVector->SetOnRatio(krate);
}

const int *mvManager::GetVectorSubsampleExtents() const
{
	return m_ExtractVector->GetVOI();
}

const int *mvManager::GetVectorSubsampleRate() const
{
	return m_ExtractVector->GetSampleRate();
}

void mvManager::ActivateVectorGlyph(int active)
{
	m_VectorGlyphActivated = active;
	if (m_VectorActor->GetVisibility())
	{
		if (active)
		{	
			m_VectorGlyphActor->VisibilityOn();
		}
		else
		{
			m_VectorGlyphActor->VisibilityOff();
		}
	}
}

int mvManager::IsVectorGlyphActivated() const
{
	return m_VectorGlyphActivated;
}

void mvManager::EnlargeVectorGlyph() 
{
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()*1.5);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()*1.5);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()*1.5);
}

void mvManager::ShrinkVectorGlyph() 
{
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()/1.5);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()/1.5);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()/1.5);
}

void mvManager::ShowPathlines()
{
	m_Pathlines->VisibilityOn();
}

void mvManager::HidePathlines()
{
	m_Pathlines->VisibilityOff();
}

int mvManager::ArePathlinesVisible() const
{
	return m_Pathlines->GetVisibility();
}

void mvManager::SetPathlineRepresentationToLine()
{
	m_Pathlines->SetRepresentationToLine();
}

void mvManager::SetPathlineRepresentationToTube()
{
	m_Pathlines->SetRepresentationToTube();
}

int mvManager::GetPathlineRepresentation() const
{
	return m_Pathlines->GetRepresentation();
}

void mvManager::GetPathlineTimeRange(float *range) const
{
	if (m_DataSource->GetNumberOfPathlines() > 0)
	{
		float *r = m_PathlineScalars->GetRange();
		range[0] = r[0];
		range[1] = r[1];
	}
}

void mvManager::SetPathlineTubeDiameter(float diameter)
{
	m_Pathlines->SetNormalizedTubeDiameter(diameter);
}

float mvManager::GetPathlineTubeDiameter() const
{
	return m_Pathlines->GetNormalizedTubeDiameter();
}

void mvManager::SetPathlineTimeClippingMode(int mode)
{
	m_PathlineTimeClippingMode = mode;
	switch (mode)
	{
	case 0:
		// no time clipping.
		if (m_DataSource->GetPathlineScalarMode() != MP_TRAVEL_TIME)
		{
			m_DataSource->SetPathlineScalarMode(MP_TRAVEL_TIME);
			UpdatePathlineScalars();
		}
		m_Pathlines->TimeClippingOff();
		break;
	case 1:
		// clip to starting time and current time.
		{
			if (m_DataSource->GetPathlineScalarMode() != MP_TRAVEL_TIME)
			{
				m_DataSource->SetPathlineScalarMode(MP_TRAVEL_TIME);
				UpdatePathlineScalars();
			}

			
			char **labels = m_DataSource->GetTimePointLabels();
			float clipTimeMax = (float) atof(labels[m_TimePointIndex]);
			m_Pathlines->SetTimeClippingRange(0, clipTimeMax);
			m_Pathlines->TimeClippingOn();
			break;
		}
	case 2:
		// clip to specified starting and ending times.
		if (m_DataSource->GetPathlineScalarMode() != MP_TRAVEL_TIME)
		{
			m_DataSource->SetPathlineScalarMode(MP_TRAVEL_TIME);
			UpdatePathlineScalars();
		}
		m_Pathlines->SetTimeClippingRange(m_PathlineClipTimeMin, m_PathlineClipTimeMax);
		m_Pathlines->TimeClippingOn();
		break;
	case 3:
		// clip to minimum time.
		if (m_DataSource->GetPathlineScalarMode() != MP_MIN_TRAVEL_TIME)
		{
			m_DataSource->SetPathlineScalarMode(MP_MIN_TRAVEL_TIME);
			UpdatePathlineScalars();
		}
		m_Pathlines->SetTimeClippingRange(m_PathlineClipTimeMin, m_PathlineClipTimeMax);
		m_Pathlines->TimeClippingOn();
		break;
	case 4:
		// clip to maximum time.
		if (m_DataSource->GetPathlineScalarMode() != MP_MAX_TRAVEL_TIME)
		{
			m_DataSource->SetPathlineScalarMode(MP_MAX_TRAVEL_TIME);
			UpdatePathlineScalars();
		}
		m_Pathlines->SetTimeClippingRange(m_PathlineClipTimeMin, m_PathlineClipTimeMax);
		m_Pathlines->TimeClippingOn();
		break;
	}
}

void mvManager::SetPathlineTimeClippingRange(float minTime, float maxTime)
{
	m_PathlineClipTimeMin = minTime;
	m_PathlineClipTimeMax = maxTime;
	m_Pathlines->SetTimeClippingRange(minTime, maxTime);
}

float mvManager::GetPathlineClipTimeMin() const
{
	return m_PathlineClipTimeMin;
}

float mvManager::GetPathlineClipTimeMax() const
{
	return m_PathlineClipTimeMax;
}

float mvManager::GetPathlineTimeBlue() const
{
	return m_Pathlines->GetTimeBlue();
}

float mvManager::GetPathlineTimeRed() const
{
	return m_Pathlines->GetTimeRed();
}

void mvManager::SetPathlineColorBarEndPoints(float valueBlue, float valueRed)
{
	m_Pathlines->SetColorBarEndPoints(valueBlue, valueRed);
}

int mvManager::GetPathlineLogTransform() const
{
	return m_Pathlines->GetLogTransform();
}

void mvManager::SetPathlineLogTransform(int Value)
{
	m_Pathlines->SetLogTransform(Value);
}

void mvManager::ShowModelFeatures()
{
	m_ModelFeatures->VisibilityOn();
}

void mvManager::HideModelFeatures()
{
	m_ModelFeatures->VisibilityOff();
}

int mvManager::AreModelFeaturesVisible() const
{
	return m_ModelFeatures->GetVisibility();
}

void mvManager::SetModelFeatureDisplayOrder(int *displayOrder)
{
	m_ModelFeatures->SetDisplayOrder(displayOrder);
}

int mvManager::HasModelFeatures() const
{
	return (m_DataSource && m_DataSource->GetModelFeatureArray());
}

int mvManager::GetNumberOfModelFeatureTypes() const
{
	return m_DataSource->GetNumberOfModelFeatureTypes();
}

const char *mvManager::GetModelFeatureLabels() const
{
	return m_DataSource->GetModelFeatureLabels();
}

int *mvManager::GetModelFeatureDisplayOrder()
{
	return m_ModelFeatures->GetDisplayOrder();
}

void mvManager::EnlargeModelFeatureGlyphs()
{
	m_ModelFeatures->EnlargeGlyphs();
}

void mvManager::ShrinkModelFeatureGlyphs()
{
	m_ModelFeatures->ShrinkGlyphs();
}

int mvManager::GetModelFeatureDisplayMode() const
{
	return m_DataSource->GetModelFeatureDisplayMode();
}

void mvManager::SetModelFeatureColor(char *modelFeatureName, float* rgba)
{
	char *names = m_DataSource->GetModelFeatureLabels();
	for (int i=0; i<m_DataSource->GetNumberOfModelFeatureTypes(); i++)
	{
		if (strncmp(names + 40*i, modelFeatureName, 40) == 0)
		{
			m_ModelFeatures->SetColor(i, rgba);
			return;
		}
	}
}

void mvManager::GetModelFeatureColor(char *modelFeatureName, float *rgba)
{
	char *names = m_DataSource->GetModelFeatureLabels();
	for (int i=0; i<m_DataSource->GetNumberOfModelFeatureTypes(); i++)
	{
		if (strncmp(names + 40*i, modelFeatureName, 40) == 0)
		{
			m_ModelFeatures->GetColor(i, rgba);
			return;
		}
	}
}

int mvManager::IsGridShellVisible() const
{
	return m_GridShell->GetVisibility();
}

void mvManager::ShowGridShell()
{
	m_GridShell->VisibilityOn();
}

void mvManager::HideGridShell()
{
	m_GridShell->VisibilityOff();
}

void mvManager::SetGridShellColor(float red, float green, float blue)
{
	m_GridShell->SetColor(red, green, blue);
}

void mvManager::SetGridShellOpacity(float opacity)
{
	m_GridShell->SetOpacity(opacity);
}

const float *mvManager::GetGridShellColor() const
{
	return m_GridShell->GetColor();
}

float mvManager::GetGridShellOpacity() const
{
	return m_GridShell->GetOpacity();
}

int mvManager::IsBoundingBoxVisible() const
{
	return m_BoundingBox->GetVisibility();
}

void mvManager::ShowBoundingBox()
{
	m_BoundingBox->VisibilityOn();
}

void mvManager::HideBoundingBox()
{
	m_BoundingBox->VisibilityOff();
}


void mvManager::SetBoundingBoxColor(float r, float g, float b)
{
	m_BoundingBox->SetColor(r, g, b);
}

const float *mvManager::GetBoundingBoxColor() const
{
	return m_BoundingBox->GetColor();
}

int mvManager::AreActivatedGridLinesVisible() const
{
	return m_ActivatedGridLinesVisibility;
}

void mvManager::ShowActivatedGridLines()
{
	m_ActivatedGridLinesVisibility = 1;
	for (int i=0; i<3; i++)
	{
		if (m_GridLinesActivated[i])
		{
			m_GridLines[i]->VisibilityOn();
		}
	}
	if (m_GridOutlineActivated)
	{
		m_GridOutline->VisibilityOn();
	}
}

void mvManager::HideGridLines()
{
	m_ActivatedGridLinesVisibility = 0;
	for (int i=0; i<3; i++)
	{
		m_GridLines[i]->VisibilityOff();
	}
	m_GridOutline->VisibilityOff();
}

void mvManager::ActivateGridLines(int i)
{
	if ((m_DataSource == 0) || (GetGridType() != MV_UNSTRUCTED_GRID))
	{
		m_GridLinesActivated[i] = 1;
		if (m_ActivatedGridLinesVisibility == 1)
		{
		m_GridLines[i]->VisibilityOn();
		}
	}
	else
	{
		if (i == 2)
		{
			m_MeshActivated = TRUE;
			m_MeshLines->VisibilityOn();
		}
	}
}

void mvManager::DeactivateGridLines(int i)
{
	m_GridLinesActivated[i] = 0;
	m_GridLines[i]->VisibilityOff();

	if ((m_DataSource != 0) || (GetGridType() == MV_UNSTRUCTED_GRID))
	{
		
		if (i == 2)
		{
			m_MeshActivated = FALSE;
			m_MeshLines->VisibilityOff();
		}
	}
}

void mvManager::SetGridLinePositions(int posX, int posY, int posZ)
{ 
	if (IsScalarSubgridOn())
	{
		const int *g = m_GridLines[0]->GetExtent();
		m_GridLines[0]->SetExtent(posX, posX, g[2], g[3], g[4], g[5]);
		g = m_GridLines[1]->GetExtent();
		m_GridLines[1]->SetExtent(g[0], g[1], posY, posY, g[4], g[5]);
		g = m_GridLines[2]->GetExtent();
		m_GridLines[2]->SetExtent(g[0], g[1], g[2], g[3], posZ, posZ);
	} 
	else
	{
		const int *sdim = m_DataSource->GetScalarGridDimensions();
		m_GridLines[0]->SetExtent(posX, posX, 0, sdim[1]-1, 0, sdim[2]-1);
		m_GridLines[1]->SetExtent(0, sdim[0]-1, posY, posY,0, sdim[2]-1);
		m_GridLines[2]->SetExtent(0, sdim[0]-1, 0, sdim[1]-1, posZ, posZ);
		m_MeshLines->SetSelectedLayer(posZ);
	}
}

void mvManager::SetGridLineColor(float r, float g, float b)
{
	for (int i=0; i<3; i++)
	{
		m_GridLines[i]->SetColor(r, g, b);
	}
	m_GridOutline->SetColor(r, g, b);
	m_MeshLines->SetColor(r, g, b);
	m_ExternalMeshVector->SetColor(r, g, b);
	m_ExternalLinesVector->SetColor(r, g, b);
}

void mvManager::ActivateGridOutline()
{
	m_GridOutlineActivated = 1;
	if (m_ActivatedGridLinesVisibility == 1)
	{
		m_GridOutline->VisibilityOn();
	}
}

void mvManager::DeactivateGridOutline()
{
	m_GridOutlineActivated = 0;
	m_GridOutline->VisibilityOff();
}

int mvManager::AreGridLinesActive(int i) const
{
	if ((m_DataSource != 0) && (GetGridType() == MV_UNSTRUCTED_GRID))
	{
		if (i == 2)
		{
			return m_MeshActivated;
		}
		else
		{
			return FALSE;
		}
	}
	else
	{
		return m_GridLinesActivated[i];
	}
}

int mvManager::IsGridOutlineActive() const
{
	return m_GridOutlineActivated;
}

void mvManager::GetGridLinePositions(int *ibuff) const
{
	const int *e0 = m_GridLines[0]->GetExtent();
	const int *e1 = m_GridLines[1]->GetExtent();
	const int *e2 = m_GridLines[2]->GetExtent();
	ibuff[0] = e0[0];
	ibuff[1] = e1[2];
	ibuff[2] = e2[4];
}

const float *mvManager::GetGridLineColor() const
{
	return m_GridLines[0]->GetColor();
}

// Sets the xyz scale for actors. 
void mvManager::SetScale(float xScale, float yScale, float zScale)
{
	float s[3];
	m_SolidActor->GetScale(s);
	if (s[0] == xScale && s[1] == yScale && s[2] == zScale)
	{
		return;
	}
	m_SolidActor->SetScale(xScale, yScale, zScale);
	m_IsosurfaceActor->SetScale(xScale, yScale, zScale);
	m_CroppedAwayPiecesActor->SetScale(xScale, yScale, zScale);
	m_VectorActor->SetScale(xScale, yScale, zScale);
	m_VectorGlyphActor->SetScale(xScale, yScale, zScale);
	m_GridShell->SetScale(xScale, yScale, zScale);
	m_BoundingBox->SetScale(xScale, yScale, zScale);
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()*s[0]/xScale);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()*s[1]/yScale);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()*s[2]/zScale);
	for (int i=0; i<3; i++)
	{
		m_GridLines[i]->SetScale(xScale, yScale, zScale);
	}
	m_GridOutline->SetScale(xScale, yScale, zScale);
    m_MeshLines->SetScale(xScale, yScale, zScale);
	m_ExternalMeshVector->SetScale(xScale, yScale, zScale);
	m_ExternalLinesVector->SetScale(xScale, yScale, zScale);

	float bounds[6];
	m_BoundingBox->GetBounds(bounds);
	bounds[0] *= xScale;
	bounds[1] *= xScale;
	bounds[2] *= yScale;
	bounds[3] *= yScale;
	bounds[4] *= zScale;
	bounds[5] *= zScale;
	m_Axes->SetDefaultPositions(bounds);
	m_Pathlines->SetScale(xScale, yScale, zScale);
	m_ModelFeatures->SetScale(xScale, yScale, zScale);
	if (m_DataSource->GetVectorArray() != 0)
	{
		UpdateScaledVectorArray();
	}
	m_Particles->SetScale(xScale, yScale, zScale);
	m_Overlay->SetScale(xScale, yScale, zScale);
}

const float *mvManager::GetScale() const
{
	return m_SolidActor->GetScale();
}

int mvManager::AreAxesVisible() const
{
	return m_Axes->GetVisibility();
}

void mvManager::ShowAxes()
{
	m_Axes->VisibilityOn();
}

void mvManager::HideAxes()
{
	m_Axes->VisibilityOff();
}

void mvManager::SetAxesNormalizedSize(float sn)
{
	m_Axes->SetNormalizedSize(sn);
}

void mvManager::SetAxesNormalizedTubeDiameter(float dn)
{
	m_Axes->SetNormalizedTubeDiameter(dn);
}

void mvManager::SetAxesNormalizedPosition(float xn, float yn, float zn)
{
	m_Axes->SetNormalizedPosition(xn, yn, zn);
}

void mvManager::SetAxesRepresentationToTube()
{
	m_Axes->SetRepresentationToTube();
}

void mvManager::SetAxesRepresentationToLine()
{
	m_Axes->SetRepresentationToLine();
}

float mvManager::GetAxesNormalizedSize() const
{
	return m_Axes->GetNormalizedSize();
}

float mvManager::GetAxesNormalizedTubeDiameter() const
{
	return m_Axes->GetNormalizedTubeDiameter();
}

const float *mvManager::GetAxesNormalizedPosition() const
{
	return m_Axes->GetNormalizedPosition();
}

int mvManager::GetAxesRepresentation() const
{
	return m_Axes->GetRepresentation();
}


int mvManager::IsTimeLabelVisible() const
{
	return m_TimeLabel->GetVisibility();
}

void mvManager::ShowTimeLabel()
{
	m_TimeLabel->VisibilityOn();
}

void mvManager::HideTimeLabel()
{
	m_TimeLabel->VisibilityOff();
}

void mvManager::SetTimeLabelPosition(float x, float y)
{
	m_TimeLabel->SetPosition(x, y);
}

const float *mvManager::GetTimeLabelPosition() const
{
	return m_TimeLabel->GetPosition();
}

void mvManager::SetTimeLabelFontSize(int size)
{
	m_TimeLabel->SetFontSize(size);
}

int mvManager::GetTimeLabelFontSize() const
{
	return m_TimeLabel->GetFontSize();
}

void mvManager::SetTimeLabelColor(float r, float g, float b)
{
	m_TimeLabel->SetColor(r, g, b);
}

const float *mvManager::GetTimeLabelColor() const
{
	return m_TimeLabel->GetColor();
}

int mvManager::IsTitleVisible() const
{
	return m_Title->GetVisibility();
	return 0;
}

void mvManager::ShowTitle()
{
	m_Title->VisibilityOn();
}

void mvManager::HideTitle()
{
	m_Title->VisibilityOff();
}

void mvManager::SetTitlePosition(float x, float y)
{
	m_Title->SetPosition(x, y);
}

const float *mvManager::GetTitlePosition() const
{
	return m_Title->GetPosition();
}

void mvManager::SetTitleFontSize(int size)
{
	m_Title->SetFontSize(size);
}

int mvManager::GetTitleFontSize() const
{
	return m_Title->GetFontSize();
}

void mvManager::SetTitleColor(float r, float g, float b)
{
	m_Title->SetColor(r, g, b);
}

const float *mvManager::GetTitleColor() const
{
	return m_Title->GetColor();
}

int mvManager::IsColorBarVisible() const
{
	return m_ColorBar->GetVisibility();
}

void mvManager::ShowColorBar()
{
	m_ColorBar->VisibilityOn();
}

void mvManager::HideColorBar()
{
	m_ColorBar->VisibilityOff();
}

void mvManager::AdvanceOneTimePoint()
{
	if (m_DataSource == 0)
	{
		return;
	}

	m_DataSource->AdvanceOneTimePoint();
	m_TimePointIndex++;
	OnDataModified();
}

void mvManager::SetTimePointTo(int timePointIndex)
{
	if (m_DataSource == 0)
	{
		return;
	}
	m_DataSource->SetTimePointTo(timePointIndex);
	m_TimePointIndex = timePointIndex;
	OnDataModified();
}

int mvManager::GetInitialDisplayTimePoint()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	else
	{
		return m_DataSource->GetInitialDisplayTimePoint();
	}
}

void mvManager::OnDataModified()
{
	m_PointScalars->Modified();
	if (!AreAllCellsActive())
	{
		m_ScalarDataSet->Modified();
	}
	if (m_DataSource->GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		m_CellScalars->Modified();
	}
	ComputeActiveScalarRange();

	char timeLabel[50];
	if (m_DataSource->GetTimeLabelOption() == 0)
	{
		sprintf(timeLabel, "Time = %s", (m_DataSource->GetTimePointLabels())[m_TimePointIndex]);
	}
	else
	{
		sprintf(timeLabel, "Time Step %s", (m_DataSource->GetTimePointLabels())[m_TimePointIndex]);
	}

	m_TimeLabel->SetText(timeLabel);

	if (m_DataSource->GetVectorArray() != 0)
	{
		ComputeVectorMagnitudes();
		UpdateScaledVectorArray();
	}

	if (m_DataSource->GetNumberOfPathlines() > 0 && m_PathlineTimeClippingMode == 1)
	{
		char **labels = m_DataSource->GetTimePointLabels();
		float clipTimeMax = (float) atof(labels[m_TimePointIndex]);
		m_Pathlines->SetTimeClippingRange(0, clipTimeMax);
	}

	if (m_DataSource->GetModelFeatureArray())
	{
		m_ModelFeatures->SetModelFeatureArray(m_DataSource->GetModelFeatureArray());
	}

	m_Particles->SetParticleCount(m_DataSource->GetParticleCount());
	if (m_DataSource->GetParticleCount())
	{
		m_Particles->SetParticleCoordinates(m_DataSource->GetParticleCoord());
		m_Particles->SetParticleConcentrations(m_DataSource->GetParticleConcentrations());
		m_Particles->Build();

	}
}

void mvManager::UpdatePathlineScalars()
{
	int nc = m_DataSource->GetNumberOfPathlineCoordinates();
	m_PathlineScalars->SetArray(m_DataSource->GetPathlineScalarArray(), nc, 1);
	m_PathlineScalars->Modified();
}


void mvManager::UpdateScaledVectorArray()
{
	float *v = m_DataSource->GetVectorArray();
	const float *s = GetScale();
//	const int *vdim = m_DataSource->GetVectorGridDimensions();
//	int np = vdim[0] * vdim[1] * vdim[2];
	int np = m_DataSource->GetNumCells();
	if (s[0] == 1 && s[1] == 1 && s[2] == 1  && !m_VectorLog10Transform)
	{
		memcpy(m_ScaledVectorArray, v, 3*np*sizeof(float));
	}
	else
	{
		float InactiveCellValue = m_DataSource->GetInactiveCellValue();
		float vx, vy, vz, vv, factor;
		for (int i=0; i<np; i++)
		{
			vx = v[3*i];
			vy = v[3*i+1];
			vz = v[3*i+2];
			if ((vx == InactiveCellValue) || (vy == InactiveCellValue) || (vz == InactiveCellValue))
			{
				vv = 0;
			}
			else
			{

				vv = vx*vx + vy*vy + vz*vz;
			}
			if (m_VectorLog10Transform && (vv > 0))
			{
				float v = sqrt(vv);
				float lgv = log10(v);
				if (vx > 0)
				{
					vx = fabs((vx/v) * lgv);
				}
				else
				{
					vx = -fabs((vx/v) * lgv);
				}

				if (vy > 0)
				{
					vy = fabs((vy/v) * lgv);
				}
				else
				{
					vy = -fabs((vy/v) * lgv);
				}
				
				if (vz > 0)
				{
					vz = fabs((vz/v) * lgv);
				}
				else
				{
					vz = -fabs((vz/v) * lgv);
				}

				vv = vx*vx + vy*vy + vz*vz;
			}

			if (vv > 0)
			{
				factor = sqrt(vv/(vx*vx*s[0]*s[0] + 
					vy*vy*s[1]*s[1] + vz*vz*s[2]*s[2]));
			}
			else
			{
				factor = 0;
			}
			m_ScaledVectorArray[3*i] = vx*factor;
			m_ScaledVectorArray[3*i+1] = vy*factor;
			m_ScaledVectorArray[3*i+2] = vz*factor;
		}
	}
	m_Vectors->Modified();
}

void mvManager::SetScalarDataTypeTo(int dataTypeIndex)
{
	if (m_DataSource == 0)
	{
		return;
	}
	m_DataSource->SetScalarDataTypeTo(dataTypeIndex);

	int numPoints = m_ScalarDataSet->GetNumberOfPoints();
	int numCells = m_ScalarDataSet->GetNumberOfCells();

	// Set Point Data
	m_PointScalars->SetArray(m_DataSource->GetScalarArray(), numPoints, 1);
	m_PointScalars->Modified();


	if (m_DataSource->GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		// Set Cell data
		m_CellScalars->SetArray(m_DataSource->GetScalarArray()+numPoints, numCells, 1);
		m_CellScalars->Modified();
	}
	else
	{
		m_CellScalars->SetArray(m_DataSource->GetScalarArray(), numPoints, 1);
		m_CellScalars->Modified();
	}
	ComputeActiveScalarRange();

	m_ScalarDataSet->Modified();

	m_ActiveDataType = dataTypeIndex;

	if (m_ColorBarDataSource == 0)
	{
		SetColorBarEndPoints(m_ColorBarValueBlue[dataTypeIndex], m_ColorBarValueRed[dataTypeIndex]);
	}
	if (m_UseLogColorBar[dataTypeIndex])
	{
		UseLogColorBar();
	}
	else
	{
		UseLinearColorBar();
	}
	SetColorBarNumberOfLabels(m_NumColorBarLabels[dataTypeIndex]);
	SetColorBarLabelPrecision(m_ColorBarLabelPrecision[dataTypeIndex]);

	if (m_SolidDisplayMode[dataTypeIndex] == MV_BLOCKY)
	{
		SetSolidDisplayToBlocky();
	}
	else if (m_SolidDisplayMode[dataTypeIndex] == MV_BANDED)
	{
		SetSolidDisplayToBanded();
	}
	else
	{
		SetSolidDisplayToSmooth();
	}
	SetNumberOfColorBands(m_NumberOfColorBands[dataTypeIndex]);
	SetSolidThresholdLimits(m_SolidThresholdMin[dataTypeIndex], m_SolidThresholdMax[dataTypeIndex]);

	if (m_DoSolidThreshold[dataTypeIndex])
	{
		SolidThresholdOn();
	}
	else
	{
		SolidThresholdOff();
	}

	if (m_UseRegularIsosurface[dataTypeIndex])
	{
		m_Isosurface->GenerateValues(m_NumberOfRegularIsosurfaces[dataTypeIndex], 
			m_RegularIsosurfaceMin[dataTypeIndex], 
			m_RegularIsosurfaceMax[dataTypeIndex]); 

	}
	else
	{
		m_Isosurface->SetNumberOfContours(m_NumberOfCustomIsosurfaces[dataTypeIndex]);
		for (int i=0; i<m_NumberOfCustomIsosurfaces[dataTypeIndex]; i++)
		{
			m_Isosurface->SetValue(i, m_CustomIsosurfaceValues[dataTypeIndex][i]);
		}
	}

	// If cropping isosurfaces, we have to rebuild the pipeline. (Not sure
	// why this is needed, but this fixes a bug that occurs if we don't rebuild
	// the pipeline. ph - Jan 12, 2001)
	if ((m_CropBounds[0] > 0 || m_CropBounds[1] < 1 || m_CropBounds[2] > 0
			|| m_CropBounds[3] < 1 || m_CropBounds[4] > 0 || m_CropBounds[5] < 1)
			&& m_IsosurfaceActor->GetVisibility())
	{
		BuildPipelineForIsosurface();
	}
}

void mvManager::SetSolidDisplayToBlocky()
{
	m_SolidDisplayMode[m_ActiveDataType] = MV_BLOCKY;
	m_SolidMapper->SetScalarModeToUseCellData(); 
	BuildPipelineForSolid();
}

void mvManager::SetSolidDisplayToSmooth()
{
	m_SolidDisplayMode[m_ActiveDataType] = MV_SMOOTH;
	m_SolidMapper->SetScalarModeToUsePointData();
	BuildPipelineForSolid();
}

void mvManager::SetSolidDisplayToBanded()
{
	m_SolidDisplayMode[m_ActiveDataType] = MV_BANDED;
	m_SolidMapper->SetScalarModeToUseCellData();
	BuildPipelineForSolid();
}

int mvManager::GetSolidDisplayMode() const
{
	if (m_SolidDisplayMode != 0)
	{
		return m_SolidDisplayMode[m_ActiveDataType];
	}
	else
	{
		return 0;
	}
}

int mvManager::IsSolidThresholdOn() const
{
	return m_DoSolidThreshold[m_ActiveDataType];
}

void mvManager::GetScalarDataRange(float *range) const
{
	if (AreAllCellsActive())
	{
		if (m_DataSource->GetDataSetToUseForRange() == MV_USE_CELL_DATA_FOR_RANGE)
		{
			m_CellScalars->GetRange(range);
		}
		else
		{
			m_PointScalars->GetRange(range);
		}
	}
	else
	{
		range[0] = m_ActiveScalarRange[0];
		range[1] = m_ActiveScalarRange[1];
	}
}

void mvManager::GetVectorMagnitudeRange(float *range) const
{
	range[0] = m_VectorMagnitudeRange[0];
	range[1] = m_VectorMagnitudeRange[1];
}

void mvManager::SetColorBarEndPoints(float valueBlue, float valueRed)
{
	if (m_NumberOfColorBands == 0)
	{
		return;
	}
	if (m_ColorBarDataSource == 1)
	{
		SetPathlineColorBarEndPoints(valueBlue, valueRed); 
	}
	float range[2];
	if (valueRed > valueBlue)
	{
		range[0] = valueBlue;
		range[1] = valueRed;
		m_LutBlueToRed->SetRange(range[0], range[1]);
		m_LutModifiedBlueToRed->SetRange(range[0], range[1]);
		m_LutCustomScale->SetRange(range[0], range[1]);
		if (range[0]*range[1] > 0)
		{
			m_LogLutBlueToRed->SetRange(range[0], range[1]);
			m_LogLutModifiedBlueToRed->SetRange(range[0], range[1]);
			m_LogLutCustomScale->SetRange(range[0], range[1]);
		}
		if (m_ColorBarDataSource == 1)
		{
			if (m_Pathlines->GetLogTransform() == 0)
			{
				m_ColorBar->SetLookupTable(m_LutBlueToRed);
			}
			else
			{
				m_ColorBar->SetLookupTable(m_LogLutBlueToRed);
			}
		}
		else if (m_SolidMapper->GetLookupTable() == m_LutBlueToRed ||
				m_SolidMapper->GetLookupTable() == m_LutRedToBlue)
		{
			m_SolidMapper->SetLookupTable(m_LutBlueToRed);
			m_IsosurfaceMapper->SetLookupTable(m_LutBlueToRed);
			m_ColorBar->SetLookupTable(m_LutBlueToRed);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LutModifiedRedToBlue ||
				m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed)
		{
			m_SolidMapper->SetLookupTable(m_LutModifiedBlueToRed);
			m_IsosurfaceMapper->SetLookupTable(m_LutModifiedBlueToRed);
			m_ColorBar->SetLookupTable(m_LutModifiedBlueToRed);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LutCustomScale ||
				m_SolidMapper->GetLookupTable() == m_LutReversedCustomScale)
		{
			m_SolidMapper->SetLookupTable(m_LutCustomScale);
			m_IsosurfaceMapper->SetLookupTable(m_LutCustomScale);
			m_ColorBar->SetLookupTable(m_LutCustomScale);
		}

		else if (m_SolidMapper->GetLookupTable() == m_LogLutRedToBlue ||
				m_SolidMapper->GetLookupTable() == m_LogLutBlueToRed)
		{
			m_SolidMapper->SetLookupTable(m_LogLutBlueToRed);
			m_IsosurfaceMapper->SetLookupTable(m_LogLutBlueToRed);
			m_ColorBar->SetLookupTable(m_LogLutBlueToRed);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LogLutModifiedRedToBlue ||
				m_SolidMapper->GetLookupTable() == m_LogLutModifiedBlueToRed)
		{
			m_SolidMapper->SetLookupTable(m_LogLutModifiedBlueToRed);
			m_IsosurfaceMapper->SetLookupTable(m_LogLutModifiedBlueToRed);
			m_ColorBar->SetLookupTable(m_LogLutModifiedBlueToRed);
		}
		else
		{
			m_SolidMapper->SetLookupTable(m_LogLutCustomScale);
			m_IsosurfaceMapper->SetLookupTable(m_LogLutCustomScale);
			m_ColorBar->SetLookupTable(m_LogLutCustomScale);
		}
	}
	else
	{
		range[0] = valueRed;
		range[1] = valueBlue;
		m_LutRedToBlue->SetRange(range[0], range[1]);
		m_LutModifiedRedToBlue->SetRange(range[0], range[1]);
		m_LutReversedCustomScale->SetRange(range[0], range[1]);
		if (range[0]*range[1] > 0)
		{
			m_LogLutRedToBlue->SetRange(range[0], range[1]);
			m_LogLutModifiedRedToBlue->SetRange(range[0], range[1]);
			m_LogLutReversedCustomScale->SetRange(range[0], range[1]);
		}
		if (m_ColorBarDataSource == 1)
		{
			if (m_Pathlines->GetLogTransform() == 0)
			{
				m_ColorBar->SetLookupTable(m_LutRedToBlue);
			}
			else
			{
				m_ColorBar->SetLookupTable(m_LogLutRedToBlue);
			}
		}
		else if (m_SolidMapper->GetLookupTable() == m_LutBlueToRed ||
				m_SolidMapper->GetLookupTable() == m_LutRedToBlue)
		{
			m_SolidMapper->SetLookupTable(m_LutRedToBlue);
			m_IsosurfaceMapper->SetLookupTable(m_LutRedToBlue);
			m_ColorBar->SetLookupTable(m_LutRedToBlue);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LutModifiedRedToBlue ||
				m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed)
		{
			m_SolidMapper->SetLookupTable(m_LutModifiedRedToBlue);
			m_IsosurfaceMapper->SetLookupTable(m_LutModifiedRedToBlue);
			m_ColorBar->SetLookupTable(m_LutModifiedRedToBlue);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LutCustomScale ||
				m_SolidMapper->GetLookupTable() == m_LutReversedCustomScale)
		{
			m_SolidMapper->SetLookupTable(m_LutReversedCustomScale);
			m_IsosurfaceMapper->SetLookupTable(m_LutReversedCustomScale);
			m_ColorBar->SetLookupTable(m_LutReversedCustomScale);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LogLutRedToBlue ||
				m_SolidMapper->GetLookupTable() == m_LogLutBlueToRed)
		{
			m_SolidMapper->SetLookupTable(m_LogLutRedToBlue);
			m_IsosurfaceMapper->SetLookupTable(m_LogLutRedToBlue);
			m_ColorBar->SetLookupTable(m_LogLutRedToBlue);
		}
		else if (m_SolidMapper->GetLookupTable() == m_LogLutModifiedRedToBlue ||
				m_SolidMapper->GetLookupTable() == m_LogLutModifiedBlueToRed)
		{
			m_SolidMapper->SetLookupTable(m_LogLutModifiedRedToBlue);
			m_IsosurfaceMapper->SetLookupTable(m_LogLutModifiedRedToBlue);
			m_ColorBar->SetLookupTable(m_LogLutModifiedRedToBlue);
		}
		else
		{
			m_SolidMapper->SetLookupTable(m_LogLutReversedCustomScale);
			m_IsosurfaceMapper->SetLookupTable(m_LogLutReversedCustomScale);
			m_ColorBar->SetLookupTable(m_LogLutReversedCustomScale);
		}
	}
	m_ColorBar->SetDataSourceIndex(m_ColorBarDataSource);
	if (m_ColorBarDataSource == 0)
	{
		m_SolidMapper->SetScalarRange(range);
		m_IsosurfaceMapper->SetScalarRange(range);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
		m_Particles->SetColorBarEndPoints(valueBlue, valueRed);
		m_ColorBarValueBlue[m_ActiveDataType] = valueBlue;
		m_ColorBarValueRed[m_ActiveDataType] = valueRed;
	}
	else
	{
		if(valueBlue <= 0)
		{
			valueBlue = 1e-40;
		}
		if(valueRed <= 0)
		{
			valueRed = 1e-40;
		}
		m_ColorBar->SetScalarRange(valueBlue, valueRed);
	}
	
}

void mvManager::UseLinearColorBar()
{
	if (m_SolidMapper->GetLookupTable() == m_LogLutBlueToRed)
	{
		m_SolidMapper->SetLookupTable(m_LutBlueToRed);
		m_IsosurfaceMapper->SetLookupTable(m_LutBlueToRed);
		m_ColorBar->SetLookupTable(m_LutBlueToRed);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LogLutRedToBlue)
	{
		m_SolidMapper->SetLookupTable(m_LutRedToBlue);
		m_IsosurfaceMapper->SetLookupTable(m_LutRedToBlue);
		m_ColorBar->SetLookupTable(m_LutRedToBlue);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LogLutModifiedRedToBlue)
	{
		m_SolidMapper->SetLookupTable(m_LutModifiedRedToBlue);
		m_IsosurfaceMapper->SetLookupTable(m_LutModifiedRedToBlue);
		m_ColorBar->SetLookupTable(m_LutModifiedRedToBlue);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LogLutModifiedBlueToRed)
	{
		m_SolidMapper->SetLookupTable(m_LutModifiedBlueToRed);
		m_IsosurfaceMapper->SetLookupTable(m_LutModifiedBlueToRed);
		m_ColorBar->SetLookupTable(m_LutModifiedBlueToRed);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LogLutCustomScale)
	{
		m_SolidMapper->SetLookupTable(m_LutCustomScale);
		m_IsosurfaceMapper->SetLookupTable(m_LutCustomScale);
		m_ColorBar->SetLookupTable(m_LutCustomScale);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LogLutReversedCustomScale)
	{
		m_SolidMapper->SetLookupTable(m_LutReversedCustomScale);
		m_IsosurfaceMapper->SetLookupTable(m_LutReversedCustomScale);
		m_ColorBar->SetLookupTable(m_LutReversedCustomScale);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	m_UseLogColorBar[m_ActiveDataType] = 0;
}

void mvManager::UseLogColorBar()
{
	if (m_SolidMapper->GetLookupTable() == m_LutBlueToRed)
	{
		m_SolidMapper->SetLookupTable(m_LogLutBlueToRed);
		m_IsosurfaceMapper->SetLookupTable(m_LogLutBlueToRed);
		m_ColorBar->SetLookupTable(m_LogLutBlueToRed);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LutRedToBlue)
	{
		m_SolidMapper->SetLookupTable(m_LogLutRedToBlue);
		m_IsosurfaceMapper->SetLookupTable(m_LogLutRedToBlue);
		m_ColorBar->SetLookupTable(m_LogLutRedToBlue);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LutModifiedRedToBlue)
	{
		m_SolidMapper->SetLookupTable(m_LogLutModifiedRedToBlue);
		m_IsosurfaceMapper->SetLookupTable(m_LogLutModifiedRedToBlue);
		m_ColorBar->SetLookupTable(m_LogLutModifiedRedToBlue);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed)
	{
		m_SolidMapper->SetLookupTable(m_LogLutModifiedBlueToRed);
		m_IsosurfaceMapper->SetLookupTable(m_LogLutModifiedBlueToRed);
		m_ColorBar->SetLookupTable(m_LogLutModifiedBlueToRed);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LutCustomScale)
	{
		m_SolidMapper->SetLookupTable(m_LogLutCustomScale);
		m_IsosurfaceMapper->SetLookupTable(m_LogLutCustomScale);
		m_ColorBar->SetLookupTable(m_LogLutCustomScale);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	else if (m_SolidMapper->GetLookupTable() == m_LutReversedCustomScale)
	{
		m_SolidMapper->SetLookupTable(m_LogLutReversedCustomScale);
		m_IsosurfaceMapper->SetLookupTable(m_LogLutReversedCustomScale);
		m_ColorBar->SetLookupTable(m_LogLutReversedCustomScale);
		if (m_NumberOfColorBands[m_ActiveDataType] > 0)
		{
			UpdateColorBands();
		}
	}
	m_UseLogColorBar[m_ActiveDataType] = 1;
}

float mvManager::GetColorBarValueBlue() const
{
	if (m_ColorBarDataSource == 1)
	{
		return GetPathlineTimeBlue();
	}
	else
	{
		float *range = m_SolidMapper->GetScalarRange();
		if (
			(m_SolidMapper->GetLookupTable() == m_LutBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LutReversedCustomScale) ||
			(m_SolidMapper->GetLookupTable() == m_LogLutBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LogLutModifiedBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LogLutReversedCustomScale)
			)
		{
			return range[0];
		}
		else
		{
			return range[1];
		}
	}
}

float mvManager::GetColorBarValueRed() const
{
	if (m_ColorBarDataSource == 1)
	{
		return GetPathlineTimeRed();
	}
	else
	{
		float *range = m_SolidMapper->GetScalarRange();
		if (
			(m_SolidMapper->GetLookupTable() == m_LutBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LutReversedCustomScale) ||
			(m_SolidMapper->GetLookupTable() == m_LogLutBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LogLutModifiedBlueToRed) ||
			(m_SolidMapper->GetLookupTable() == m_LogLutReversedCustomScale)
			)
		{
			return range[1];
		}
		else
		{
			return range[0];
		}
	}
}

int mvManager::IsColorBarLinear() const
{
	if (m_ColorBarDataSource == 0)
	{
		return ((m_SolidMapper->GetLookupTable() == m_LutBlueToRed)||
				(m_SolidMapper->GetLookupTable() == m_LutRedToBlue)||
				(m_SolidMapper->GetLookupTable() == m_LutModifiedRedToBlue)||
				(m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed)||
				(m_SolidMapper->GetLookupTable() == m_LutCustomScale)||
				(m_SolidMapper->GetLookupTable() == m_LutReversedCustomScale)
				);
	}
	else
	{
		return !m_Pathlines->GetLogTransform();
	}
}

bool mvManager::IsColorBarNormal() const
{
	return ((m_SolidMapper->GetLookupTable() == m_LutBlueToRed)||
			(m_SolidMapper->GetLookupTable() == m_LutModifiedBlueToRed)||
			(m_SolidMapper->GetLookupTable() == m_LogLutBlueToRed)||
			(m_SolidMapper->GetLookupTable() == m_LogLutModifiedBlueToRed)||
			(m_SolidMapper->GetLookupTable() == m_LutCustomScale)||
			(m_SolidMapper->GetLookupTable() == m_LogLutCustomScale)
			);
}

void mvManager::SetColorBarWidth(int w)
{
	m_ColorBar->SetWidth(w);
}

void mvManager::SetColorBarHeight(int h)
{
	m_ColorBar->SetHeight(h);
}

void mvManager::SetColorBarFontSize(int f)
{
	m_ColorBar->SetFontSize(f);
}

void mvManager::SetColorBarOffset(int r)
{
	m_ColorBar->SetOffset(r);
}

void mvManager::SetColorBarTextColor(float r, float g, float b)
{
	m_ColorBar->GetProperty()->SetColor(r, g, b);
}

void mvManager::SetColorBarNumberOfLabels(int n)
{
	m_ColorBar->SetNumberOfLabels(n);
	m_NumColorBarLabels[m_ActiveDataType] = n;
}

void mvManager::SetColorBarLabelPrecision(int d)
{
	m_ColorBar->SetLabelPrecision(d);
	m_ColorBarLabelPrecision[m_ActiveDataType] = d;
}

void mvManager::SetColorBarColorScheme(int value)
{
	m_ColorBar->SetColorScheme(value);
	switch (value)
	{
	case MV_DEFAULT_COLOR_SCHEME:
		if (IsColorBarNormal())
		{
			if (IsColorBarLinear())
			{
				m_SolidMapper->SetLookupTable(m_LutBlueToRed);
				m_IsosurfaceMapper->SetLookupTable(m_LutBlueToRed);
				m_ColorBar->SetLookupTable(m_LutBlueToRed);
			}
			else
			{
				m_SolidMapper->SetLookupTable(m_LogLutBlueToRed);
				m_IsosurfaceMapper->SetLookupTable(m_LogLutBlueToRed);
				m_ColorBar->SetLookupTable(m_LogLutBlueToRed);
			}
		}
		else
		{
			if (IsColorBarLinear())
			{
				m_SolidMapper->SetLookupTable(m_LutRedToBlue);
				m_IsosurfaceMapper->SetLookupTable(m_LutRedToBlue);
				m_ColorBar->SetLookupTable(m_LutRedToBlue);
			}
			else
			{
				m_SolidMapper->SetLookupTable(m_LogLutRedToBlue);
				m_IsosurfaceMapper->SetLookupTable(m_LogLutRedToBlue);
				m_ColorBar->SetLookupTable(m_LogLutRedToBlue);
			}
		}
		break;
	case MV_MODIFIED_COLOR_SCHEME:
		if (IsColorBarNormal())
		{
			if (IsColorBarLinear())
			{
				m_SolidMapper->SetLookupTable(m_LutModifiedBlueToRed);
				m_IsosurfaceMapper->SetLookupTable(m_LutModifiedBlueToRed);
				m_ColorBar->SetLookupTable(m_LutModifiedBlueToRed);
			}
			else
			{
				m_SolidMapper->SetLookupTable(m_LogLutModifiedBlueToRed);
				m_IsosurfaceMapper->SetLookupTable(m_LogLutModifiedBlueToRed);
				m_ColorBar->SetLookupTable(m_LogLutModifiedBlueToRed);
			}
		}
		else
		{
			if (IsColorBarLinear())
			{
				m_SolidMapper->SetLookupTable(m_LutModifiedRedToBlue);
				m_IsosurfaceMapper->SetLookupTable(m_LutModifiedRedToBlue);
				m_ColorBar->SetLookupTable(m_LutModifiedRedToBlue);
			}
			else
			{
				m_SolidMapper->SetLookupTable(m_LogLutModifiedRedToBlue);
				m_IsosurfaceMapper->SetLookupTable(m_LogLutModifiedRedToBlue);
				m_ColorBar->SetLookupTable(m_LogLutModifiedRedToBlue);
			}
		}
		break;
	case MV_CUSTOM_COLOR_SCHEME:
		if (IsColorBarNormal())
		{
			if (IsColorBarLinear())
			{
				m_SolidMapper->SetLookupTable(m_LutCustomScale);
				m_IsosurfaceMapper->SetLookupTable(m_LutCustomScale);
				m_ColorBar->SetLookupTable(m_LutCustomScale);
			}
			else
			{
				m_SolidMapper->SetLookupTable(m_LogLutCustomScale);
				m_IsosurfaceMapper->SetLookupTable(m_LogLutCustomScale);
				m_ColorBar->SetLookupTable(m_LogLutCustomScale);
			}
		}
		else
		{
			if (IsColorBarLinear())
			{
				m_SolidMapper->SetLookupTable(m_LutReversedCustomScale);
				m_IsosurfaceMapper->SetLookupTable(m_LutReversedCustomScale);
				m_ColorBar->SetLookupTable(m_LutReversedCustomScale);
			}
			else
			{
				m_SolidMapper->SetLookupTable(m_LogLutReversedCustomScale);
				m_IsosurfaceMapper->SetLookupTable(m_LogLutReversedCustomScale);
				m_ColorBar->SetLookupTable(m_LogLutReversedCustomScale);
			}
		}
		break;
	}
}

int mvManager::GetColorBarWidth() const
{
	return m_ColorBar->GetWidth();
}

int mvManager::GetColorBarHeight() const
{
	return m_ColorBar->GetHeight();
}

int mvManager::GetColorBarFontSize() const
{
	return m_ColorBar->GetFontSize();
}

int mvManager::GetColorBarOffset() const
{
	return m_ColorBar->GetOffset();
}

const float *mvManager::GetColorBarTextColor() const
{
	return m_ColorBar->GetProperty()->GetColor();
}

int mvManager::GetColorBarNumberOfLabels() const
{
	return m_ColorBar->GetNumberOfLabels();
}

int mvManager::GetColorBarLabelPrecision() const
{
	return m_ColorBar->GetLabelPrecision();
}

int mvManager::GetColorBarColorScheme() const
{
	return m_ColorBar->GetColorScheme();
}

unsigned long mvManager::GetColorBarFirstCustomColor() const
{
	return dynamic_cast<mvColorTable*>(m_LutCustomScale)->GetFirstCustomColor();
}

unsigned long mvManager::GetColorBarLastCustomColor() const
{
	return dynamic_cast<mvColorTable*>(m_LutCustomScale)->GetLastCustomColor();
}

void mvManager::SetColorBarFirstCustomColor(unsigned long value)
{
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetFirstCustomColor(value);
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetCustomColorScheme();

	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetFirstCustomColor(value);
	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetReversedCustomColorScheme();

	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetFirstCustomColor(value);
	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetCustomColorScheme();

	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetFirstCustomColor(value);
	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetReversedCustomColorScheme();
}

void mvManager::SetColorBarLastCustomColor(unsigned long value)
{
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetLastCustomColor(value);
	dynamic_cast<mvColorTable*>(m_LutCustomScale)->SetCustomColorScheme();

	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetLastCustomColor(value);
	dynamic_cast<mvColorTable*>(m_LutReversedCustomScale)->SetReversedCustomColorScheme();

	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetLastCustomColor(value);
	dynamic_cast<mvLogColorTable*>(m_LogLutCustomScale)->SetCustomColorScheme();

	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetLastCustomColor(value);
	dynamic_cast<mvLogColorTable*>(m_LogLutReversedCustomScale)->SetReversedCustomColorScheme();
}

int mvManager::GetColorBarSource() const
{
	return m_ColorBarDataSource;
}

void mvManager::SetColorBarSource(int value)
{
	m_ColorBarDataSource = value;
}

void mvManager::SetDiffuseLighting(float diffuse)
{
	m_SolidActor->GetProperty()->SetDiffuse(diffuse);
	m_IsosurfaceActor->GetProperty()->SetDiffuse(diffuse);
	m_CroppedAwayPiecesActor->GetProperty()->SetDiffuse(diffuse);
	m_GridShell->SetDiffuse(diffuse);
	m_ModelFeatures->SetDiffuse(diffuse);
}

void mvManager::SetAmbientLighting(float ambient)
{
	m_SolidActor->GetProperty()->SetAmbient(ambient);
	m_IsosurfaceActor->GetProperty()->SetAmbient(ambient);
	m_CroppedAwayPiecesActor->GetProperty()->SetAmbient(ambient);
	m_GridShell->SetAmbient(ambient);
	m_ModelFeatures->SetAmbient(ambient);
}

void mvManager::SetSpecularLighting(float specular)
{
	m_SolidActor->GetProperty()->SetSpecular(specular);
	m_IsosurfaceActor->GetProperty()->SetSpecular(specular);
	m_CroppedAwayPiecesActor->GetProperty()->SetSpecular(specular);
	m_GridShell->SetSpecular(specular);
	m_ModelFeatures->SetSpecular(specular);
}

void mvManager::SetSpecularPower(float specularPower)
{
	m_SolidActor->GetProperty()->SetSpecularPower(specularPower);
	m_IsosurfaceActor->GetProperty()->SetSpecularPower(specularPower);
	m_CroppedAwayPiecesActor->GetProperty()->SetSpecularPower(specularPower);
	m_GridShell->SetSpecularPower(specularPower);
	m_ModelFeatures->SetSpecularPower(specularPower);
}

float mvManager::GetDiffuseLighting() const
{
	return 	m_SolidActor->GetProperty()->GetDiffuse();
}

float mvManager::GetAmbientLighting() const
{
	return 	m_SolidActor->GetProperty()->GetAmbient();
}

float mvManager::GetSpecularLighting() const
{
	return 	m_SolidActor->GetProperty()->GetSpecular();
}

float mvManager::GetSpecularPower() const
{
	return 	m_SolidActor->GetProperty()->GetSpecularPower();
}

void mvManager::ComputeActiveScalarRange()
{
	vtkFloatArray *a;
	float inactive = m_DataSource->GetInactiveCellValue();
	if (m_DataSource->GetDataSetToUseForRange() == MV_USE_CELL_DATA_FOR_RANGE)
	{
		a = m_CellScalars;
	}
	else
	{
		a = m_PointScalars;
	}
	m_ActiveScalarRange[0] = 0;
	m_ActiveScalarRange[1] = 0;
	float v;
	int start = 1;
	for (int i=0; i<a->GetNumberOfTuples(); i++)
	{
		v = a->GetValue(i);
		if (v != inactive)
		{
			if (start)
			{
				m_ActiveScalarRange[0] = v;
				m_ActiveScalarRange[1] = v;
				start = 0;
			}
			else
			{
				if (v < m_ActiveScalarRange[0])
				{
					m_ActiveScalarRange[0] = v;
				}
				else if (v > m_ActiveScalarRange[1])
				{
					m_ActiveScalarRange[1] = v;
				}
			}
		}
	}
}

void mvManager::SetLogTransformVector(int Value)
{
	if (m_VectorLog10Transform != Value)
	{
		float scalefactor = GetVectorScaleFactor();
		float optimalscalefactor = ComputeOptimalVectorSize();
		float ratio;
		if (optimalscalefactor > 0)
		{
			ratio = scalefactor/optimalscalefactor;
		}
		else
		{
			ratio = 0;
		}

		m_VectorLog10Transform = Value;

		int np = m_DataSource->GetNumCells();
		if (m_VectorLog10Transform)
		{
			m_VectorMagnitudes->SetArray(m_VectorLogMagnitudeArray, np, 1);
		}
		else
		{
			m_VectorMagnitudes->SetArray(m_VectorMagnitudeArray, np, 1);
		}
		m_VectorMagnitudes->Modified();

		optimalscalefactor = ComputeOptimalVectorSize();
		scalefactor = optimalscalefactor*ratio;

		UpdateScaledVectorArray();

		SetVectorScaleFactor(scalefactor);
		float limits[2];
		GetVectorThresholdLimits(limits);

		SetVectorThresholdLimits(limits[0], limits[1]);
	}
}

void mvManager::ComputeVectorMagnitudes()
{
	m_VectorMagnitudeRange[0] = 0;
	m_VectorMagnitudeRange[1] = 0;
	m_VectorLogMagnitudeRange[0] = 0;
	m_VectorLogMagnitudeRange[1] = 0;
	float minPosValue = 0;
	float *v = m_DataSource->GetVectorArray();
	if (v == 0)
	{
		return;
	}

	int np = m_DataSource->GetNumCells();
	float inactive = m_DataSource->GetInactiveCellValue();
	int m;
	float mag;

	int start = 1;
	int startmin = 1;
	for (int i=0; i<np; i++)
	{
		m = 3*i;
		if (v[m] == inactive)
		{
			m_VectorMagnitudeArray[i] = -2;
		}
		else
		{
			mag = sqrt(v[m]*v[m] + v[m+1]*v[m+1] + v[m+2]*v[m+2]);
			m_VectorMagnitudeArray[i] = mag;
			if (start)
			{
				m_VectorMagnitudeRange[0] = mag;
				m_VectorMagnitudeRange[1] = mag;
				start = 0;
			}
			else
			{
				if (mag < m_VectorMagnitudeRange[0])
				{
					m_VectorMagnitudeRange[0] = mag;
				}
				else if (mag > m_VectorMagnitudeRange[1])
				{
					m_VectorMagnitudeRange[1] = mag;
				}
			}
			if (mag > 0)
			{
				if (startmin)
				{
					minPosValue = mag;
					startmin = 0;
				}
				else
				{
					if (mag < minPosValue)
					{
						minPosValue = mag;
					}
				}
			}
		}
	}


	m_MinPositiveVector = minPosValue;
	minPosValue = minPosValue/2;
	for (i=0; i<np; i++)
	{
		m = 3*i;
		if ((v[m] != inactive) && (minPosValue > 0) && (m_VectorMagnitudeArray[i] > 0))
		{
			m_VectorLogMagnitudeArray[i] = log10(m_VectorMagnitudeArray[i]/minPosValue);
		}
		else
		{
			m_VectorLogMagnitudeArray[i] = 0;
		}
	}
	if (m_VectorMagnitudeRange[0] > 0)
	{
		m_VectorLogMagnitudeRange[0] = log10(m_VectorMagnitudeRange[0]/minPosValue);
	}
	else
	{
		m_VectorLogMagnitudeRange[0] = 0;
	}
	if (m_VectorMagnitudeRange[1] > 0)
	{
		m_VectorLogMagnitudeRange[1] = log10(m_VectorMagnitudeRange[1]/minPosValue);
	}
	else
	{
		m_VectorLogMagnitudeRange[1] = 0;
	}
}


void mvManager::SolidThresholdOn()
{
	m_DoSolidThreshold[m_ActiveDataType] = 1;
	BuildPipelineForSolid();
}

void mvManager::SolidThresholdOff()
{
	m_DoSolidThreshold[m_ActiveDataType] = 0;
	BuildPipelineForSolid();
}

void mvManager::SetSolidThresholdLimits(float minValue, float maxValue)
{
	m_BlockySolidThreshold->ThresholdBetween(minValue, maxValue);
	m_GridShellClipMin->SetValue(minValue);
	m_GridShellClipMax->SetValue(maxValue);
	m_SmoothSolidIsosurface->GenerateValues(2, minValue, maxValue);
	m_FacesClipMin->SetValue(minValue);
	m_FacesClipMax->SetValue(maxValue);
	m_FacesThreshold->ThresholdBetween(minValue, maxValue);
	m_SolidThresholdMin[m_ActiveDataType] = minValue;
	m_SolidThresholdMax[m_ActiveDataType] = maxValue;
}

void mvManager::GetSolidThresholdLimits(float *limits) const
{
	limits[0] = m_BlockySolidThreshold->GetLowerThreshold();
	limits[1] = m_BlockySolidThreshold->GetUpperThreshold();
}

void mvManager::Crop(float xMin, float xMax, float yMin, float yMax, 
					float zMin, float zMax, float cropAngle)
{
	m_CropBounds[0] = xMin;
	m_CropBounds[1] = xMax;
	m_CropBounds[2] = yMin;
	m_CropBounds[3] = yMax;
	m_CropBounds[4] = zMin;
	m_CropBounds[5] = zMax;
	if (cropAngle > 45)
	{
		m_CropAngle = 45;
	}
	else if (cropAngle < -45)
	{
		m_CropAngle = -45;
	}
	else
	{
		m_CropAngle = cropAngle;
	}
	UpdateCrop();
}

void mvManager::UpdateCrop()
{
	const float *bounds = m_BoundingBox->GetBounds();
	float dx = bounds[1] - bounds[0];
	float dy = bounds[3] - bounds[2];
	float dz = bounds[5] - bounds[4];
	float midx = bounds[0] + dx/2;
	float midy = bounds[2] + dy/2;
	float midz = bounds[4] + dz/2;
	float sn = sin(m_CropAngle*1.745329e-2f);
	float cs = cos(m_CropAngle*1.745329e-2f);
	m_Plane[0]->SetNormal(cs, sn, 0);
	m_Plane[1]->SetNormal(-cs, -sn, 0);
	m_Plane[2]->SetNormal(-sn, cs, 0);
	m_Plane[3]->SetNormal(sn, -cs, 0);
	m_Overlay->SetCropAngle(m_CropAngle);
	float sy = fabs(sn*dy);
	float sx = fabs(sn*dx);
	if (m_CropBounds[0] == 0)
	{
		m_Plane[0]->SetOrigin(bounds[0] - sy/2, midy, midz);
		m_Overlay->SetCropperPlaneOrigin(0, bounds[0] - sy/2, midy, midz);
	}
	else
	{
		m_Plane[0]->SetOrigin(bounds[0] - sy/2 + m_CropBounds[0] * (dx + sy), midy, midz);
		m_Overlay->SetCropperPlaneOrigin(0, bounds[0] - sy/2 + m_CropBounds[0] * (dx + sy), midy, midz);
	}
	if (m_CropBounds[1] == 1)
	{
		m_Plane[1]->SetOrigin(bounds[1] + sy/2, midy, midz);
		m_Overlay->SetCropperPlaneOrigin(1, bounds[1] + sy/2, midy, midz);
	}
	else
	{
		m_Plane[1]->SetOrigin(bounds[0] - sy/2 + m_CropBounds[1] * (dx + sy), midy, midz);
		m_Overlay->SetCropperPlaneOrigin(1, bounds[0] - sy/2 + m_CropBounds[1] * (dx + sy), midy, midz);
	}

	if (m_CropBounds[2] == 0)
	{
		m_Plane[2]->SetOrigin(midx, bounds[2] - sx/2, midz);
		m_Overlay->SetCropperPlaneOrigin(2, midx, bounds[2] - sx/2, midz);
	}
	else
	{
		m_Plane[2]->SetOrigin(midx, bounds[2] - sx/2 + m_CropBounds[2] * (dy + sx), midz);
		m_Overlay->SetCropperPlaneOrigin(2, midx, bounds[2] - sx/2 + m_CropBounds[2] * (dy + sx), midz);
	}
	if (m_CropBounds[3] == 1)
	{
		m_Plane[3]->SetOrigin(midx, bounds[3] + sx/2, midz);
		m_Overlay->SetCropperPlaneOrigin(3, midx, bounds[3] + sx/2, midz);
	}
	else
	{
		m_Plane[3]->SetOrigin(midx, bounds[2] - sx/2 + m_CropBounds[3] * (dy + sx), midz);
		m_Overlay->SetCropperPlaneOrigin(3, midx, bounds[2] - sx/2 + m_CropBounds[3] * (dy + sx), midz);
	}

	if (m_CropBounds[4] == 0)
	{
		m_Plane[4]->SetOrigin(midx, midy, bounds[4]);
	}
	else
	{
		m_Plane[4]->SetOrigin(midx, midy, bounds[4] + m_CropBounds[4] * (bounds[5] - bounds[4]));
	}
	if (m_CropBounds[5] == 1)
	{
		m_Plane[5]->SetOrigin(midx, midy, bounds[5]);
	}
	else
	{
		m_Plane[5]->SetOrigin(midx, midy, bounds[4] + m_CropBounds[5] * (bounds[5] - bounds[4]));
	}

	if (m_SolidActor->GetVisibility())
	{
		BuildPipelineForSolid();
	}
	else if (m_IsosurfaceActor->GetVisibility())
	{
		BuildPipelineForIsosurface();
	}
}

float mvManager::GetHorizontalCropAngle() const
{
	return m_CropAngle;
}

void mvManager::ShowCroppedAwayPieces()
{
	m_CroppedAwayPiecesActor->VisibilityOn();
	m_ShowCroppedAwayPieces = 1;
}

void mvManager::HideCroppedAwayPieces()
{
	m_CroppedAwayPiecesActor->VisibilityOff();
	m_ShowCroppedAwayPieces = 0;
}

int mvManager::AreCroppedAwayPiecesShown() const
{
	return m_ShowCroppedAwayPieces;
}

void mvManager::SetCroppedAwayPiecesColor(float red, float green, float blue)
{
	m_CroppedAwayPiecesActor->GetProperty()->SetColor(red, green, blue);
}

void mvManager::SetCroppedAwayPiecesOpacity(float opacity)
{
	m_CroppedAwayPiecesActor->GetProperty()->SetOpacity(opacity);
}

const float *mvManager::GetCroppedAwayPiecesColor() const
{
	return m_CroppedAwayPiecesActor->GetProperty()->GetColor();
}

float mvManager::GetCroppedAwayPiecesOpacity() const
{
	return m_CroppedAwayPiecesActor->GetProperty()->GetOpacity();
}

void mvManager::SetRegularIsosurfaces(int numIsosurface, float minValue, float maxValue)
{
	m_UseRegularIsosurface[m_ActiveDataType] = 1;
	m_NumberOfRegularIsosurfaces[m_ActiveDataType] = numIsosurface;
	m_RegularIsosurfaceMin[m_ActiveDataType] = minValue;
	m_RegularIsosurfaceMax[m_ActiveDataType] = maxValue;
	m_Isosurface->GenerateValues(numIsosurface, minValue, maxValue);
}

int mvManager::GetNumberOfRegularIsosurfaces() const
{
	return m_NumberOfRegularIsosurfaces[m_ActiveDataType];
}

int mvManager::GetNumberOfCustomIsosurfaces() const
{
	return m_NumberOfCustomIsosurfaces[m_ActiveDataType];
}

void mvManager::GetRegularIsosurfaceRange(float *range) const
{
	range[0] = m_RegularIsosurfaceMin[m_ActiveDataType];
	range[1] = m_RegularIsosurfaceMax[m_ActiveDataType];
}

const float *mvManager::GetCustomIsosurfaceValues() const
{
	return m_CustomIsosurfaceValues[m_ActiveDataType];
}

int mvManager::UsingRegularIsosurfaces() const
{
	return m_UseRegularIsosurface[m_ActiveDataType];
}

void mvManager::SetCustomIsosurfaces(int count, const float *values)
{
	m_UseRegularIsosurface[m_ActiveDataType] = 0;
	m_NumberOfCustomIsosurfaces[m_ActiveDataType] = count;
	m_Isosurface->SetNumberOfContours(count);
	if (m_CustomIsosurfaceValues[m_ActiveDataType] != 0)
	{
		delete [] m_CustomIsosurfaceValues[m_ActiveDataType];
	}
	if (count > 0)
	{
		m_CustomIsosurfaceValues[m_ActiveDataType] = new float[count];
	}
	else
	{
		m_CustomIsosurfaceValues[m_ActiveDataType] = 0;
		return;
	}
	for (int i=0; i<count; i++)
	{
		m_CustomIsosurfaceValues[m_ActiveDataType][i] = values[i];
		m_Isosurface->SetValue(i, values[i]);
	}
}

void mvManager::BuildPipelineForSolid()
{
	int i;

	// Step 1: Determine which type of solid to start with.
	vtkPolyData *previousFilterOutput;
	if (m_DoSolidThreshold[m_ActiveDataType])
	{
		if (m_SolidDisplayMode[m_ActiveDataType] == MV_SMOOTH || m_SolidDisplayMode[m_ActiveDataType] == MV_BANDED)
		{
			previousFilterOutput = m_SmoothSolid->GetOutput();
		}
		else
		{
			previousFilterOutput = m_BlockySolid->GetOutput();
		}
	}
	else
	{
		previousFilterOutput = m_GridShell->GetShell();
	}

	m_CroppedSolid->Delete();
	m_Faces->Delete();
	m_CroppedAwayPieces->Delete();
	m_CroppedSolid = mvCustomAppendPolyData::New();
	m_Faces = mvCustomAppendPolyData::New();
	m_CroppedAwayPieces = mvCustomAppendPolyData::New();
	m_CroppedAwayPiecesMapper->SetInput(m_CroppedAwayPieces->GetOutput());

	// Step 3: Crop the solid and extract faces at cropping planes
	vtkPolyData *face[6];
	for (i=0; i<6; i++)
	{
		face[i] = 0;
	}
	int hasFaces = 0;
	if (m_CropBounds[0] > 0)
	{
		m_Cropper[0]->SetInput(previousFilterOutput);
		previousFilterOutput = m_Cropper[0]->GetOutput();
		m_CroppedAwayPieces->AddInput(m_Cropper[0]->GetClippedOutput());
		face[0] = m_ExtractFace[0]->GetOutput();
		hasFaces = 1;
	}
	if (m_CropBounds[1] < 1)
	{
		m_Cropper[1]->SetInput(previousFilterOutput);
		previousFilterOutput = m_Cropper[1]->GetOutput();
		m_CroppedAwayPieces->AddInput(m_Cropper[1]->GetClippedOutput());
		face[1] = m_ExtractFace[1]->GetOutput();
		hasFaces = 1;
	}
	if (m_CropBounds[2] > 0)
	{
		m_Cropper[2]->SetInput(previousFilterOutput);
		previousFilterOutput = m_Cropper[2]->GetOutput();
		m_CroppedAwayPieces->AddInput(m_Cropper[2]->GetClippedOutput());
		face[2] = m_ExtractFace[2]->GetOutput();
		hasFaces = 1;
	}
	if (m_CropBounds[3] < 1)
	{
		m_Cropper[3]->SetInput(previousFilterOutput);
		previousFilterOutput = m_Cropper[3]->GetOutput();
		m_CroppedAwayPieces->AddInput(m_Cropper[3]->GetClippedOutput());
		face[3] = m_ExtractFace[3]->GetOutput();
		hasFaces = 1;
	}
	if (m_CropBounds[4] > 0)
	{
		m_Cropper[4]->SetInput(previousFilterOutput);
		previousFilterOutput = m_Cropper[4]->GetOutput();
		m_CroppedAwayPieces->AddInput(m_Cropper[4]->GetClippedOutput());
		face[4] = m_ExtractFace[4]->GetOutput();
		hasFaces = 1;
	}
	if (m_CropBounds[5] < 1)
	{
		m_Cropper[5]->SetInput(previousFilterOutput);
		previousFilterOutput = m_Cropper[5]->GetOutput();
		m_CroppedAwayPieces->AddInput(m_Cropper[5]->GetClippedOutput());
		face[5] = m_ExtractFace[5]->GetOutput();
		hasFaces = 1;
	}

	// Step 4: Crop the faces. Each face has to be cropped 4 times.
	if (hasFaces)
	{
		m_CroppedSolid->AddInput(previousFilterOutput);

		if (m_CropBounds[0] == m_CropBounds[1])
		{
			face[2] = 0;
			face[3] = 0;
			face[4] = 0;
			face[5] = 0;
		}
		else if (m_CropBounds[2] == m_CropBounds[3])
		{
			face[0] = 0;
			face[1] = 0;
			face[4] = 0;
			face[5] = 0;
		}
		else if (m_CropBounds[4] == m_CropBounds[5])
		{
			face[0] = 0;
			face[1] = 0;
			face[2] = 0;
			face[3] = 0;
		}
		if (m_CropBounds[0] > 0)
		{
			if (face[2] != 0)
			{
				m_FaceCrop[0]->SetInput(face[2]);
				face[2] = m_FaceCrop[0]->GetOutput();
			}
			if (face[3] != 0)
			{
				m_FaceCrop[1]->SetInput(face[3]);
				face[3] = m_FaceCrop[1]->GetOutput();
			}
			if (face[4] != 0)
			{
				m_FaceCrop[2]->SetInput(face[4]);
				face[4] = m_FaceCrop[2]->GetOutput();
			}
			if (face[5] != 0)
			{
				m_FaceCrop[3]->SetInput(face[5]);
				face[5] = m_FaceCrop[3]->GetOutput();
			}
		}
		if (m_CropBounds[1] < 1)
		{
			if (face[2] != 0)
			{
				m_FaceCrop[4]->SetInput(face[2]);
				face[2] = m_FaceCrop[4]->GetOutput();
			}
			if (face[3] != 0)
			{
				m_FaceCrop[5]->SetInput(face[3]);
				face[3] = m_FaceCrop[5]->GetOutput();
			}
			if (face[4] != 0)
			{
				m_FaceCrop[6]->SetInput(face[4]);
				face[4] = m_FaceCrop[6]->GetOutput();
			}
			if (face[5] != 0)
			{
				m_FaceCrop[7]->SetInput(face[5]);
				face[5] = m_FaceCrop[7]->GetOutput();
			}
		}
		if (m_CropBounds[2] > 0)
		{
			if (face[0] != 0)
			{
				m_FaceCrop[8]->SetInput(face[0]);
				face[0] = m_FaceCrop[8]->GetOutput();
			}
			if (face[1] != 0)
			{
				m_FaceCrop[9]->SetInput(face[1]);
				face[1] = m_FaceCrop[9]->GetOutput();
			}
			if (face[4] != 0)
			{
				m_FaceCrop[10]->SetInput(face[4]);
				face[4] = m_FaceCrop[10]->GetOutput();
			}
			if (face[5] != 0)
			{
				m_FaceCrop[11]->SetInput(face[5]);
				face[5] = m_FaceCrop[11]->GetOutput();
			}
		}
		if (m_CropBounds[3] < 1)
		{
			if (face[0] != 0)
			{
				m_FaceCrop[12]->SetInput(face[0]);
				face[0] = m_FaceCrop[12]->GetOutput();
			}
			if (face[1] != 0)
			{
				m_FaceCrop[13]->SetInput(face[1]);
				face[1] = m_FaceCrop[13]->GetOutput();
			}
			if (face[4] != 0)
			{
				m_FaceCrop[14]->SetInput(face[4]);
				face[4] = m_FaceCrop[14]->GetOutput();
			}
			if (face[5] != 0)
			{
				m_FaceCrop[15]->SetInput(face[5]);
				face[5] = m_FaceCrop[15]->GetOutput();
			}
		}
		if (m_CropBounds[4] > 0)
		{
			if (face[0] != 0)
			{
				m_FaceCrop[16]->SetInput(face[0]);
				face[0] = m_FaceCrop[16]->GetOutput();
			}
			if (face[1] != 0)
			{
				m_FaceCrop[17]->SetInput(face[1]);
				face[1] = m_FaceCrop[17]->GetOutput();
			}
			if (face[2] != 0)
			{
				m_FaceCrop[18]->SetInput(face[2]);
				face[2] = m_FaceCrop[18]->GetOutput();
			}
			if (face[3] != 0)
			{
				m_FaceCrop[19]->SetInput(face[3]);
				face[3] = m_FaceCrop[19]->GetOutput();
			}
		}
		if (m_CropBounds[5] < 1)
		{
			if (face[0] != 0)
			{
				m_FaceCrop[20]->SetInput(face[0]);
				face[0] = m_FaceCrop[20]->GetOutput();
			}
			if (face[1] != 0)
			{
				m_FaceCrop[21]->SetInput(face[1]);
				face[1] = m_FaceCrop[21]->GetOutput();
			}
			if (face[2] != 0)
			{
				m_FaceCrop[22]->SetInput(face[2]);
				face[2] = m_FaceCrop[22]->GetOutput();
			}
			if (face[3] != 0)
			{
				m_FaceCrop[23]->SetInput(face[3]);
				face[3] = m_FaceCrop[23]->GetOutput();
			}
		}

		// Append all faces
		int faceCount = 0;
		for (i=0; i<6; i++)
		{
			if (face[i] != 0)
			{
				m_Faces->AddInput(face[i]);
				faceCount++;
			}
		}
		// If there are faces, then append them to the cropped solid
		if (faceCount > 0)
		{
			// If solid threshold is on, we need to clip the faces
			if (m_DoSolidThreshold[m_ActiveDataType])
			{
				// for smooth or banded solid, we clip
				if (m_SolidDisplayMode[m_ActiveDataType] == MV_SMOOTH || m_SolidDisplayMode[m_ActiveDataType] == MV_BANDED)
				{
					m_FacesClipMin->SetInput(m_Faces->GetOutput());
					m_CroppedSolid->AddInput(m_FacesClipMax->GetOutput());
				}
				// for blocky solid, we pull out entire cells
				else
				{
					// Apply threshold only when there are cells. Otherwise an error 
					// occurs in vtkThreshold
					m_Faces->Update();
					if (m_Faces->GetOutput()->GetNumberOfCells() > 0)
					{
						m_FacesThreshold->SetInput(m_Faces->GetOutput());
						m_CroppedSolid->AddInput(m_FacesThresholdGeometry->GetOutput());
					}
				}
			}
			else
			{
				m_CroppedSolid->AddInput(m_Faces->GetOutput());
			}
		}
		previousFilterOutput = m_CroppedSolid->GetOutput();
	}
	
	// Create color bands
	if (m_SolidDisplayMode[m_ActiveDataType] == MV_BANDED)
	{
		m_ColorBandFilter->SetInput(previousFilterOutput);
		previousFilterOutput = m_ColorBandFilter->GetOutput();
	}

	m_SolidMapper->SetInput(previousFilterOutput);
}

void mvManager::BuildPipelineForIsosurface()
{
	vtkPolyData *polyData = m_Isosurface->GetOutput();
	if (m_CropBounds[0] == m_CropBounds[1])
	{
		m_IsosurfaceCutter[0]->SetInput(polyData);
		polyData = m_IsosurfaceCutter[0]->GetOutput();
		if (m_CropBounds[0] == 0)
		{
			m_IsosurfaceCutter[0]->SetCutFunction(m_Plane[0]);
		}
		else
		{
			m_IsosurfaceCutter[0]->SetCutFunction(m_Plane[1]);
		}
	}
	else
	{
		if (m_CropBounds[0] > 0)
		{
			m_Cropper[0]->SetInput(polyData);
			polyData = m_Cropper[0]->GetOutput();
		}
		if (m_CropBounds[1] < 1)
		{
			m_Cropper[1]->SetInput(polyData);
			polyData = m_Cropper[1]->GetOutput();
		}
	}
	if (m_CropBounds[2] == m_CropBounds[3])
	{
		m_IsosurfaceCutter[1]->SetInput(polyData);
		polyData = m_IsosurfaceCutter[1]->GetOutput();
		if (m_CropBounds[2] == 0)
		{
			m_IsosurfaceCutter[1]->SetCutFunction(m_Plane[2]);
		}
		else
		{
			m_IsosurfaceCutter[1]->SetCutFunction(m_Plane[3]);
		}
	}
	else
	{
		if (m_CropBounds[2] > 0)
		{
			m_Cropper[2]->SetInput(polyData);
			polyData = m_Cropper[2]->GetOutput();
		}
		if (m_CropBounds[3] < 1)
		{
			m_Cropper[3]->SetInput(polyData);
			polyData = m_Cropper[3]->GetOutput();
		}
	}
	if (m_CropBounds[4] == m_CropBounds[5])
	{
		m_IsosurfaceCutter[2]->SetInput(polyData);
		polyData = m_IsosurfaceCutter[2]->GetOutput();
		if (m_CropBounds[4] == 0)
		{
			m_IsosurfaceCutter[2]->SetCutFunction(m_Plane[4]);
		}
		else
		{
			m_IsosurfaceCutter[2]->SetCutFunction(m_Plane[5]);
		}
	}
	else
	{
		if (m_CropBounds[4] > 0)
		{
			m_Cropper[4]->SetInput(polyData);
			polyData = m_Cropper[4]->GetOutput();
		}
		if (m_CropBounds[5] < 1)
		{
			m_Cropper[5]->SetInput(polyData);
			polyData = m_Cropper[5]->GetOutput();
		}
	}
	m_IsosurfaceMapper->SetInput(polyData);

	// cropped-away pieces
	m_CroppedAwayPieces->Delete();
	m_CroppedAwayPieces = mvCustomAppendPolyData::New();
	m_CroppedAwayPiecesMapper->SetInput(m_CroppedAwayPieces->GetOutput());
	if (m_CropBounds[0] == m_CropBounds[1] || m_CropBounds[2] == m_CropBounds[3]
		|| m_CropBounds[4] == m_CropBounds[5])
	{
		m_CroppedAwayPieces->AddInput(m_Isosurface->GetOutput());
	}
	else
	{
		if (m_CropBounds[0] > 0)
		{
			m_CroppedAwayPieces->AddInput(m_Cropper[0]->GetClippedOutput());
		}
		if (m_CropBounds[1] < 1)
		{
			m_CroppedAwayPieces->AddInput(m_Cropper[1]->GetClippedOutput());
		}
		if (m_CropBounds[2] > 0)
		{
			m_CroppedAwayPieces->AddInput(m_Cropper[2]->GetClippedOutput());
		}
		if (m_CropBounds[3] < 1)
		{
			m_CroppedAwayPieces->AddInput(m_Cropper[3]->GetClippedOutput());
		}
		if (m_CropBounds[4] > 0)
		{
			m_CroppedAwayPieces->AddInput(m_Cropper[4]->GetClippedOutput());
		}
		if (m_CropBounds[5] < 1)
		{
			m_CroppedAwayPieces->AddInput(m_Cropper[5]->GetClippedOutput());
		}
	}
}


// Write parameters to file
char *mvManager::Serialize(const char *fileName, mvGUISettings *gui) const
{
	// Reading data is done in "Deserialize".

	// Open an output file stream
	ofstream out(fileName);

	// Header
	out << "Application name = Model Viewer\n";
	out << "Version = " << MV_VERSION << "\n";

	out << "Title = \n";

	// Model name and data files
	out << "Model name = " << m_DataSource->GetName() << endl;
	// Copy the model input file names from the data source.
	char *fnames = new char[strlen(m_DataSource->GetDataFileList()) + 1];
	strcpy(fnames, m_DataSource->GetDataFileList());
	// Write the number of file codes. This can be determined
	// by searching the number of occurences of '\n' in the file name string.
	char *cr = fnames;
	int ncode = 0;
	while (1)
	{
		cr = strchr(cr, '\n');
		if (!cr)
		{
			break;
		}
		ncode++;
		cr++;
	}
	out << "File code count = " << ncode << endl;

	// Model data files
	char *code = fnames;
	for (int i=0; i<ncode; i++)
	{
		cr = strchr(code, '\n');
		*(cr) = '\0';
		out << "File code " << (i+1) << " = " << code << endl;
		code = cr+1;
	}
	out << "All Cells Active = " << AreAllCellsActive() << endl;

	// Data types
	delete [] fnames;
	int numDataTypes = m_DataSource->GetNumberOfScalarDataTypes();
	out << "Data type count = " << numDataTypes << endl;
	out << "Data type active= " << m_ActiveDataType << endl;

	// Color bar, solid and isosurface control
	const float *rgb = GetColorBarTextColor();
	out << "Color bar data source = " << GetColorBarSource() << endl;
	out << "Color bar width = " << GetColorBarWidth() << endl;
	out << "Color bar height = " << GetColorBarHeight() << endl;
	out << "Color bar offset = " << GetColorBarOffset() << endl;
	out << "Color bar font size = " << GetColorBarFontSize() << endl;
	out << "Color bar label color option = " << (int) (rgb[0]*2+.1)  << endl;
	out << "Color bar color scheme = " << GetColorBarColorScheme()  << endl;

    unsigned long color = GetColorBarFirstCustomColor();
	unsigned long red, green, blue;
	// red
	red = color;
	red = red << 24;
	red = red >> 24;
	//green
	green = color;
	green = green << 16;
	green = green >> 24;
	//blue
	blue = color;
	blue = blue << 8;
	blue = blue >> 24;
	out << "Color bar first custom color red = "   << (int)red    << endl;
	out << "Color bar first custom color green = " << (int)green  << endl;
	out << "Color bar first custom color blue = "   << (int)blue  << endl;

	color = GetColorBarLastCustomColor();
	red = color;
	red = red << 24;
	red = red >> 24;
	//green
	green = color;
	green = green << 16;
	green = green >> 24;
	//blue
	blue = color;
	blue = blue << 8;
	blue = blue >> 24;

	out << "Color bar last custom color red = "   << (int)red    << endl;
	out << "Color bar last custom color green = " << (int)green  << endl;
	out << "Color bar last custom color blue = "   << (int)blue  << endl;

	for (i=0; i<numDataTypes; i++)
	{
		// Color Bar
		out << "Color bar " << (i+1) << " value blue = " << m_ColorBarValueBlue[i] << endl;
		out << "Color bar " << (i+1) << " value red = " << m_ColorBarValueRed[i] << endl;
		out << "Color bar " << (i+1) << " logarithmic scale = " << m_UseLogColorBar[i] << endl;
		out << "Color bar " << (i+1) << " number of labels = " << m_NumColorBarLabels[i] << endl;
		out << "Color bar " << (i+1) << " label precision = " << m_ColorBarLabelPrecision[i] << endl;

		// Solid Control
		out << "Solid " << (i+1) << " display mode = " << m_SolidDisplayMode[i] << endl;
		out << "Solid " << (i+1) << " apply threshold = " << m_DoSolidThreshold[i] << endl;
		out << "Solid " << (i+1) << " threshold lower limit = " << m_SolidThresholdMin[i] << endl;
		out << "Solid " << (i+1) << " threshold upper limit = " << m_SolidThresholdMax[i] << endl;
		out << "Solid " << (i+1) << " number of color bands = " << m_NumberOfColorBands[i] << endl;

		// Isosurface
		out << "Isosurfaces " << (i+1) << " show regular = " << m_UseRegularIsosurface[i] << endl;
		out << "Isosurfaces " << (i+1) << " regular count = " << m_NumberOfRegularIsosurfaces[i] << endl;
		out << "Isosurfaces " << (i+1) << " regular min  = " << m_RegularIsosurfaceMin[i] << endl;
		out << "Isosurfaces " << (i+1) << " regular max  = " << m_RegularIsosurfaceMax[i] << endl;
		out << "Isosurfaces " << (i+1) << " custom count = " << m_NumberOfCustomIsosurfaces[i] << endl;
		for (int j=0; j<m_NumberOfCustomIsosurfaces[i]; j++)
		{
			out << "Isosurfaces " << (i+1) << " custom value " << (j+1) << " = " << m_CustomIsosurfaceValues[i][j] << endl;
		}
	}

	// Vector
	out << "Vector data = " << HasVectorData() << endl;
	if (HasVectorData())
	{
		int *voi = m_ExtractVector->GetVOI();
		int *srate = m_ExtractVector->GetSampleRate();
		rgb = m_VectorActor->GetProperty()->GetColor();
		out << "Vector subsample i min = " << voi[0] << endl;
		out << "Vector subsample i max = " << voi[1] << endl;
		out << "Vector subsample j min = " << voi[2] << endl;
		out << "Vector subsample j max = " << voi[3] << endl;
		out << "Vector subsample k min = " << voi[4] << endl;
		out << "Vector subsample k max = " << voi[5] << endl;
		out << "Vector subsample i rate = " << srate[0] << endl;
		out << "Vector subsample j rate = " << srate[1] << endl;
		out << "Vector subsample k rate = " << srate[2] << endl;
		out << "Vector scale factor = " << GetVectorScaleFactor() << endl;
		out << "Vector threshold apply = " << m_DoVectorThreshold << endl;
		out << "Vector threshold lower limit = " << m_VectorThreshold->GetLowerThreshold() << endl;
		out << "Vector threshold upper limit = " << m_VectorThreshold->GetUpperThreshold() << endl;
		out << "Vector glyph active = " << IsVectorGlyphActivated() << endl;
		out << "Vector glyph x length = " << m_CubeSource->GetXLength() << endl;
		out << "Vector glyph y length = " << m_CubeSource->GetYLength() << endl;
		out << "Vector glyph z length = " << m_CubeSource->GetZLength() << endl;
		out << "Vector color option = " << (int) (rgb[0]*2+.1)  << endl;

		out << "Vector Crop bounds x min = " << m_VectorClippingXMin << endl;
		out << "Vector Crop bounds x max = " << m_VectorClippingXMax << endl;
		out << "Vector Crop bounds y min = " << m_VectorClippingYMin << endl;
		out << "Vector Crop bounds y max = " << m_VectorClippingYMax << endl;
		out << "Vector Crop bounds z min = " << m_VectorClippingZMin << endl;
		out << "Vector Crop bounds z max = " << m_VectorClippingZMax << endl;
		out << "Vector Crop angle = "        << m_VectorClippingAngle << endl;

		out << "Vector Log transform = " << m_VectorLog10Transform << endl;
		out << "Vector Line Width = " << m_VectorActor->GetProperty()->GetLineWidth() << endl;
	}

	// Pathlines
	out << "Pathline data = " << HasPathlineData()  << endl;
	out << "Pathline representation = " << GetPathlineRepresentation() << endl;
	out << "Pathline tube diameter = " << GetPathlineTubeDiameter() << endl;
	out << "Pathline time blue = " << m_Pathlines->GetTimeBlue() << endl;
	out << "Pathline time red = " << m_Pathlines->GetTimeRed() << endl;
	out << "Pathline threshold mode = " << m_PathlineTimeClippingMode << endl;
	out << "Pathline threshold time min = " << m_PathlineClipTimeMin << endl;
	out << "Pathline threshold time max = " << m_PathlineClipTimeMax << endl;
	out << "Pathline log transform = " << GetPathlineLogTransform() << endl;

	// Model Features
	int nmft = m_DataSource->GetNumberOfModelFeatureTypes();
	float rgba[4];
	out << "Number of model feature types = " << nmft << endl;
	int *displayOrder = m_ModelFeatures->GetDisplayOrder();
	for (i=0; i<nmft; i++)
	{
		out << "Model feature type " << i << " display order = " << displayOrder[i] << endl;
		m_ModelFeatures->GetColor(i, rgba);
		out << "Model feature type " << i << " red = " << rgba[0] << endl;
		out << "Model feature type " << i << " green = " << rgba[1] << endl;
		out << "Model feature type " << i << " blue = " << rgba[2] << endl;
		out << "Model feature type " << i << " alpha = " << rgba[3] << endl;
	}
	out << "Model feature glyph size = " << m_ModelFeatures->GetGlyphSize() << endl;

	// Subgrid
	int *subg = m_ScalarSubDataSet->GetVOI();
	out << "Subgrid active = " << IsScalarSubgridOn() << endl;
	out << "Subgrid i min = " << subg[0] << endl;
	out << "Subgrid i max = " << subg[1] << endl;
	out << "Subgrid j min = " << subg[2] << endl;
	out << "Subgrid j max = " << subg[3] << endl;
	out << "Subgrid k min = " << subg[4] << endl;
	out << "Subgrid k max = " << subg[5] << endl;

	// Surface lighting
	out << "Lighting diffuse = " << m_SolidActor->GetProperty()->GetDiffuse() << endl;
	out << "Lighting ambient = " << m_SolidActor->GetProperty()->GetAmbient() << endl;
	out << "Lighting specular = " << m_SolidActor->GetProperty()->GetSpecular() << endl;
	out << "Lighting Specular power = " << m_SolidActor->GetProperty()->GetSpecularPower() << endl;

	// Grid Shell
	rgb = m_GridShell->GetColor();
	out << "Grid shell color red = " << rgb[0] << endl;
	out << "Grid shell color green = " << rgb[1] << endl;
	out << "Grid shell color blue = " << rgb[2] << endl;
	out << "Grid shell color opacity = " << m_GridShell->GetOpacity() << endl;

	// Grid Lines
	int ibuff[3];
	GetGridLinePositions(ibuff);
	rgb = m_GridLines[0]->GetColor();
	out << "Grid lines Position x = " << ibuff[0] << endl;
	out << "Grid lines Position y = " << ibuff[1] << endl;
	out << "Grid lines Position z = " << ibuff[2] << endl;
	out << "Grid lines activation x = " << m_GridLinesActivated[0] << endl;
	out << "Grid lines activation y = " << m_GridLinesActivated[1] << endl;
	out << "Grid lines activation z = " << m_GridLinesActivated[2] << endl;
	out << "Grid lines color option = " << (int) (rgb[0]*2+.1)  << endl;
	out << "Grid outline activation = " << m_GridOutlineActivated << endl;
	out << "Mesh outline choice = " << m_MeshOutlineChoice << endl;
	
	// Scale
	float scale[3];
	m_SolidActor->GetScale(scale);
	out << "Scale x = " << scale[0] << endl;
	out << "Scale y = " << scale[1] << endl;
	out << "Scale z = " << scale[2] << endl;

	// Axes
	const float *pn = m_Axes->GetNormalizedPosition();
	out << "Axes representation = " << GetAxesRepresentation() << endl;
	out << "Axes normalized size = " << GetAxesNormalizedSize() << endl;
	out << "Axes normalized tube diameter = " << GetAxesNormalizedTubeDiameter() << endl;
	out << "Axes normalized position x = " << pn[0] << endl;
	out << "Axes normalized position y = " << pn[1] << endl;
	out << "Axes normalized position z = " << pn[2] << endl;

	out << "Log transform x axis = " << m_DataSource->GetLogTransformXAxis() << endl;
	out << "Log transform y axis = " << m_DataSource->GetLogTransformYAxis() << endl;
	out << "Log transform z axis = " << m_DataSource->GetLogTransformZAxis() << endl;

	// Bounding Box
	rgb = m_BoundingBox->GetColor();
	out << "Bounding box color option = " << (int) (rgb[0]*2+.1)  << endl;

	// Crop
	rgb = m_CroppedAwayPiecesActor->GetProperty()->GetColor();
	out << "Crop bounds x min = " << m_CropBounds[0] << endl;
	out << "Crop bounds x max = " << m_CropBounds[1] << endl;
	out << "Crop bounds y min = " << m_CropBounds[2] << endl;
	out << "Crop bounds y max = " << m_CropBounds[3] << endl;
	out << "Crop bounds z min = " << m_CropBounds[4] << endl;
	out << "Crop bounds z max = " << m_CropBounds[5] << endl;
	out << "Crop angle = " << m_CropAngle << endl;
	out << "Cropped away pieces red = " << rgb[0] << endl;
	out << "Cropped away pieces green = " << rgb[1] << endl;
	out << "Cropped away pieces blue = " << rgb[2] << endl;
	out << "Cropped away pieces opacity = " << m_CroppedAwayPiecesActor->GetProperty()->GetOpacity() << endl;

	// Actor visibility
	out << "Visibility Solid = " << m_SolidActor->GetVisibility() << endl;
	out << "Visibility Isosurface = " << m_IsosurfaceActor->GetVisibility() << endl;
	out << "Visibility Cropped away pieces = " << m_CroppedAwayPiecesActor->GetVisibility() << endl;
	out << "Visibility Vector = " << m_VectorActor->GetVisibility() << endl;
	out << "Visibility Pathline = " << m_Pathlines->GetVisibility() << endl;
	out << "Visibility Model features = " << m_ModelFeatures->GetVisibility() << endl;
	out << "Visibility Grid shell = " << m_GridShell->GetVisibility() << endl;
	out << "Visibility Activated grid lines = " << m_ActivatedGridLinesVisibility << endl;
	out << "Visibility Axes = " << m_Axes->GetVisibility() << endl;
	out << "Visibility Bounding box = " << m_BoundingBox->GetVisibility() << endl;
	out << "Visibility Time label = " << m_TimeLabel->GetVisibility() << endl;
	out << "Visibility Title = " << m_Title->GetVisibility() << endl;
	out << "Visibility Color bar = " << m_ColorBar->GetVisibility() << endl;
	out << "Visibility Overlay = " << m_Overlay->GetVisibility() << endl;

	// Particles
	out << "Particle Visibility = " << m_Particles->GetVisibility() << endl;
	out << "Particle Glyph Size = " << m_Particles->GetGlyphSize() << endl;
	out << "Particle Displayed Concentration Limits have been set = " << m_Particles->GetDisplayedConcentrationsHaveBeenSet() << endl;
	if (m_Particles->GetDisplayedConcentrationsHaveBeenSet())
	{
		out << "Particle Maximum Displayed Concentration = " << m_Particles->GetMaxDisplayedConcentration() << endl;
		out << "Particle Minimum Displayed Concentration = " << m_Particles->GetMinDisplayedConcentration() << endl;

	}
	out << "Particles Cropped  = " << m_Particles->GetParticlesCropped() << endl;

	if (m_Particles->GetParticlesCropped())
	{
		out << "Particle Crop bounds x min = " << m_ParticleClippingXMin << endl;
		out << "Particle Crop bounds x max = " << m_ParticleClippingXMax << endl;
		out << "Particle Crop bounds y min = " << m_ParticleClippingYMin << endl;
		out << "Particle Crop bounds y max = " << m_ParticleClippingYMax << endl;
		out << "Particle Crop bounds z min = " << m_ParticleClippingZMin << endl;
		out << "Particle Crop bounds z max = " << m_ParticleClippingZMax << endl;
	}

	// Overlay
	if (m_Overlay->HasData())
	{
		out << "Overlay file = " << m_Overlay->GetFileName() << endl;
	}
	out << "Overlay data type = " << m_Overlay->GetType() << endl;
	double x, y;
	int oldprecision = out.precision(12);
	m_Overlay->GetCoordinatesAtGridOrigin(x, y);
	out << "Overlay x coordinate at grid origin = " << x << endl;
	out << "Overlay y coordinate at grid origin = " << y << endl;
	out << "Overlay to grid scale = " << m_Overlay->GetOverlayToGridScale() << endl;
	out << "Overlay angle = " << m_Overlay->GetAngle() << endl;
	out << "Overlay drape = " << m_Overlay->GetDrape() << endl;
	out << "Overlay trim = " << m_Overlay->GetTrim() << endl;
	out << "Overlay crop = " << m_Overlay->GetCrop() << endl;
	out << "Overlay elevation = " << m_Overlay->GetElevation() << endl;
	out << "Overlay drape gap = " << m_Overlay->GetDrapeGap() << endl;
	out.precision(oldprecision);

	// GUI settings
	gui->Serialize(&out);

	out.close();

	return 0;
}

// Read parameters from file
char *mvManager::Deserialize(const char *fileName, mvGUISettings *gui)
{
	int i, ivalue, i1, i2, i3, i4, i5, i6, i7, i8, i9;
	float fvalue, f1, f2, f3, f4, f5, f6;
	char key[100], buffer[1024];

	// Open an input stream
	ifstream in(fileName, ios::in|ios::nocreate);

	if (!in.is_open())
	{
		return "Unable to open the Model Viewer file.";
	}

	// Set the working directory to the directory that contains
	// the document (.mv) file. We assume that model data files
	// are also in this directory.
	strcpy(buffer, fileName);
	char *p = strrchr(buffer, '\\');
	if (*(p+1) == ':')
	{
		p++;
	}
	*p = '\0';
	_chdir(buffer);	

	// Create a hash table and read the data file into the hash table
	mvHashTable *hashTable = new mvHashTable;
	while (!in.eof())
	{
		if (mvUtil::ExtractLabelAndValue(&in, key, buffer))
		{
			// Start of fix for error in version 1.0 mv file
			// This is because the label "Subgrid i max" is misprinted as "Subgrid i min".
			// Same error with j and k. Also, the labels "Pathline clip ..." has been revised
			// as "Pathline threshold ..."
			if (strcmp(key, "Subgrid i min") == 0 && hashTable->GetHashTableValue("Subgrid i min", ivalue))
			{
				strcpy(key, "Subgrid i max");
			}
			else if (strcmp(key, "Subgrid j min") == 0 && hashTable->GetHashTableValue("Subgrid j min", ivalue))
			{
				strcpy(key, "Subgrid j max");
			}
			else if (strcmp(key, "Subgrid k min") == 0 && hashTable->GetHashTableValue("Subgrid k min", ivalue))
			{
				strcpy(key, "Subgrid k max");
			}
			else if (strcmp(key, "Pathline clip mode") == 0)
			{
				strcpy(key, "Pathline threshold mode");
			}
			else if (strcmp(key, "Pathline clip time min") == 0)
			{
				strcpy(key, "Pathline threshold time min");
			}
			else if (strcmp(key, "Pathline clip time max") == 0)
			{
				strcpy(key, "Pathline threshold time max");
			}
			// End of fix for version 1.0 mv file
			hashTable->AddHashEntry(key, buffer);
		}
	}
	in.close();


	// Check header
	buffer[0] = '\0';
	hashTable->GetHashTableValue("Application name", buffer);
	if (strcmp(buffer, "Model Viewer") != 0)
	{
		delete hashTable;
		return "This file does not contain data for Model Viewer";
	}
	float version = 0;
	hashTable->GetHashTableValue("Version", version);
	if (version < 1.0)
	{
		delete hashTable;
		return "Cannot open a Model Viewer file with version lower than 1.0.";
	}
	m_version = version;

	// Read data file names
	char modelName[1024];
	modelName[0] = '\0';
	hashTable->GetHashTableValue("Model name", modelName);
	int ncode = 0;
	hashTable->GetHashTableValue("File code count", ncode);
	if (ncode == 0)
	{
		delete hashTable;
		return "The Model Viewer file is damaged and cannot be loaded.";
	}

	char *dataFileList = new char [ncode*1024];
	dataFileList[0] = '\0';
	for (i=0; i<ncode; i++)
	{
		buffer[0] = '\0';
		sprintf(key, "File code %u", i+1);
		if (!hashTable->GetHashTableValue(key, buffer))
		{
			delete hashTable;
			delete [] dataFileList;
			return "The Model Viewer file is damaged and cannot be loaded.";
		}
		strcat(dataFileList, buffer);
		strcat(dataFileList, "\n");
	}

	
	// Load the data and set up the visualization pipeline 

	char *errorMsg = LoadData(modelName, dataFileList);
	delete [] dataFileList;

	if (errorMsg != 0)
	{
		delete hashTable;
		return errorMsg;
	}

	SetTimePointTo(m_DataSource->GetInitialDisplayTimePoint());
	SetScalarDataTypeTo(0);
	ApplyDefaultSettings();

	if (hashTable->GetHashTableValue("All Cells Active", ivalue))
	{
		AssumeAllCellsAreActive(ivalue);
	}
	else
	{
		AssumeAllCellsAreActive(0);
	}

	// Actor Visibility
	if (hashTable->GetHashTableValue("Visibility Solid", ivalue))
	{
		m_SolidActor->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Isosurface", ivalue))
	{
		m_IsosurfaceActor->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Cropped away pieces", ivalue))
	{
		m_ShowCroppedAwayPieces = ivalue;
		m_CroppedAwayPiecesActor->SetVisibility(m_ShowCroppedAwayPieces);
	}
	if (hashTable->GetHashTableValue("Visibility Vector", ivalue))
	{
		m_VectorActor->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Pathline", ivalue))
	{
		m_Pathlines->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Model features", ivalue))
	{
		m_ModelFeatures->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Grid shell", ivalue))
	{
		m_GridShell->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Axes", ivalue))
	{
		m_Axes->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Bounding box", ivalue))
	{
		m_BoundingBox->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Time label", ivalue))
	{
		m_TimeLabel->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Title", ivalue))
	{
		m_Title->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Color bar", ivalue))
	{
		m_ColorBar->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Visibility Overlay", ivalue))
	{
		m_Overlay->SetVisibility(ivalue);
	}

	// Grid line activation
	HideGridLines();
	if (hashTable->GetHashTableValue("Grid lines activation x", ivalue) && ivalue)
	{
		ActivateGridLines(0);
	}
	else
	{
		DeactivateGridLines(0);
	}
	if (hashTable->GetHashTableValue("Grid lines activation y", ivalue) && ivalue)
	{
		ActivateGridLines(1);
	}
	else
	{
		DeactivateGridLines(1);
	}
	if (hashTable->GetHashTableValue("Grid lines activation z", ivalue) && ivalue)
	{
		ActivateGridLines(2);
	}
	else
	{
		DeactivateGridLines(2);
	}
	if (hashTable->GetHashTableValue("Grid outline activation", ivalue) && ivalue)
	{
		ActivateGridOutline();
	}
	else
	{
		DeactivateGridOutline();
	}
	if (hashTable->GetHashTableValue("Mesh outline choice", ivalue))
	{
		SetMeshOutlineChoice(ivalue);
	}
	else
	{
		SetMeshOutlineChoice(0);
	}

//	int iStructuredGrid = m_DataSource->GetGridType();
	bool StructuredGrid = GetIsStructuredGrid();
//	bool StructuredGrid = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
//		|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
//		|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);
	
	if (hashTable->GetHashTableValue("Visibility Activated grid lines", ivalue) && ivalue)
	{
		ShowActivatedGridLines();
		if (StructuredGrid)
		{
			HideMeshLines();
		}
		else
		{
			ShowMeshLines();
		}
	}
	else
	{
		HideMeshLines();
	}

	// Color Bar
	if (hashTable->GetHashTableValue("Color bar data source", ivalue))
	{
		SetColorBarSource(ivalue);
		if (ivalue)
		{
			float range[2];
			GetPathlineTimeRange(range);
			SetColorBarEndPoints(range[0], range[1]);
		}

	}
	if (hashTable->GetHashTableValue("Color bar width", ivalue))
	{
		SetColorBarWidth(ivalue);
	}
	if (hashTable->GetHashTableValue("Color bar height", ivalue))
	{
		SetColorBarHeight(ivalue);
	}
	if (hashTable->GetHashTableValue("Color bar offset", ivalue))
	{
		SetColorBarOffset(ivalue);
	}
	if (hashTable->GetHashTableValue("Color bar font size", ivalue))
	{
		SetColorBarFontSize(ivalue);
	}
	if (hashTable->GetHashTableValue("Color bar label color option", fvalue))
	{
		float v = fvalue * 0.5f;
		SetColorBarTextColor(v, v, v);
	}
	if (hashTable->GetHashTableValue("Color bar color scheme", ivalue))
	{
		SetColorBarColorScheme(ivalue);
	}
	unsigned long color, red, green, blue;
	if (hashTable->GetHashTableValue("Color bar first custom color red", ivalue))
	{
		red = ivalue;
	}
	else
	{
		red = 0x5A;
	}
	if (hashTable->GetHashTableValue("Color bar first custom color green", ivalue))
	{
		green = ivalue;
	}
	else
	{
		green = 0x5A;
	}
	if (hashTable->GetHashTableValue("Color bar first custom color blue", ivalue))
	{
		blue = ivalue;
	}
	else
	{
		blue = 0x5A;
	}
	green = green << 8;
	blue = blue << 16;
	color = red + green + blue;
	SetColorBarFirstCustomColor(color);
	if (hashTable->GetHashTableValue("Color bar last custom color red", ivalue))
	{
		red = ivalue;
	}
	else
	{
		red = 0xF0;
	}
	if (hashTable->GetHashTableValue("Color bar last custom color green", ivalue))
	{
		green = ivalue;
	}
	else
	{
		green = 0xF0;
	}
	if (hashTable->GetHashTableValue("Color bar last custom color blue", ivalue))
	{
		blue = ivalue;
	}
	else
	{
		blue = 0xF0;
	}
	green = green << 8;
	blue = blue << 16;
	color = red + green + blue;
	SetColorBarLastCustomColor(color);



	// If the saved number of data types is the same as the
	// number of data types in the data source, then
	// delete the arrays allocated in the LoadData method
	// and replace by the deserialized ones
	if (hashTable->GetHashTableValue("Data type count", ivalue))
	{
		int ierror = 0;
		int numDataTypes = ivalue;
		if (numDataTypes == m_DataSource->GetNumberOfScalarDataTypes())
		{
			float *colorBarValueBlue = new float [numDataTypes];
			float *colorBarValueRed = new float [numDataTypes];
			int *useLogLut = new int [numDataTypes];
			int *numColorBarLabels = new int [numDataTypes];
			int *colorBarLabelPrecision = new int [numDataTypes];
			int *solidDisplayMode = new int [numDataTypes];
			int *applySolidThreshold = new int [numDataTypes];
			int *numColorBands = new int [numDataTypes];
			float *solidThresholdMin = new float [numDataTypes];
			float *solidThresholdMax = new float [numDataTypes];
			int *usingRegularIsosurfaces = new int [numDataTypes];
			int *numberOfRegularIsosurfaces = new int [numDataTypes];
			float *regularIsosurfaceMin = new float [numDataTypes];
			float *regularIsosurfaceMax = new float [numDataTypes];
			int *numberOfCustomIsosurfaces = new int [numDataTypes];
			float **customIsosurfaceValues = new float *[numDataTypes];
			for (i=0; i<numDataTypes; i++)
			{
				customIsosurfaceValues[i] = 0;
			}
			char key[100];
			for (i=0; i<numDataTypes; i++)
			{
				// Color Bar
				sprintf(key, "Color bar %u value blue", i+1);
				if (!hashTable->GetHashTableValue(key, colorBarValueBlue[i])) {ierror = 1; break;}
				sprintf(key, "Color bar %u value red", i+1);
				if (!hashTable->GetHashTableValue(key, colorBarValueRed[i])) {ierror = 1; break;}
				sprintf(key, "Color bar %u logarithmic scale", i+1);
				if (!hashTable->GetHashTableValue(key, useLogLut[i])) {ierror = 1; break;}
				sprintf(key, "Color bar %u number of labels", i+1);
				if (!hashTable->GetHashTableValue(key, numColorBarLabels[i])) {ierror = 1; break;}
				sprintf(key, "Color bar %u label precision", i+1);
				if (!hashTable->GetHashTableValue(key, colorBarLabelPrecision[i])) {ierror = 1; break;}

				// Solid
				sprintf(key, "Solid %u display mode", i+1);
				if (!hashTable->GetHashTableValue(key, solidDisplayMode[i])) {ierror = 1; break;}
				sprintf(key, "Solid %u apply threshold", i+1);
				if (!hashTable->GetHashTableValue(key, applySolidThreshold[i])) {ierror = 1; break;}
				sprintf(key, "Solid %u threshold lower limit", i+1);
				if (!hashTable->GetHashTableValue(key, solidThresholdMin[i])) {ierror = 1; break;}
				sprintf(key, "Solid %u threshold upper limit", i+1);
				if (!hashTable->GetHashTableValue(key, solidThresholdMax[i])) {ierror = 1; break;}
				sprintf(key, "Solid %u number of color bands", i+1);
				if (!hashTable->GetHashTableValue(key, numColorBands[i])) {ierror = 1; break;}

				// update for version 1.0rc2 and earlier
				if (numColorBands[i] == 0) 
				{
					numColorBands[i] = 10;
				}

				// Isosurface
				sprintf(key, "Isosurfaces %u show regular", i+1);
				if (!hashTable->GetHashTableValue(key, usingRegularIsosurfaces[i])) {ierror = 1; break;}
				sprintf(key, "Isosurfaces %u regular count", i+1);
				if (!hashTable->GetHashTableValue(key, numberOfRegularIsosurfaces[i])) {ierror = 1; break;}
				sprintf(key, "Isosurfaces %u regular min", i+1);
				if (!hashTable->GetHashTableValue(key, regularIsosurfaceMin[i])) {ierror = 1; break;}
				sprintf(key, "Isosurfaces %u regular max", i+1);
				if (!hashTable->GetHashTableValue(key, regularIsosurfaceMax[i])) {ierror = 1; break;}
				sprintf(key, "Isosurfaces %u custom count", i+1);
				if (!hashTable->GetHashTableValue(key, numberOfCustomIsosurfaces[i])) {ierror = 1; break;}
				if (numberOfCustomIsosurfaces[i] > 0)
				{
					customIsosurfaceValues[i] = new float[numberOfCustomIsosurfaces[i]];
					for (int j=0; j<numberOfCustomIsosurfaces[i]; j++)
					{
						sprintf(key, "Isosurfaces %u custom value %u", i+1, j+1);
						if (!hashTable->GetHashTableValue(key, customIsosurfaceValues[i][j])) {ierror = 1; break;}
					}
					if (ierror) {break;}
				}
			}
			if (!ierror)
			{
				delete [] m_ColorBarValueBlue;
				delete [] m_ColorBarValueRed;
				delete [] m_UseLogColorBar;
				delete [] m_NumColorBarLabels;
				delete [] m_ColorBarLabelPrecision;

				delete [] m_SolidDisplayMode;
				delete [] m_DoSolidThreshold;
				delete [] m_SolidThresholdMax;
				delete [] m_SolidThresholdMin;
				delete [] m_NumberOfColorBands;

				delete [] m_UseRegularIsosurface;
				delete [] m_NumberOfRegularIsosurfaces;
				delete [] m_RegularIsosurfaceMin;
				delete [] m_RegularIsosurfaceMax;
				delete [] m_NumberOfCustomIsosurfaces;
				if (m_CustomIsosurfaceValues != 0)
				{
					for (int i=0; i<m_NumScalarDataTypes; i++)
					{
						if (m_CustomIsosurfaceValues[i] != 0)
						{
							delete [] m_CustomIsosurfaceValues[i];
						}
					}
				}

				delete [] m_CustomIsosurfaceValues;

				m_ColorBarValueBlue = colorBarValueBlue;
				m_ColorBarValueRed = colorBarValueRed;
				m_UseLogColorBar = useLogLut;
				m_NumColorBarLabels = numColorBarLabels;
				m_ColorBarLabelPrecision = colorBarLabelPrecision;

				m_SolidDisplayMode = solidDisplayMode;
				m_DoSolidThreshold = applySolidThreshold;
				m_SolidThresholdMax = solidThresholdMax;
				m_SolidThresholdMin = solidThresholdMin;
				m_NumberOfColorBands = numColorBands;

				m_UseRegularIsosurface = usingRegularIsosurfaces;
				m_NumberOfRegularIsosurfaces = numberOfRegularIsosurfaces;
				m_RegularIsosurfaceMin = regularIsosurfaceMin;
				m_RegularIsosurfaceMax = regularIsosurfaceMax;
				m_NumberOfCustomIsosurfaces = numberOfCustomIsosurfaces;
				m_CustomIsosurfaceValues = customIsosurfaceValues;

				if (hashTable->GetHashTableValue("Data type active", ivalue))
				{
					SetScalarDataTypeTo(ivalue);
				}
			}
			else
			{
				delete [] colorBarValueBlue;
				delete [] colorBarValueRed;
				delete [] useLogLut;
				delete [] numColorBarLabels;
				delete [] colorBarLabelPrecision;
				delete [] solidDisplayMode;
				delete [] applySolidThreshold;
				delete [] numColorBands;
				delete [] solidThresholdMin;
				delete [] solidThresholdMax;
				delete [] usingRegularIsosurfaces;
				delete [] numberOfRegularIsosurfaces;
				delete [] regularIsosurfaceMin;
				delete [] regularIsosurfaceMax;
				delete [] numberOfCustomIsosurfaces;
				for (i=0; i<numDataTypes; i++)
				{
					if (customIsosurfaceValues[i])
					{
						delete [] customIsosurfaceValues[i];
					}
				}
				delete [] customIsosurfaceValues;
			}

		}
	}

	// Surface lighting
	SetDiffuseLighting(1.0);	// diffuse lighting is hard wired
	if (hashTable->GetHashTableValue("Lighting ambient", fvalue))
	{
		SetAmbientLighting(fvalue);
	}
	if (hashTable->GetHashTableValue("Lighting specular", fvalue))
	{
		SetSpecularLighting(fvalue);
	}
	if (hashTable->GetHashTableValue("Lighting Specular power", fvalue))
	{
		SetSpecularPower(fvalue);
	}

	// Grid Shell
	if (hashTable->GetHashTableValue("Grid shell color red", f1) 
			&& hashTable->GetHashTableValue("Grid shell color green", f2) 
			&& hashTable->GetHashTableValue("Grid shell color blue", f3))
	{
		SetGridShellColor(f1, f2, f3);
	}
	if (hashTable->GetHashTableValue("Grid shell color opacity", fvalue))
	{
		SetGridShellOpacity(fvalue);
	}

	// Scale
	if (hashTable->GetHashTableValue("Scale x", f1) 
		&& hashTable->GetHashTableValue("Scale y", f2) 
		&& hashTable->GetHashTableValue("Scale z", f3))
	{
		SetScale(f1, f2, f3);
	}

	// Bounding box
	if (hashTable->GetHashTableValue("Bounding box color option", fvalue))
	{
		float v = (fvalue) * 0.5f;
		SetBoundingBoxColor(v, v, v);
	}

	// Subgrid 
	if ( hashTable->GetHashTableValue("Subgrid i min", i1)
		&& hashTable->GetHashTableValue("Subgrid i max", i2)
		&& hashTable->GetHashTableValue("Subgrid j min", i3)
		&& hashTable->GetHashTableValue("Subgrid j max", i4)
		&& hashTable->GetHashTableValue("Subgrid k min", i5)
		&& hashTable->GetHashTableValue("Subgrid k max", i6))
	{
		SetScalarSubgridExtent(i1, i2, i3, i4, i5, i6);
	}
	if (hashTable->GetHashTableValue("Subgrid active", ivalue) && ivalue)
	{
		ScalarSubgridOn();
	}
	else
	{
		ScalarSubgridOff();
	}

	// Axes
	if (hashTable->GetHashTableValue("Axes representation", ivalue) && ivalue == 0)
	{
		SetAxesRepresentationToTube();
	}
	else
	{
		SetAxesRepresentationToLine();
	}
	if (hashTable->GetHashTableValue("Axes normalized size", fvalue))
	{
		SetAxesNormalizedSize(fvalue);
	}
	if (hashTable->GetHashTableValue("Axes normalized tube diameter", fvalue))
	{
		SetAxesNormalizedTubeDiameter(fvalue);
	}
	if (hashTable->GetHashTableValue("Axes normalized position x", f1) 
		&& hashTable->GetHashTableValue("Axes normalized position y", f2) 
		&& hashTable->GetHashTableValue("Axes normalized position z", f3))
	{
		SetAxesNormalizedPosition(f1, f2, f3);
	}

	if (hashTable->GetHashTableValue("Log transform x axis", i1))
	{
		SetLogTransformXAxis((i1 != 0));
	}
	if (hashTable->GetHashTableValue("Log transform y axis", i1))
	{
		SetLogTransformYAxis((i1 != 0));
	}
	if (hashTable->GetHashTableValue("Log transform z axis", i1))
	{
		SetLogTransformZAxis((i1 != 0));
	}

	// Grid Lines
	if (hashTable->GetHashTableValue("Grid lines Position x", f1) 
		&& hashTable->GetHashTableValue("Grid lines Position y", f2) 
		&& hashTable->GetHashTableValue("Grid lines Position z", f3))
	{
		SetGridLinePositions(f1, f2, f3);
	}
	if (hashTable->GetHashTableValue("Grid lines color option", fvalue))
	{
		float v = fvalue * 0.5f;
		SetGridLineColor(v, v, v);
	}

	// Crop
	if (hashTable->GetHashTableValue("Crop bounds x min", f1) 
		&& hashTable->GetHashTableValue("Crop bounds x max", f2) 
		&& hashTable->GetHashTableValue("Crop bounds y min", f3) 
		&& hashTable->GetHashTableValue("Crop bounds y max", f4) 
		&& hashTable->GetHashTableValue("Crop bounds z min", f5) 
		&& hashTable->GetHashTableValue("Crop bounds z max", f6) 
		&& hashTable->GetHashTableValue("Crop angle", fvalue))
	{
		Crop(f1, f2, f3, f4, f5, f6, fvalue);
	}
	if (hashTable->GetHashTableValue("Cropped away pieces red", f1) 
		&& hashTable->GetHashTableValue("Cropped away pieces green", f2) 
		&& hashTable->GetHashTableValue("Cropped away pieces blue", f3))
	{
		SetCroppedAwayPiecesColor(f1, f2, f3);
	}
	if (hashTable->GetHashTableValue("Cropped away pieces opacity", fvalue))
	{
		SetCroppedAwayPiecesOpacity(fvalue);
	}

	// Vector

	if (hashTable->GetHashTableValue("Vector data", ivalue) 
		&& ivalue && m_DataSource->GetVectorArray())
	{
		if ( hashTable->GetHashTableValue("Vector subsample i min", i1)
			&& hashTable->GetHashTableValue("Vector subsample i max", i2)
			&& hashTable->GetHashTableValue("Vector subsample j min", i3)
			&& hashTable->GetHashTableValue("Vector subsample j max", i4)
			&& hashTable->GetHashTableValue("Vector subsample k min", i5)
			&& hashTable->GetHashTableValue("Vector subsample k max", i6)
			&& hashTable->GetHashTableValue("Vector subsample i rate", i7)
			&& hashTable->GetHashTableValue("Vector subsample j rate", i8)
			&& hashTable->GetHashTableValue("Vector subsample k rate", i9))
		{
			SubsampleVectors(i1, i2, i3, i4, i5, i6, i7, i8, i9);
		}
		if (hashTable->GetHashTableValue("Vector threshold apply", ivalue) && ivalue)
		{
			VectorThresholdOn();
		}
		else
		{
			VectorThresholdOff();
		}
		if (hashTable->GetHashTableValue("Vector threshold lower limit", f1) 
			&& hashTable->GetHashTableValue("Vector threshold upper limit", f2))
		{
			SetVectorThresholdLimits(f1, f2);
		}
		if (hashTable->GetHashTableValue("Vector glyph active", ivalue))
		{
			ActivateVectorGlyph(ivalue);

		}
		if ( hashTable->GetHashTableValue("Vector glyph x length", f1)
			&& hashTable->GetHashTableValue("Vector glyph y length", f2)
			&& hashTable->GetHashTableValue("Vector glyph z length", f3))
		{
			m_CubeSource->SetXLength(f1);
			m_CubeSource->SetYLength(f2);
			m_CubeSource->SetZLength(f3);
		}
		if (hashTable->GetHashTableValue("Vector color option", fvalue))
		{
			float v = (fvalue) *0.5f;
			SetVectorColor(v, v, v);
		}

		if ( hashTable->GetHashTableValue("Vector Crop bounds x min", f1)
			&& hashTable->GetHashTableValue("Vector Crop bounds x max", f2)
			&& hashTable->GetHashTableValue("Vector Crop bounds y min", f3)
			&& hashTable->GetHashTableValue("Vector Crop bounds y max", f4)
			&& hashTable->GetHashTableValue("Vector Crop bounds z min", f5)
			&& hashTable->GetHashTableValue("Vector Crop bounds z max", f6)
			&& hashTable->GetHashTableValue("Vector Crop angle", ivalue))
		{
			CropVectors(f1, f2, f3, f4, f5, f6, ivalue);
		}
		else
		{
			CropVectors(0, 1, 0, 1, 0, 1, 0);
		}
		if (hashTable->GetHashTableValue("Vector Log transform", ivalue))
		{
			SetLogTransformVector(ivalue);
		}
		if (hashTable->GetHashTableValue("Vector Line Width", fvalue))
		{
			SetVectorLineWidth(fvalue);
		}
		if (hashTable->GetHashTableValue("Vector scale factor", fvalue))
		{
			SetVectorScaleFactor(fvalue);
		}
	}

	// Pathline
	if (hashTable->GetHashTableValue("Pathline data", ivalue) && ivalue)
	{
		if (hashTable->GetHashTableValue("Pathline representation", ivalue) 
			&& ivalue == MV_TUBE)
		{
			SetPathlineRepresentationToTube();
		}
		else
		{
			SetPathlineRepresentationToLine();
		}
		if (hashTable->GetHashTableValue("Pathline tube diameter", fvalue))
		{
			SetPathlineTubeDiameter(fvalue);
		}
		if (hashTable->GetHashTableValue("Pathline time blue", f1)
			&& hashTable->GetHashTableValue("Pathline time red", f2))
		{
			SetPathlineColorBarEndPoints(f1, f2);
		}
		if (hashTable->GetHashTableValue("Pathline threshold time min", f1) 
			&& hashTable->GetHashTableValue("Pathline threshold time max", f2))
		{
			SetPathlineTimeClippingRange(f1, f2);
		}
		if (hashTable->GetHashTableValue("Pathline threshold mode", ivalue))
		{
			SetPathlineTimeClippingMode(ivalue);
		}
		if (hashTable->GetHashTableValue("Pathline log transform", ivalue))
		{
			SetPathlineLogTransform(ivalue);
		}
		else
		{
			SetPathlineLogTransform(0);
		}

	}

	// Model Features
	if (hashTable->GetHashTableValue("Number of model feature types", ivalue) && ivalue)
	{
		int numberOfModelFeatureTypes = ivalue;
		int *modelFeatureDisplayOrder = new int [numberOfModelFeatureTypes];
		float **modelFeatureRgba = new float *[numberOfModelFeatureTypes];
		for (i=0; i<numberOfModelFeatureTypes; i++)
		{
			modelFeatureRgba[i] = 0;
		}
		char key[100];
		int ierror = 0;
		for (i=0; i<numberOfModelFeatureTypes; i++)
		{
			sprintf(key, "Model feature type %u display order", i);
			if (!hashTable->GetHashTableValue(key, modelFeatureDisplayOrder[i])) {ierror = 1; break;}
			modelFeatureRgba[i] = new float[4];
			sprintf(key, "Model feature type %u red", i);
			if (!hashTable->GetHashTableValue(key, modelFeatureRgba[i][0])) {ierror = 1; break;}
			sprintf(key, "Model feature type %u green", i);
			if (!hashTable->GetHashTableValue(key, modelFeatureRgba[i][1])) {ierror = 1; break;}
			sprintf(key, "Model feature type %u blue", i);
			if (!hashTable->GetHashTableValue(key, modelFeatureRgba[i][2])) {ierror = 1; break;}
			sprintf(key, "Model feature type %u alpha", i);
			if (!hashTable->GetHashTableValue(key, modelFeatureRgba[i][3])) {ierror = 1; break;}
		}

		if (!ierror && numberOfModelFeatureTypes == m_DataSource->GetNumberOfModelFeatureTypes())
		{
			int *displayOrder = m_ModelFeatures->GetDisplayOrder();
			int consistent=1;
			for (i=0; i<numberOfModelFeatureTypes; i++)
			{
				if ((displayOrder[i]!=-2 && modelFeatureDisplayOrder[i]==-2) ||
					(displayOrder[i]==-2 && modelFeatureDisplayOrder[i]!=-2))
				{
					consistent=0;
					break;
				}
			}
			if (consistent)
			{
				m_ModelFeatures->SetDisplayOrder(modelFeatureDisplayOrder);
			}
			for (i=0; i<numberOfModelFeatureTypes; i++)
			{
				m_ModelFeatures->SetColor(i, modelFeatureRgba[i]);
			}
		}
		for (i=0; i<numberOfModelFeatureTypes; i++)
		{
			if (modelFeatureRgba[i])
			{
				delete [] modelFeatureRgba[i];
			}
		}
		delete [] modelFeatureDisplayOrder;
		delete [] modelFeatureRgba;
	}
	if (hashTable->GetHashTableValue("Model feature glyph size", fvalue))
	{
		m_ModelFeatures->SetDefaultGlyphSize(fvalue);
	}

	// Particles
	if (hashTable->GetHashTableValue("Particle Visibility", ivalue))
	{
		m_Particles->SetVisibility(ivalue);
	}
	if (hashTable->GetHashTableValue("Particle Glyph Size", fvalue))
	{
		m_Particles->SetDefaultGlyphSize(fvalue);
	}
	if (hashTable->GetHashTableValue("Particle Displayed Concentration Limits have been set", ivalue) && ivalue)
	{
		if (hashTable->GetHashTableValue("Particle Minimum Displayed Concentration", f1) 
			&& hashTable->GetHashTableValue("Particle Maximum Displayed Concentration", f2))
		{
			m_Particles->SetDisplayedConcentrations(f1, f2);
		}
	}
	if (hashTable->GetHashTableValue("Particles Cropped", ivalue) && ivalue)
	{
		if ( hashTable->GetHashTableValue("Particle Crop bounds x min", f1)
			&& hashTable->GetHashTableValue("Particle Crop bounds x max", f2)
			&& hashTable->GetHashTableValue("Particle Crop bounds y min", f3)
			&& hashTable->GetHashTableValue("Particle Crop bounds y max", f4)
			&& hashTable->GetHashTableValue("Particle Crop bounds z min", f5)
			&& hashTable->GetHashTableValue("Particle Crop bounds z max", f6))
		{
			CropParticles(f1, f2, f3, f4, f5, f6);
		}
		else
		{
			CropParticles(0, 1, 0, 1, 0, 1);
		}
	}
	else
	{
		CropParticles(0, 1, 0, 1, 0, 1);
	}

	// Overlay
	if (hashTable->GetHashTableValue("Overlay data type", ivalue) && ivalue)
	{
		SetOverlayType(ivalue);
	}
	double dvalue, d1, d2;
	if (hashTable->GetHashTableValue("Overlay x coordinate at grid origin", d1) 
		&& hashTable->GetHashTableValue("Overlay y coordinate at grid origin", d2))
	{
		SetOverlayCoordinatesAtGridOrigin(d1, d2);
	}
	if (hashTable->GetHashTableValue("Overlay to grid scale", d1))
	{
		SetOverlayToGridScale(d1);
	}
	if (hashTable->GetHashTableValue("Overlay angle", dvalue))
	{
		SetOverlayAngle(dvalue);
	}
	if (hashTable->GetHashTableValue("Overlay drape", ivalue))
	{
		SetOverlayDrape(ivalue);
	}
	if (hashTable->GetHashTableValue("Overlay trim", ivalue))
	{
		SetOverlayTrim(ivalue);
	}
	if (hashTable->GetHashTableValue("Overlay crop", ivalue))
	{
		SetOverlayCrop(ivalue);
	}
	if (hashTable->GetHashTableValue("Overlay elevation", dvalue))
	{
		SetOverlayElevation(dvalue);
	}
	if (hashTable->GetHashTableValue("Overlay drape gap", dvalue))
	{
		SetOverlayDrapeGap(dvalue);
	}
	char filename[1024];
	char *errMsg = 0;
	if (hashTable->GetHashTableValue("Overlay file", filename))
	{
		SetOverlayFileName(filename);
		if (!UpdateOverlay(errMsg)) 
		{ 
			strcat(m_WarningMessage, "Unable to load overlay file. Overlay will not be displayed.");
		}
	}

	gui->Deserialize(hashTable, version);
	
	delete hashTable;


	return 0;
}

char *mvManager::GetDataFileList() const
{
	if (m_DataSource != 0)
	{
		return m_DataSource->GetDataFileList();
	}
	else
	{
		return 0;
	}
}

int mvManager::HasVectorData() const
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	else
	{
		return (m_DataSource->GetVectorArray() != 0);
	}
}


int mvManager::HasPathlineData() const
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	else
	{
		return (m_DataSource->GetNumberOfPathlines() > 0);
	}
}

void mvManager::ShowParticles()
{

	CropParticles(m_ParticleClippingXMin, m_ParticleClippingXMax, 
		m_ParticleClippingYMin, m_ParticleClippingYMax, 
		m_ParticleClippingZMin, m_ParticleClippingZMax);

	m_Particles->VisibilityOn();
}

void mvManager::HideParticles()
{
	m_Particles->VisibilityOff();
}

int mvManager::AreParticlesVisible() const
{
	return m_Particles->GetVisibility();
}

int mvManager::HasParticles() const
{
	return (m_Particles && m_Particles->ParticleCount());
}

void mvManager::EnlargeParticleGlyphs()
{
	m_Particles->EnlargeGlyphs();
}

void mvManager::ShrinkParticleGlyphs()
{
	m_Particles->ShrinkGlyphs();
}

void mvManager::FilterParticles(float concMin, float concMax)
{
	m_Particles->SetDisplayedConcentrations(concMin, concMax);
}

float mvManager::GetMaxDisplayedParticleConcentration() const
{
	return m_Particles->GetMaxDisplayedConcentration();
}

float mvManager::GetMinDisplayedParticleConcentration() const
{
	return m_Particles->GetMinDisplayedConcentration();
}

float mvManager::GetMaxParticleConcentration() const
{
	return m_Particles->GetMaxConcentration();
}

float mvManager::GetMinParticleConcentration() const
{
	return m_Particles->GetMinConcentration();
}

void mvManager::CropParticles(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax)
{
	int ShouldCrop = ((xmin != 0.) || (xmax != 1.) || (ymin != 0.) || (ymax != 1.) || (zmin != 0.) || (zmax != 1.));

	m_ParticleClippingXMin = xmin;
	m_ParticleClippingXMax = xmax;
	m_ParticleClippingYMin = ymin;
	m_ParticleClippingYMax = ymax;
	m_ParticleClippingZMin = zmin;
	m_ParticleClippingZMax = zmax;

	float bounds[6];
	m_BoundingBox->GetBounds(bounds);

	m_Particles->CropParticles(ShouldCrop, xmin, xmax, ymin, ymax, zmin, zmax, bounds);
}

int mvManager::HasOverlay()
{
	return m_Overlay->HasData();
}

void mvManager::ShowOverlay()
{
	m_Overlay->VisibilityOn();
}

void mvManager::HideOverlay()
{
	m_Overlay->VisibilityOff();
}

int mvManager::IsOverlayVisible() const
{
	return m_Overlay->GetVisibility();
}

void mvManager::ClearOverlayData()
{
	m_Overlay->ClearData();
}

void mvManager::SetOverlayFileName(char * filename)
{
	m_Overlay->SetFileName(filename);
}

void mvManager::SetOverlayType(int type)
{
	m_Overlay->SetType(type);
}

void mvManager::SetOverlayCoordinatesAtGridOrigin(double xorig, double yorig)
{
	m_Overlay->SetCoordinatesAtGridOrigin(xorig, yorig);
}

void mvManager::SetOverlayElevation(double elev)
{
	m_Overlay->SetElevation(elev);
}

void mvManager::SetOverlayAngle(double angle)
{
	m_Overlay->SetAngle(angle);
}

void mvManager::SetOverlayToGridScale(double scale)
{
	m_Overlay->SetOverlayToGridScale(scale);
}

void mvManager::SetOverlayDrape(int d)
{
	m_Overlay->SetDrape(d);
}

void mvManager::SetOverlayTrim(int d)
{
	m_Overlay->SetTrim(d);
}

void mvManager::SetOverlayCrop(int d)
{
	m_Overlay->SetCrop(d);
}

void mvManager::SetOverlayDrapeGap(double d)
{
	m_Overlay->SetDrapeGap(d);
}

int mvManager::UpdateOverlay(char *errMsg)
{
	return m_Overlay->Update(errMsg);
}

char *mvManager::GetOverlayFileName()
{
	return m_Overlay->GetFileName();
}

int mvManager::GetOverlayType()
{
	return m_Overlay->GetType();
}

void mvManager::GetOverlayCoordinatesAtGridOrigin(double &xorig, double &yorig)
{
	m_Overlay->GetCoordinatesAtGridOrigin(xorig, yorig);
}

double mvManager::GetOverlayElevation()
{
	return m_Overlay->GetElevation();
}

double mvManager::GetOverlayAngle()
{
	return m_Overlay->GetAngle();
}

double mvManager::GetOverlayToGridScale()
{
	return m_Overlay->GetOverlayToGridScale();
}

int mvManager::GetOverlayDrape()
{
	return m_Overlay->GetDrape();
}

double mvManager::GetOverlayDrapeGap()
{
	return m_Overlay->GetDrapeGap();
}

int mvManager::GetOverlayTrim()
{
	return m_Overlay->GetTrim();
}

int mvManager::GetOverlayCrop()
{
	return m_Overlay->GetCrop();
}

void mvManager::GetOverlayBounds(double &xmin, double &xmax, double &ymin, double &ymax)
{
	m_Overlay->GetBounds(xmin, xmax, ymin, ymax);
}

void mvManager::RemoveOverlay()
{
	m_Overlay->ClearData();
	m_Overlay->SetFileName(0);
	m_Overlay->SetCoordinatesAtGridOrigin(0, 0);
	m_Overlay->SetOverlayToGridScale(1);
	m_Overlay->SetAngle(0);
	m_Overlay->SetDrape(0);
	m_Overlay->SetTrim(0);
	m_Overlay->SetCrop(0);
	m_Overlay->SetElevation(0);
	m_Overlay->SetDrapeGap(0);
	m_Overlay->VisibilityOff();

}

void mvManager::AfterNewDataSource()
{
	bool StructuredGrid = GetIsStructuredGrid();

	// debugging code
	//StructuredGrid = TRUE;


	// Scalar Data Set
	if (m_ScalarDataSet != 0)
	{
		m_ScalarDataSet->Delete();
	}
	if (StructuredGrid)
	{
		m_ScalarDataSet = vtkStructuredGrid::New();
	}
	else
	{
		m_ScalarDataSet = vtkUnstructuredGrid::New();
	}
	m_ScalarGridPoints = vtkPoints::New();
	m_PointScalars = vtkFloatArray::New();
	m_PointScalars->SetNumberOfComponents(1);
	m_CellScalars = vtkFloatArray::New();
	m_CellScalars->SetNumberOfComponents(1);
	m_ScalarDataSet->SetPoints(m_ScalarGridPoints);
	m_ScalarDataSet->GetPointData()->SetScalars(m_PointScalars);
	m_ScalarDataSet->GetCellData()->SetScalars(m_CellScalars);
	m_ScalarGridPoints->Delete();
	m_PointScalars->Delete();
	m_CellScalars->Delete();
	m_ActiveScalarDataSet->SetInput(m_ScalarDataSet);


	if (StructuredGrid)
	{
		m_VectorDataSet = vtkStructuredGrid::New();
	}
	else
	{
		m_VectorDataSet = vtkUnstructuredGrid::New();
	}
	//m_VectorDataSet = vtkStructuredGrid::New();
	m_VectorGridPoints = vtkPoints::New();
	m_Vectors = vtkFloatArray::New();
	m_Vectors->SetNumberOfComponents(3);
	m_VectorMagnitudes = vtkFloatArray::New();
	m_VectorMagnitudes->SetNumberOfComponents(1);
	m_VectorDataSet->SetPoints(m_VectorGridPoints);
	m_VectorDataSet->GetPointData()->SetVectors(m_Vectors);
	m_VectorDataSet->GetPointData()->SetScalars(m_VectorMagnitudes);
	m_VectorGridPoints->Delete();
	m_Vectors->Delete();
	m_VectorMagnitudes->Delete();


	if (StructuredGrid)
	{
		m_ScalarSubDataSet->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
	}
	else
	{
		m_ScalarSubDataSet->SetInput(0);
	}
	if (StructuredGrid)
	{
		m_ExtractVector->SetInput(dynamic_cast<vtkStructuredGrid*>(m_VectorDataSet));
		m_ActiveVectorDataSet->SetInput(m_ExtractVector->GetOutput());
		m_ExtractIrregularMeshVector->SetInput(0);
	}
	else
	{
		m_ExtractIrregularMeshVector->SetInput(dynamic_cast<vtkUnstructuredGrid*>(m_VectorDataSet));
		m_ExtractVector->SetInput(0);
		m_ActiveVectorDataSet->SetInput(m_ExtractIrregularMeshVector->GetOutput());
	}


	for (int i=0; i<3; i++)
	{
		if (StructuredGrid)
		{
			m_GridLines[i]->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
		}
		else
		{
			m_GridLines[i]->SetInput(0);
		}
	}

	m_BlockySolidThreshold->SetInput(m_ScalarDataSet);

	if (StructuredGrid)
	{
		m_GridOutline->SetInput(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
	}
	else
	{ 
		m_GridOutline->SetInput(0);
	}
	if (StructuredGrid)
	{
		m_Overlay->SetFullGrid(dynamic_cast<vtkStructuredGrid*>(m_ScalarDataSet));
	}
	else
	{
		m_Overlay->SetFullGrid(0);
	}
}

void mvManager::AddExternalMeshActors()
{
	if (!m_ExternalMeshActorsAdded)
	{
		for (int i = 0; i < m_ExternalMeshVector->GetSize(); i++)
		{
			mvExternalMesh* Item = m_ExternalMeshVector->GetItem(i); 
			m_PropCollection->AddItem(Item->GetActor());
		}
		for (i = 0; i < m_ExternalLinesVector->GetSize(); i++)
		{
			mvExternalMesh* Item = m_ExternalLinesVector->GetItem(i); 
			m_PropCollection->AddItem(Item->GetActor());
		}
		m_ExternalMeshActorsAdded = 1;
	}
}

int mvManager::AreMeshLinesVisible() const
{
	return m_MeshLines->GetVisibility();
};

void mvManager::ShowMeshLines()
{
	m_MeshActivated = TRUE;
	m_MeshLines->VisibilityOn();
	m_MeshLines->DataSet()->Modified();
	SetMeshOutlineChoice(m_MeshOutlineChoice);
};

void mvManager::HideMeshLines()
{
	m_MeshActivated = FALSE;
	m_MeshLines->VisibilityOff();
	SetMeshOutlineChoice(m_MeshOutlineChoice);
};

bool mvManager::GetIsStructuredGrid()
{
	int iStructuredGrid = GetGridType();
	bool IsStructured = (iStructuredGrid == MV_STRUCTURED_GRID_ALL_ACTIVE)
		|| (iStructuredGrid == MV_STRUCTURED_GRID_WITH_INACTIVE_CELLS)
		|| (iStructuredGrid == MV_RECTILINEAR_GRID_ALL_ACTIVE);
	return IsStructured;
};

bool mvManager::GetCanLogTransformXAxis()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	return m_DataSource->GetCanLogTransformXAxis();
};

bool mvManager::GetCanLogTransformYAxis()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	return m_DataSource->GetCanLogTransformYAxis();
};

bool mvManager::GetCanLogTransformZAxis()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	return m_DataSource->GetCanLogTransformZAxis();
};

bool mvManager::GetLogTransformXAxis()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	return m_DataSource->GetLogTransformXAxis();
};

bool mvManager::GetLogTransformYAxis()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	return m_DataSource->GetLogTransformYAxis();
};

bool mvManager::GetLogTransformZAxis()
{
	if (m_DataSource == 0)
	{
		return 0;
	}
	return m_DataSource->GetLogTransformZAxis();
};

void mvManager::SetBoundingBoxBounds()
{
	float bounds[6];
	if (m_DataSource->GetPrimaryScalarMode() == MV_CELL_SCALARS)
	{
		m_ActiveScalarDataSet->Update();
		m_ActiveScalarDataSet->GetOutput()->ComputeBounds();
		m_ActiveScalarDataSet->GetOutput()->GetBounds(bounds);
		m_BoundingBox->SetBounds(bounds);
	}
	else
	{
		m_ScalarDataSet->ComputeBounds();
		m_ScalarDataSet->GetBounds(bounds);
		m_BoundingBox->SetBounds(bounds);
	}
}

void mvManager::UpdateLogTransformedData()
{
	m_ScalarGridPoints->Modified();
	m_ScalarDataSet->Modified();
	m_ScalarSubDataSet->Modified();
	m_ActiveScalarDataSet->Modified();
	m_Isosurface->Modified();

	SetBoundingBoxBounds();

	float bounds[6];
	m_ScalarDataSet->ComputeBounds();
	m_ScalarDataSet->GetBounds(bounds);
	float defaultAxesSize = (bounds[1]-bounds[0] + bounds[3]-bounds[2] + bounds[5]-bounds[4])/12;
	m_Axes->SetDefaultPositions(bounds);
	m_Axes->SetDefaultSize(defaultAxesSize);
	m_Axes->SetDefaultTubeDiameter(defaultAxesSize * 0.1);

	m_Pathlines->SetDefaultTubeDiameter(defaultAxesSize * 0.1);
}

void mvManager::SetLogTransformXAxis(bool transform)
{
	if (m_DataSource == 0)
	{
		return;
	}
	m_DataSource->SetLogTransformXAxis(transform);
	UpdateLogTransformedData();
};

void mvManager::SetLogTransformYAxis(bool transform)
{
	if (m_DataSource == 0)
	{
		return;
	}
	m_DataSource->SetLogTransformYAxis(transform);
	UpdateLogTransformedData();
};

void mvManager::SetLogTransformZAxis(bool transform)
{
	if (m_DataSource == 0)
	{
		return;
	}
	m_DataSource->SetLogTransformZAxis(transform);
	UpdateLogTransformedData();
};

char *mvManager::XAxisLabel()
{
	if (m_DataSource != 0)
	{
		return m_DataSource->XAxisLabel();
	}
	else
	{
		return "X";
	}
}

char *mvManager::YAxisLabel()
{
	if (m_DataSource != 0)
	{
		return m_DataSource->YAxisLabel();
	}
	else
	{
		return "Y";
	}
}
char *mvManager::ZAxisLabel()
{
	if (m_DataSource != 0)
	{
		return m_DataSource->ZAxisLabel();
	}
	else
	{
		return "Z";
	}
}

void mvManager::SetMeshOutlineChoice(int i)
{
	m_MeshOutlineChoice = i;
	if ((m_MeshOutlineChoice < 0) || (m_MeshOutlineChoice > 2))
	{
		m_MeshOutlineChoice = 0;
	}
	if (m_MeshActivated)
	{
		switch (m_MeshOutlineChoice)
		{
		case 0:
			m_ExternalMeshVector->VisibilityOff();
			m_ExternalLinesVector->VisibilityOff();
			break;
		case 1:
			m_ExternalMeshVector->VisibilityOff();
			m_ExternalLinesVector->VisibilityOn();
			break;
		case 2:
			m_ExternalLinesVector->VisibilityOff();
			m_ExternalMeshVector->VisibilityOn();
			break;
		}
	}
	else
	{
		m_ExternalMeshVector->VisibilityOff();
		m_ExternalLinesVector->VisibilityOff();
	}

}

