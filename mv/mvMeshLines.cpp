// mvMeshLines.cpp: implementation of the mvMeshLines class.
//
//////////////////////////////////////////////////////////////////////

#include "mvMeshLines.h"
#include "vtkThreshold.h"
#include "vtkGeometryFilter.h"
#include "vtkFeatureEdges.h"
#include "vtkPolyData.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

mvMeshLines::mvMeshLines()
{
	m_SelectedLayer = -1;
	m_DataSet = vtkPolyData::New();
	m_LayeredGridFilter = vtkPolyDataConnectivityFilter::New();
//	SetMapperInput(m_DataSet);

	m_LayeredGridFilter->SetExtractionModeToSpecifiedRegions();
	m_LayeredGridFilter->AddSeed(0);
	m_LayeredGridFilter->SetInput(m_DataSet);
	SetMapperInput(m_LayeredGridFilter->GetOutput());
	
	m_Mapper->ScalarVisibilityOn();
	m_Actor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
	m_Actor->GetProperty()->SetRepresentationToWireframe();
	m_LODActor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_LODActor->GetProperty()->SetAmbient(1.0);
	m_LODActor->GetProperty()->SetDiffuse(0.0);
	m_LODActor->GetProperty()->SetRepresentationToWireframe();
}

mvMeshLines::~mvMeshLines()
{
	m_LayeredGridFilter->Delete();
	m_DataSet->Delete();
}

void mvMeshLines::SetSelectedLayer(int Value)
{
	if (m_SelectedLayer != Value)
	{
		if (m_SelectedLayer >= 0)
		{
			m_LayeredGridFilter->DeleteSpecifiedRegion(m_SelectedLayer);
		}
		m_SelectedLayer = Value;
		m_LayeredGridFilter->AddSpecifiedRegion(m_SelectedLayer);
	}

}