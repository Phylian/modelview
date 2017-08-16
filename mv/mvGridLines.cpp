#include "mvGridLines.h"
#include "vtkStructuredGrid.h"
#include "vtkStructuredGridGeometryFilter.h"
#include "vtkThreshold.h"
#include "vtkGeometryFilter.h"
#include "vtkFeatureEdges.h"

mvGridLines::mvGridLines()
{
	m_SGGeometryFilter = vtkStructuredGridGeometryFilter::New();
	m_Threshold = vtkThreshold::New();
	m_Threshold->SetInput(m_SGGeometryFilter->GetOutput());
	m_Threshold->SetAttributeModeToUseCellData();
	m_GeometryFilter = vtkGeometryFilter::New();
	m_GeometryFilter->SetInput(m_Threshold->GetOutput());
	m_FeatureEdges = vtkFeatureEdges::New();
	m_FeatureEdges->SetInput(m_GeometryFilter->GetOutput());
	m_FeatureEdges->ManifoldEdgesOn();
	m_FeatureEdges->BoundaryEdgesOn();
	SetMapperInput(m_FeatureEdges->GetOutput());
	m_Mapper->ScalarVisibilityOff();
	m_Actor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
	m_LODActor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_LODActor->GetProperty()->SetAmbient(1.0);
	m_LODActor->GetProperty()->SetDiffuse(0.0);
}

mvGridLines::~mvGridLines()
{
	m_SGGeometryFilter->Delete();
	m_Threshold->Delete();
	m_FeatureEdges->Delete();
	m_GeometryFilter->Delete();
}

void mvGridLines::SetInput(vtkStructuredGrid *dataSet)
{
	m_SGGeometryFilter->SetInput(dataSet);
}

void mvGridLines::SetThresholdMin(float value)
{
	m_Threshold->ThresholdByLower(value);
}

const int *mvGridLines::GetExtent() const
{
	return m_SGGeometryFilter->GetExtent();
}

void mvGridLines::SetExtent(int imin, int imax, int jmin, int jmax, int kmin, int kmax)
{
	m_SGGeometryFilter->SetExtent(imin, imax, jmin, jmax, kmin, kmax);
}

void mvGridLines::DoThresholdUsingCellData()
{
	m_Threshold->SetAttributeModeToUseCellData();
}

void mvGridLines::DoThresholdUsingPointData()
{
	m_Threshold->SetAttributeModeToUsePointData();
}

void mvGridLines::AssumeAllCellsAreActive(int b)
{
	if (b)
	{
		m_GeometryFilter->SetInput(m_SGGeometryFilter->GetOutput());
	}
	else
	{
		m_GeometryFilter->SetInput(m_Threshold->GetOutput());
	}
}