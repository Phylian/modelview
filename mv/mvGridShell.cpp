#include "mvGridShell.h"
#include "vtkGeometryFilter.h"

mvGridShell::mvGridShell()
{
	m_GeometryFilter = vtkGeometryFilter::New();
	SetMapperInput(m_GeometryFilter->GetOutput());
	m_Mapper->ScalarVisibilityOff();
	m_Actor->GetProperty()->SetColor(1.0, 0.8, 0.6);
	m_Actor->GetProperty()->SetOpacity(.2);
	m_LODActor->GetProperty()->SetColor(1.0, 0.8, 0.6);
	m_LODActor->GetProperty()->SetOpacity(.2);
}

mvGridShell::~mvGridShell()
{
	m_GeometryFilter->Delete();
}

void mvGridShell::SetInput(vtkDataSet *dataSet)
{
	m_GeometryFilter->SetInput(dataSet);
}

vtkDataSet *mvGridShell::GetInput()
{
	return m_GeometryFilter->GetInput();
}

vtkPolyData *mvGridShell::GetShell()
{
	return m_GeometryFilter->GetOutput();
}