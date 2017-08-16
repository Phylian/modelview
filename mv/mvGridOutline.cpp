#include "mvGridOutline.h"
#include "vtkStructuredGrid.h"
#include "vtkStructuredGridOutlineFilter.h"
#include "vtkPolyData.h"
#include "vtkPointSet.h"

mvGridOutline::mvGridOutline()
{
	m_OutlineFilter = vtkStructuredGridOutlineFilter::New();
//	m_MeshOutlineFilter = vtkPolyData::New();
	SetMapperInput(m_OutlineFilter->GetOutput());
	m_Mapper->ScalarVisibilityOff();
	m_Actor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
	m_LODActor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_LODActor->GetProperty()->SetAmbient(1.0);
	m_LODActor->GetProperty()->SetDiffuse(0.0);
}

mvGridOutline::~mvGridOutline()
{
	m_OutlineFilter->Delete();
/*	if (m_MeshOutlineFilter != 0)
	{
		m_MeshOutlineFilter->Delete();
	}*/
}

void mvGridOutline::SetInput(vtkStructuredGrid *dataSet)
{
	m_OutlineFilter->SetInput(dataSet);
//	SetMapperInput(m_OutlineFilter->GetOutput());
}

/*
void mvGridOutline::SetMeshInput()
{
	SetMapperInput(m_MeshOutlineFilter);
}
*/

