#include "mvBoundingBox.h"
#include "vtkOutlineSource.h"

mvBoundingBox::mvBoundingBox()
{
	m_Outline = vtkOutlineSource::New();
	SetMapperInput(m_Outline->GetOutput());
	m_Mapper->ScalarVisibilityOff();
	m_Actor->GetProperty()->SetColor(0, 0, 0);
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
}

mvBoundingBox::~mvBoundingBox()
{
	m_Outline->Delete();
}

void mvBoundingBox::SetBounds(const float *bounds)
{
	float b[6];
	memcpy(b, bounds, 6*sizeof(float));
	m_Outline->SetBounds(b);
}

const float *mvBoundingBox::GetBounds() const
{
	return m_Outline->GetBounds();
}

void mvBoundingBox::GetBounds(float *bounds) const
{
	const float *b = m_Outline->GetBounds();
	memcpy(bounds, b, 6*sizeof(float));
}
