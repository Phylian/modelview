#include "mvDisplayObject.h"
#include "vtkActor.h"

mvDisplayObject::mvDisplayObject()
{
	m_Mapper = 0;
	m_Actor = vtkActor::New();
	m_Actor->VisibilityOff();
	m_LODActor = vtkLODActor::New();
	m_LODActor->VisibilityOff();
	m_ActiveActor = m_Actor;
}

mvDisplayObject::~mvDisplayObject()
{
	if (m_Mapper)
	{
		m_Mapper->Delete();
	}
	m_Actor->Delete();
	m_LODActor->Delete();
}

void mvDisplayObject::VisibilityOn()
{
	m_ActiveActor->VisibilityOn();
}

void mvDisplayObject::VisibilityOff()
{
	m_ActiveActor->VisibilityOff();
}

void mvDisplayObject::SetVisibility(int v)
{
	m_ActiveActor->SetVisibility(v);
}

int mvDisplayObject::GetVisibility()
{
	return m_ActiveActor->GetVisibility();
}

void mvDisplayObject::SetMapperInput(vtkPolyData *in)
{
	if (m_Mapper == 0)
	{
		m_Mapper = vtkPolyDataMapper::New();
		m_Actor->SetMapper(m_Mapper);
		m_LODActor->SetMapper(m_Mapper);
	}
	((vtkPolyDataMapper *) m_Mapper)->SetInput(in);
}

void mvDisplayObject::SetMapperInput(vtkDataSet *in)
{
	if (m_Mapper == 0)
	{
		m_Mapper = vtkDataSetMapper::New();
		m_Actor->SetMapper(m_Mapper);
		m_LODActor->SetMapper(m_Mapper);
	}
	((vtkDataSetMapper *) m_Mapper)->SetInput(in);
}

void mvDisplayObject::SetColor(float red, float green, float blue)
{
	m_Actor->GetProperty()->SetColor(red, green, blue);
	m_LODActor->GetProperty()->SetColor(red, green, blue);
}

const float *mvDisplayObject::GetColor()
{
	return m_ActiveActor->GetProperty()->GetColor();
}

void mvDisplayObject::SetOpacity(float p)
{
	m_Actor->GetProperty()->SetOpacity(p);
	m_LODActor->GetProperty()->SetOpacity(p);
}

float mvDisplayObject::GetOpacity()
{
	return m_ActiveActor->GetProperty()->GetOpacity();
}

void mvDisplayObject::SetDiffuse(float d)
{
	m_Actor->GetProperty()->SetDiffuse(d);
	m_LODActor->GetProperty()->SetDiffuse(d);
}

void mvDisplayObject::SetAmbient(float a)
{
	m_Actor->GetProperty()->SetAmbient(a);
	m_LODActor->GetProperty()->SetAmbient(a);
}

void mvDisplayObject::SetSpecular(float s)
{
	m_Actor->GetProperty()->SetSpecular(s);
	m_LODActor->GetProperty()->SetSpecular(s);
}

void mvDisplayObject::SetSpecularPower(float sp)
{
	m_Actor->GetProperty()->SetSpecularPower(sp);
	m_LODActor->GetProperty()->SetSpecularPower(sp);
}

float mvDisplayObject::GetDiffuse()
{
	return m_ActiveActor->GetProperty()->GetDiffuse();
}

float mvDisplayObject::GetAmbient()
{
	return m_ActiveActor->GetProperty()->GetAmbient();
}

float mvDisplayObject::GetSpecular()
{
	return m_ActiveActor->GetProperty()->GetSpecular();
}

float mvDisplayObject::GetSpecularPower()
{
	return m_ActiveActor->GetProperty()->GetSpecularPower();
}

void mvDisplayObject::SetScale(float xScale, float yScale, float zScale)
{
	m_Actor->SetScale(xScale, yScale, zScale);
	m_LODActor->SetScale(xScale, yScale, zScale);
}

void mvDisplayObject::SetImmediateModeRendering(int b)
{
	m_Mapper->SetImmediateModeRendering(b);
}

void mvDisplayObject::UseLODActor(int b)
{
	if (b)
	{
		if (m_ActiveActor == m_Actor)
		{
			m_LODActor->SetVisibility(m_Actor->GetVisibility());
		}
		m_ActiveActor = m_LODActor;
	}
	else
	{
		if (m_ActiveActor == m_LODActor)
		{
			m_Actor->SetVisibility(m_LODActor->GetVisibility());
		}
		m_ActiveActor = m_Actor;
	}
}

void mvDisplayObject::SetNumberOfCloudPoints(int n)
{
	m_LODActor->SetNumberOfCloudPoints(n);
}

void mvDisplayObject::SetScalarRange(float min, float max)
{
	if (m_Mapper != 0)
	{
		m_Mapper->SetScalarRange(min, max);
	}
}
