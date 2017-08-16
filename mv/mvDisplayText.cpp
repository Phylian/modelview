#include "mvDisplayText.h"

mvDisplayText::mvDisplayText()
{
	m_TextMapper = vtkTextMapper::New();
    m_TextMapper->SetFontFamilyToArial();
    m_TextMapper->SetFontSize(14);
	m_TextActor = vtkActor2D::New();
	m_TextActor->SetMapper(m_TextMapper);
	m_TextActor->GetProperty()->SetColor(0, 0, 0);
	m_TextActor->SetPosition(10, 10);
	m_TextActor->VisibilityOff();
}

mvDisplayText::~mvDisplayText()
{
	m_TextMapper->Delete();
	m_TextActor->Delete();
}

void mvDisplayText::VisibilityOn()
{
	m_TextActor->VisibilityOn();
}

void mvDisplayText::VisibilityOff()
{
	m_TextActor->VisibilityOff();
}

void mvDisplayText::SetVisibility(int v)
{
	m_TextActor->SetVisibility(v);
}

int mvDisplayText::GetVisibility()
{
	return m_TextActor->GetVisibility();
}

void mvDisplayText::SetColor(float red, float green, float blue)
{
	m_TextActor->GetProperty()->SetColor(red, green, blue);
}

const float *mvDisplayText::GetColor()
{
	return m_TextActor->GetProperty()->GetColor();
}

void mvDisplayText::SetText(char *text)
{
	m_TextMapper->SetInput(text);
}

void mvDisplayText::SetPosition(float x, float y)
{
	m_TextActor->SetPosition(x, y);
}

const float *mvDisplayText::GetPosition()
{
	return m_TextActor->GetPosition();
}

void mvDisplayText::SetFontSize(int size)
{
	m_TextMapper->SetFontSize(size);
}

int mvDisplayText::GetFontSize()
{
	return m_TextMapper->GetFontSize();
}

