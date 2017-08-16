#include "mvDisplayLine.h"
#include "vtkPolyData.h"
#include "vtkTubeFilter.h"

mvDisplayLine::mvDisplayLine()
{
	m_DefaultTubeDiameter = 1;
	m_Tube = vtkTubeFilter::New();
	m_Tube->SetNumberOfSides(10);
	SetMapperInput(m_Tube->GetOutput());
}

mvDisplayLine::~mvDisplayLine()
{
	m_Tube->Delete();
}

void mvDisplayLine::SetInput(vtkPolyData *in)
{
	m_Input = in;
	m_Tube->SetInput(in);
}

void mvDisplayLine::SetDefaultTubeDiameter(float d)
{
	m_Tube->SetRadius(GetNormalizedTubeDiameter() * d / 2);
	m_DefaultTubeDiameter = d;
}

void mvDisplayLine::SetNormalizedTubeDiameter(float dn)
{
	m_Tube->SetRadius(dn * m_DefaultTubeDiameter/2);
}

void mvDisplayLine::SetRepresentationToTube()
{
	SetMapperInput(m_Tube->GetOutput());
	m_Actor->GetProperty()->SetAmbient(0.0);
	m_Actor->GetProperty()->SetDiffuse(1.0);
}

void mvDisplayLine::SetRepresentationToLine()
{
	SetMapperInput(m_Input);
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
}

float mvDisplayLine::GetNormalizedTubeDiameter()
{
	return (2 * m_Tube->GetRadius() / m_DefaultTubeDiameter);
}

int mvDisplayLine::GetRepresentation()
{
	if (m_Mapper->GetInput() == m_Tube->GetOutput())
	{
		return 0;    // tube
	}
	else
	{
		return 1;    // line
	}
}
