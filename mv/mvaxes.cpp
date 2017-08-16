#include "mvAxes.h"
#include "vtkAxes.h"
#include "vtkTubeFilter.h"
#include "vtkLookupTable.h"

mvAxes::mvAxes()
{
	m_DefaultSize = 1;
	m_DefaultTubeDiameter = 1;
	m_DefaultPositions[0] = 0;
	m_DefaultPositions[1] = 1;
	m_DefaultPositions[2] = 0;
	m_DefaultPositions[3] = 1;
	m_DefaultPositions[4] = 0;
	m_DefaultPositions[5] = 1;
	m_NormalizedPosition[0] = 0;
	m_NormalizedPosition[1] = 0;
	m_NormalizedPosition[2] = 0;
	m_Axes = vtkAxes::New();
	m_Tube = vtkTubeFilter::New();
	m_Tube->SetNumberOfSides(10);
	m_Tube->SetInput(m_Axes->GetOutput());
	m_LookupTable = vtkLookupTable::New();
	m_LookupTable->SetNumberOfColors(3);
	m_LookupTable->SetTableValue(0, 1, 0, 0, 1);
	m_LookupTable->SetTableValue(1, 0, 1, 0, 1);
	m_LookupTable->SetTableValue(2, 0, 0, 1, 1);
	m_LookupTable->Build();
	SetMapperInput(m_Tube->GetOutput());
	m_Mapper->SetLookupTable(m_LookupTable);
	m_Mapper->SetScalarRange(0, 0.5);
}

mvAxes::~mvAxes()
{
	m_Axes->Delete();
	m_Tube->Delete();
	m_LookupTable->Delete();
}

void mvAxes::SetDefaultPositions(const float *p)
{
	m_DefaultPositions[0] = p[0];
	m_DefaultPositions[1] = p[1];
	m_DefaultPositions[2] = p[2];
	m_DefaultPositions[3] = p[3];
	m_DefaultPositions[4] = p[4];
	m_DefaultPositions[5] = p[5];

	float xo = m_NormalizedPosition[0]*(p[1]-p[0]) + p[0];
	float yo = m_NormalizedPosition[1]*(p[3]-p[2]) + p[2];
	float zo = m_NormalizedPosition[2]*(p[5]-p[4]) + p[4];

	m_Axes->SetOrigin(xo, yo, zo);

}

void mvAxes::SetDefaultSize(float s)
{
	m_Axes->SetScaleFactor(GetNormalizedSize() * s);
	m_DefaultSize = s;
}

void mvAxes::SetDefaultTubeDiameter(float d)
{
	m_Tube->SetRadius(GetNormalizedTubeDiameter() * d / 2);
	m_DefaultTubeDiameter = d;
}

void mvAxes::SetNormalizedPosition(float xn, float yn,float zn)
{
	m_NormalizedPosition[0] = xn;
	m_NormalizedPosition[1] = yn;
	m_NormalizedPosition[2] = zn;

	float xo = xn*(m_DefaultPositions[1]-m_DefaultPositions[0]) + m_DefaultPositions[0];
	float yo = yn*(m_DefaultPositions[3]-m_DefaultPositions[2]) + m_DefaultPositions[2];
	float zo = zn*(m_DefaultPositions[5]-m_DefaultPositions[4]) + m_DefaultPositions[4];

	m_Axes->SetOrigin(xo, yo, zo);
}

void mvAxes::SetNormalizedSize(float sn)
{
	m_Axes->SetScaleFactor(sn * m_DefaultSize);
}

void mvAxes::SetNormalizedTubeDiameter(float dn)
{
	m_Tube->SetRadius(dn * m_DefaultTubeDiameter/2);
}

void mvAxes::SetRepresentationToTube()
{
	SetMapperInput(m_Tube->GetOutput());
	m_Actor->GetProperty()->SetAmbient(0.0);
	m_Actor->GetProperty()->SetDiffuse(1.0);
}

void mvAxes::SetRepresentationToLine()
{
	SetMapperInput(m_Axes->GetOutput());
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
}

float mvAxes::GetNormalizedSize() const
{
	return (m_Axes->GetScaleFactor()/m_DefaultSize);
}

float mvAxes::GetNormalizedTubeDiameter() const
{
	return (2 * m_Tube->GetRadius() / m_DefaultTubeDiameter);
}

const float *mvAxes::GetNormalizedPosition() const
{
	return m_NormalizedPosition;
}

int mvAxes::GetRepresentation() const
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
