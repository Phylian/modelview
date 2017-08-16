// mvExternalMeshVector.cpp: implementation of the mvExternalMeshVector class.
//
//////////////////////////////////////////////////////////////////////

#include "mvExternalMeshVector.h"
#include "mvExternalMesh.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

mvExternalMeshVector::mvExternalMeshVector()
{
	m_ThreshholdOn = 0;
	m_Visible = 0;
	m_LowThreshHold = 0;
	m_HighThreshHold = 0;

}

mvExternalMeshVector::~mvExternalMeshVector()
{
	SetSize(0);
}

int mvExternalMeshVector::GetSize()
{
	return m_MeshVector.size();
}

void mvExternalMeshVector::SetSize(int Value)
{
	mvExternalMesh* Item = 0;
	while (Value < GetSize())
	{
		Item = GetItem(GetSize()-1);
		delete Item;
		m_MeshVector.pop_back();
	}
	while (Value > GetSize())
	{
		Item = new mvExternalMesh;
		m_MeshVector.push_back(Item);
	}
}

mvExternalMesh* mvExternalMeshVector::GetItem(int i)
{
	return m_MeshVector.at(i);
}

void mvExternalMeshVector::ThreshholdOn()
{
	m_ThreshholdOn = 1;
	UpdateVisibility();
}

void mvExternalMeshVector::ThreshholdOff()
{
	m_ThreshholdOn = 0;
	UpdateVisibility();
}

void mvExternalMeshVector::SetThreshhold(int LowValue, int HighValue)
{
	m_LowThreshHold = LowValue;
	m_HighThreshHold = HighValue;
	UpdateVisibility();
}

bool mvExternalMeshVector::GetThreshholdOn()
{
	return m_ThreshholdOn;
}

void mvExternalMeshVector::VisibilityOn()
{
	m_Visible = 1;
	UpdateVisibility();
}

void mvExternalMeshVector::VisibilityOff()
{
	m_Visible = 0;
	UpdateVisibility();
}

bool mvExternalMeshVector::GetVisibilityOn()
{
	return m_Visible;
}

void mvExternalMeshVector::UpdateVisibility()
{
	int Start = 0;
	int Stop = GetSize() -1;
	if (m_ThreshholdOn)
	{
		if (Start < m_LowThreshHold)
		{
			Start = m_LowThreshHold;
		}
		if (Stop > m_HighThreshHold)
		{
			Stop = m_HighThreshHold;
		}

	}
	for (int i = 0; i < GetSize(); i++)
	{
		mvExternalMesh* Item = GetItem(i);
		if (m_Visible && (i >= Start) && (i <= Stop))
		{
			Item->VisibilityOn();
		}
		else
		{
			Item->VisibilityOff();
		}
		}
}

void mvExternalMeshVector::SetImmediateModeRendering(int b)
{
	for (int i = 0; i < GetSize(); i++)
	{
		mvExternalMesh* Item = GetItem(i);
		Item->SetImmediateModeRendering(b);
	}
}

void mvExternalMeshVector::SetColor(float red, float green, float blue)
{
	for (int i = 0; i < GetSize(); i++)
	{
		mvExternalMesh* Item = GetItem(i);
		Item->SetColor(red, green, blue);
	}
}

void mvExternalMeshVector::SetScale(float xScale, float yScale, float zScale)
{
	for (int i = 0; i < GetSize(); i++)
	{
		mvExternalMesh* Item = GetItem(i);
		Item->SetScale(xScale, yScale, zScale);
	}
}
