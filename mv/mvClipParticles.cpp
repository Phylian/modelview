// mvClipParticles.cpp: implementation of the mvClipParticles class.
//
//////////////////////////////////////////////////////////////////////

#include "mvClipParticles.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
mvClipParticles::mvClipParticles()
{
	m_Angle = 0;
}


void mvClipParticles::SetAngle(int angle)
{
	m_Angle = angle;
	UpdateInternalBounds();
	Modified();
} 

void mvClipParticles::SetBounds(float bounds[6])
{
	for (int i = 0; i<6; i++)
	{
		m_Bounds[i] = bounds[i];
	}
	UpdateInternalBounds();
	Modified();
} 

void mvClipParticles::SetModelBounds(float bounds[6])
{
	for (int i = 0; i<6; i++)
	{
		m_ModelBounds[i] = bounds[i];
	}
	UpdateInternalBounds();
	Modified();
} 

float mvClipParticles::EvaluateFunction(float x[3])
{
	float tempX = cos(-m_RadianAngle) * x[0] - sin(-m_RadianAngle) * x[1];
	float tempY = sin(-m_RadianAngle) * x[0] + cos(-m_RadianAngle) * x[1];

	float result = tempX - m_InternalBounds[0];
	float temp = m_InternalBounds[1] - tempX;
	if (temp < result)
	{
		result = temp;
	}

	temp = tempY - m_InternalBounds[2];
	if (temp < result)
	{
		result = temp;
	}
	temp = m_InternalBounds[3] - tempY;
	if (temp < result)
	{
		result = temp;
	}

	temp = x[2] - m_InternalBounds[4];
	if (temp < result)
	{
		result = temp;
	}
	temp = m_InternalBounds[5] - x[2];
	if (temp < result)
	{
		result = temp;
	}
	return result;

}

void mvClipParticles::EvaluateGradient(float x[3], float g[3])
{
	float tempX = cos(-m_RadianAngle) * x[0] - sin(-m_RadianAngle) * x[1];
	float tempY = sin(-m_RadianAngle) * x[0] + cos(-m_RadianAngle) * x[1];

	g[0] = (m_InternalBounds[1] + m_InternalBounds[0])/2 - tempX;
	g[1] = (m_InternalBounds[3] + m_InternalBounds[2])/2 - tempY;
	g[2] = (m_InternalBounds[5] + m_InternalBounds[4])/2 - x[2];

	return;
}

void mvClipParticles::UpdateInternalBounds()
{
	float deltax = m_ModelBounds[1] - m_ModelBounds[0];
	float deltay = m_ModelBounds[3] - m_ModelBounds[2];
	float deltaz = m_ModelBounds[5] - m_ModelBounds[4];

	if (m_Angle == 0)
	{
		m_RadianAngle = 0;
		m_InternalBounds[0] = m_Bounds[0]*deltax + m_ModelBounds[0];
		m_InternalBounds[1] = m_Bounds[1]*deltax + m_ModelBounds[0];
		m_InternalBounds[2] = m_Bounds[2]*deltay + m_ModelBounds[2];
		m_InternalBounds[3] = m_Bounds[3]*deltay + m_ModelBounds[2];
	}
	else
	{
		m_RadianAngle = m_Angle*1.745329e-2f;

		float tempX = cos(-m_RadianAngle) * m_ModelBounds[0] - sin(-m_RadianAngle) * m_ModelBounds[2];
		float tempY = sin(-m_RadianAngle) * m_ModelBounds[0] + cos(-m_RadianAngle) * m_ModelBounds[2];
		float MinX = tempX;
		float MinY = tempY;
		float MaxX = tempX;
		float MaxY = tempY;

		tempX = cos(-m_RadianAngle) * m_ModelBounds[1] - sin(-m_RadianAngle) * m_ModelBounds[2];
		tempY = sin(-m_RadianAngle) * m_ModelBounds[1] + cos(-m_RadianAngle) * m_ModelBounds[2];
		if (tempX < MinX)
		{
			MinX = tempX;
		}
		if (tempX > MaxX)
		{
			MaxX = tempX;
		}
		if (tempY < MinY)
		{
			MinY = tempY;
		}
		if (tempY > MaxY)
		{
			MaxY = tempY;
		}

		tempX = cos(-m_RadianAngle) * m_ModelBounds[0] - sin(-m_RadianAngle) * m_ModelBounds[3];
		tempY = sin(-m_RadianAngle) * m_ModelBounds[0] + cos(-m_RadianAngle) * m_ModelBounds[3];
		if (tempX < MinX)
		{
			MinX = tempX;
		}
		if (tempX > MaxX)
		{
			MaxX = tempX;
		}
		if (tempY < MinY)
		{
			MinY = tempY;
		}
		if (tempY > MaxY)
		{
			MaxY = tempY;
		}

		tempX = cos(-m_RadianAngle) * m_ModelBounds[1] - sin(-m_RadianAngle) * m_ModelBounds[3];
		tempY = sin(-m_RadianAngle) * m_ModelBounds[1] + cos(-m_RadianAngle) * m_ModelBounds[3];
		if (tempX < MinX)
		{
			MinX = tempX;
		}
		if (tempX > MaxX)
		{
			MaxX = tempX;
		}
		if (tempY < MinY)
		{
			MinY = tempY;
		}
		if (tempY > MaxY)
		{
			MaxY = tempY;
		}

		deltax = MaxX-MinX;
		deltay = MaxY-MinY;
		m_InternalBounds[0] = m_Bounds[0]*deltax + MinX;
		m_InternalBounds[1] = m_Bounds[1]*deltax + MinX;
		m_InternalBounds[2] = m_Bounds[2]*deltay + MinY;
		m_InternalBounds[3] = m_Bounds[3]*deltay + MinY;
	}
	m_InternalBounds[4] = m_Bounds[4]*deltaz + m_ModelBounds[4];
	m_InternalBounds[5] = m_Bounds[5]*deltaz + m_ModelBounds[4];
	if (deltaz == 0)
	{
		// 2D model
		m_InternalBounds[4] = m_InternalBounds[4] -1;
		m_InternalBounds[5] = m_InternalBounds[5] +1;
	}
}
