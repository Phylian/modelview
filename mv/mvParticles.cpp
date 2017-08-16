// mvParticles.cpp: implementation of the mvParticles class.
//
//////////////////////////////////////////////////////////////////////

#include "mvParticles.h"
#include "vtkPolyData.h"
#include "vtkGlyph3D.h"
#include "vtkCubeSource.h"
#include "vtkThresholdPoints.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkFloatArray.h"
#include "vtkLookupTable.h"
#include "vtkLogLookupTable.h"
#include "vtkClipPolyData.h"
#include "mvClipParticles.h"
#include "mvColorTable.h"
#include "mvLogColorTable.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

mvParticles::mvParticles()
{
	m_minConc = 0;
	m_maxConc = 0;
	m_MaxDisplayedConcentration = 0;
	m_MinDisplayedConcentration = 0;
	m_ParticleCount = 0;
	m_bounds[0] = m_bounds[1] = m_bounds[2] = m_bounds[3] = m_bounds[4] = m_bounds[5] = 0;

	m_ColorsSet = 0;
	m_ParticleCoordinates = 0;
	m_ParticleConcentrations = 0;
// Objects for glyph display
	m_PolyData = vtkPolyData::New();
	m_Glyph = vtkGlyph3D::New();
	m_CubeSource = vtkCubeSource::New();
	m_ThresholdPoints = vtkThresholdPoints::New();
	m_ThresholdPoints->ThresholdBetween(0.5, 10000);
	m_Transform = vtkTransform::New();
	m_TransformFilter = vtkTransformPolyDataFilter::New();
	m_ThresholdPoints->SetInput(m_PolyData);
	m_TransformFilter->SetInput(m_ThresholdPoints->GetOutput());
	m_TransformFilter->SetTransform(m_Transform);
	m_Glyph->SetInput(m_TransformFilter->GetOutput());
	m_Glyph->SetScaleModeToDataScalingOff();
	m_Glyph->SetColorModeToColorByScalar();
	m_Glyph->SetSource(m_CubeSource->GetOutput());
	m_DefaultGlyphSize = 1;
	UseLODActor(1);
	m_ConcentrationsSet = 0;
//	m_GlyphScalarArray = 0;

	m_CropFilter = vtkClipPolyData::New();
	m_CropParticles = 0;
	m_ClipParticles = mvClipParticles::New();
	m_CropFilter->SetClipFunction(m_ClipParticles);
	//m_CropFilter->GenerateClipScalarsOn();

	// Lookup tables
	m_LutBlueToRed = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutBlueToRed)->SetDefaultColorScheme();
//	m_LutBlueToRed->SetHueRange(.6667, 0);
//	m_LutBlueToRed->Build();
	m_LutRedToBlue = mvColorTable::New();
	dynamic_cast<mvColorTable*>(m_LutRedToBlue)->SetReversedDefaultColorScheme();
//	m_LutRedToBlue->SetHueRange(0, .6667);
//	m_LutRedToBlue->Build();
	m_LogLutBlueToRed = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutBlueToRed)->SetDefaultColorScheme();
//	m_LogLutBlueToRed->SetHueRange(.6667, 0);
	//m_LogLutBlueToRed->Build();
	m_LogLutRedToBlue = mvLogColorTable::New();
	dynamic_cast<mvLogColorTable*>(m_LogLutBlueToRed)->SetReversedDefaultColorScheme();
//	m_LogLutRedToBlue->SetHueRange(0, .6667);
//	m_LogLutRedToBlue->Build();
}

mvParticles::~mvParticles()
{
	m_PolyData->Delete();
	m_Glyph->Delete();
	m_CubeSource->Delete();
	m_ThresholdPoints->Delete();
	m_Transform->Delete();
	m_TransformFilter->Delete();
	m_CropFilter->Delete();
	m_ClipParticles->Delete();

	/*if (m_GlyphScalarArray != 0)
	{
		delete [] m_GlyphScalarArray;
	}*/
	m_LutBlueToRed->Delete();
	m_LutRedToBlue->Delete();
	m_LogLutBlueToRed->Delete();
	m_LogLutRedToBlue->Delete();

}

void mvParticles::SetParticleCoordinates(float *particleCoordinates)
{
	m_ParticleCoordinates = particleCoordinates;
}

void mvParticles::SetParticleConcentrations(float *particleConcentrations)
{
	m_ParticleConcentrations = particleConcentrations;
}

float mvParticles::minConcentration()
{
	if (ParticleCount() == 0)
	{
		return 0;
	}
	else
	{
		int i;
		float temp = m_ParticleConcentrations[0];
		for (i=1; i<ParticleCount(); i++)
		{
			if (temp > m_ParticleConcentrations[i])
			{
				temp = m_ParticleConcentrations[i];
			}
		}
		return temp;
	}
}

float mvParticles::maxConcentration()
{
	if (ParticleCount() == 0)
	{
		return 1;
	}
	else
	{
		int i;
		float temp = m_ParticleConcentrations[0];
		for (i=1; i<ParticleCount(); i++)
		{
			if (temp < m_ParticleConcentrations[i])
			{
				temp = m_ParticleConcentrations[i];
			}
		}
		return temp;
	}
}

void mvParticles::SetParticleCount(int particleCount)
{
	m_ParticleCount = particleCount;
}

void mvParticles::Build()
{
	SetMapperInput(m_Glyph->GetOutput());
    // side effect - creates m_Mapper if it doesn't already exist.

	int j;
	if (m_ParticleCount == 0)
	{
		return;
	}
	m_minConc = minConcentration();
	m_maxConc = maxConcentration();
	if (!m_ConcentrationsSet)
	{
		SetDisplayedConcentrations(m_maxConc/100, m_maxConc);
	}
    
	m_Mapper->SetLookupTable(m_LutBlueToRed);

	if (!m_ColorsSet)
	{
		m_Mapper->SetScalarRange(m_minConc, m_maxConc);
		m_ColorsSet = 1;
	}
	m_Actor->SetScale(1, 1, 1);

	vtkPoints *points = vtkPoints::New();
	vtkFloatArray *scalars = vtkFloatArray::New();
	scalars->SetNumberOfComponents(1);


	points->SetNumberOfPoints(ParticleCount());
	for (j=1; j<ParticleCount()+1; j++)
	{
		points->SetPoint(j-1, m_ParticleCoordinates[j*3-3], m_ParticleCoordinates[j*3-2], m_ParticleCoordinates[j*3-1]);
		// All glyphs are
		// set to be initially invisible.
	}
	scalars->SetArray(m_ParticleConcentrations, ParticleCount(), 1);

	m_PolyData->SetPoints(points);
	m_PolyData->GetPointData()->SetScalars(scalars);

	points->Delete();
	scalars->Delete();

	if (1 /* m_CropParticles*/)
	{
		m_CropFilter->SetInput(m_ThresholdPoints->GetOutput());
		m_TransformFilter->SetInput(m_CropFilter->GetOutput());
	}
	else
	{
		m_TransformFilter->SetInput(m_ThresholdPoints->GetOutput());
	};
	m_TransformFilter->Modified();

	m_PolyData->Modified();
}

void mvParticles::SetDefaultGlyphSize(float gs)
{
	m_CubeSource->SetXLength(gs);
	m_CubeSource->SetYLength(gs);
	m_CubeSource->SetZLength(gs);
}

void mvParticles::EnlargeGlyphs() 
{
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()*1.5);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()*1.5);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()*1.5);
}

void mvParticles::ShrinkGlyphs() 
{
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()/1.5);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()/1.5);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()/1.5);
}

void mvParticles::SetScale(float xScale, float yScale, float zScale)
{
	const float *ps = m_Transform->GetScale();
	m_Transform->Scale(xScale/ps[0], yScale/ps[1], zScale/ps[2]);
}

void mvParticles::SetDisplayedConcentrations(float minConcentration, float maxConcentration) 
{
	m_MinDisplayedConcentration = minConcentration;
	m_MaxDisplayedConcentration = maxConcentration;
	m_ConcentrationsSet = 1;
	m_ThresholdPoints->ThresholdBetween(m_MinDisplayedConcentration, m_MaxDisplayedConcentration);

}

void mvParticles::CropParticles(int ShouldCrop, float xmin, float xmax, float ymin, float ymax, float zmin, float zmax,
								float ModelBounds[6])
{
	m_CropParticles = ShouldCrop;
	m_bounds[0] = xmin;
	m_bounds[1] = xmax;
	m_bounds[2] = ymin;
	m_bounds[3] = ymax;
	m_bounds[4] = zmin;
	m_bounds[5] = zmax;
	m_ClipParticles->SetBounds(m_bounds);
	m_ClipParticles->SetModelBounds(ModelBounds);
	m_ClipParticles->SetAngle(0);
	if (1 /*m_CropParticles*/)
	{
		m_CropFilter->SetInput(m_ThresholdPoints->GetOutput());
		m_TransformFilter->SetInput(m_CropFilter->GetOutput());
		m_TransformFilter->Modified();
	}
	else
	{
		m_TransformFilter->SetInput(m_ThresholdPoints->GetOutput());
		m_TransformFilter->Modified();
	};

}

float mvParticles::GetGlyphSize() 
{
	return m_CubeSource->GetXLength();
}

void mvParticles::SetColorBarEndPoints(float valueBlue, float valueRed)
{
	float range[2];
	if (valueRed > valueBlue)
	{
		range[0] = valueBlue;
		range[1] = valueRed;
		m_LutBlueToRed->SetRange(range[0], range[1]);
		if (range[0]*range[1] > 0)
		{
			m_LogLutBlueToRed->SetRange(range[0], range[1]);
		}
		if (m_Mapper->GetLookupTable() == m_LutBlueToRed ||
				m_Mapper->GetLookupTable() == m_LutRedToBlue)
		{
			m_Mapper->SetLookupTable(m_LutBlueToRed);
		}
		else
		{
			m_Mapper->SetLookupTable(m_LogLutBlueToRed);
		}
	}
	else
	{
		range[0] = valueRed;
		range[1] = valueBlue;
		m_LutRedToBlue->SetRange(range[0], range[1]);
		if (range[0]*range[1] > 0)
		{
			m_LogLutRedToBlue->SetRange(range[0], range[1]);
		}
		if (m_Mapper->GetLookupTable() == m_LutBlueToRed ||
				m_Mapper->GetLookupTable() == m_LutRedToBlue)
		{
			m_Mapper->SetLookupTable(m_LutRedToBlue);
		}
		else
		{
			m_Mapper->SetLookupTable(m_LogLutRedToBlue);
		}
	}
	m_Mapper->SetScalarRange(range);
	m_ColorsSet = 1;
}

void mvParticles::Reset()
{
	m_ConcentrationsSet = 0;
	m_ColorsSet = 0;
}
