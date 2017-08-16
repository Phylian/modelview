#include "mvCroppableDisplayObject.h"
#include "mvCustomAppendPolyData.h"
#include "vtkPolyDataMapper.h"
#include "vtkActor.h"

mvCroppableDisplayObject::mvCroppableDisplayObject()
{
	m_CroppedAwayPieces = mvCustomAppendPolyData::New();
	m_CroppedAwayPiecesMapper = vtkPolyDataMapper::New();
	m_CroppedAwayPiecesMapper->SetInput(m_CroppedAwayPieces->GetOutput());
	m_CroppedAwayPiecesActor = vtkActor::New();
	m_CroppedAwayPiecesActor->SetMapper(m_CroppedAwayPiecesMapper);
	m_CroppedAwayPiecesActor->GetProperty()->SetColor(1.0, 0.8, 0.6);
	m_CroppedAwayPiecesActor->GetProperty()->SetOpacity(0.2);

	m_Bounds[0] = 0;
	m_Bounds[1] = 1;
	m_Bounds[2] = 0;
	m_Bounds[3] = 1;
	m_Bounds[4] = 0;
	m_Bounds[5] = 1;
	m_CropPositions[0] = 0;
	m_CropPositions[1] = 1;
	m_CropPositions[2] = 0;
	m_CropPositions[3] = 1;
	m_CropPositions[4] = 0;
	m_CropPositions[5] = 1;
	for (int i=0; i<6; i++)
	{
		m_Plane[i] = vtkPlane::New();
	}
	m_Plane[0]->SetNormal(-1, 0, 0);
	m_Plane[1]->SetNormal(1, 0, 0);
	m_Plane[2]->SetNormal(0, -1, 0);
	m_Plane[3]->SetNormal(0, 1, 0);
	m_Plane[4]->SetNormal(0, 0, -1);
	m_Plane[5]->SetNormal(0, 0, 1);
	for (i=0; i<6; i++)
	{
		m_Cropper[i] = vtkClipPolyData::New();
		m_Cropper[i]->SetClipFunction(m_Plane[i]);
		m_Cropper[i]->InsideOutOn();
		m_Cropper[i]->GenerateClippedOutputOn();
		m_CroppedAwayPieces->AddInput(m_Cropper[i]->GetClippedOutput());
	}
	for (i=0; i<3; i++)
	{
		m_Cutter[i] = vtkCutter::New();
	}

}

mvCroppableDisplayObject::~mvCroppableDisplayObject()
{
	for (int i=0; i<6; i++)
	{
		m_Plane[i]->Delete();
		m_Cropper[i]->Delete();
	}
	for (i=0; i<3; i++)
	{
		m_Cutter[i]->Delete();
	}
	m_CroppedAwayPieces->Delete();
	m_CroppedAwayPiecesMapper->Delete();
	m_CroppedAwayPiecesActor->Delete();
}

void mvCroppableDisplayObject::SetBounds(float *bounds)
{
	for (int i=0; i<6; i++)
	{
		m_Bounds[i] = bounds[i];
	}
	UpdateCrop();
}


void mvCroppableDisplayObject::SetScale(float xScale, float yScale, float zScale)
{
	mvDisplayObject::SetScale(xScale, yScale, zScale);
	m_CroppedAwayPiecesActor->SetScale(xScale, yScale, zScale);
}

void mvCroppableDisplayObject::SetCropParameters(float xMin, float xMax, 
			float yMin, float yMax,	float zMin, float zMax, float cropAngle)
{
	m_CropPositions[0] = xMin;
	m_CropPositions[1] = xMax;
	m_CropPositions[2] = yMin;
	m_CropPositions[3] = yMax;
	m_CropPositions[4] = zMin;
	m_CropPositions[5] = zMax;
	if (cropAngle > 45)
	{
		m_HorizontalCropAngle = 45;
	}
	else if (cropAngle < -45)
	{
		m_HorizontalCropAngle = -45;
	}
	else
	{
		m_HorizontalCropAngle = cropAngle;
	}
	UpdateCrop();
}

void mvCroppableDisplayObject::UpdateCrop()
{
	float dx = m_Bounds[1] - m_Bounds[0];
	float dy = m_Bounds[3] - m_Bounds[2];
	float dz = m_Bounds[5] - m_Bounds[4];
	float midx = m_Bounds[0] + dx/2;
	float midy = m_Bounds[2] + dy/2;
	float midz = m_Bounds[4] + dz/2;
	float sn = sin(m_HorizontalCropAngle*1.745329e-2f);
	float cs = cos(m_HorizontalCropAngle*1.745329e-2f);
	m_Plane[0]->SetNormal(-cs, -sn, 0);
	m_Plane[1]->SetNormal(cs, sn, 0);
	m_Plane[2]->SetNormal(sn, -cs, 0);
	m_Plane[3]->SetNormal(-sn, cs, 0);
	if (m_CropPositions[0] == 0)
	{
		m_Plane[0]->SetOrigin(m_Bounds[0] - sn*dy/2, midy, midz);
	}
	else
	{
		m_Plane[0]->SetOrigin(m_Bounds[0] - sn*dy/2 + m_CropPositions[0] * (dx + sn*dy), midy, midz);
	}
	if (m_CropPositions[1] == 1)
	{
		m_Plane[1]->SetOrigin(m_Bounds[1] + sn*dy/2, midy, midz);
	}
	else
	{
		m_Plane[1]->SetOrigin(m_Bounds[0] - sn*dy/2 + m_CropPositions[1] * (dx + sn*dy), midy, midz);
	}

	if (m_CropPositions[2] == 0)
	{
		m_Plane[2]->SetOrigin(midx, m_Bounds[2] - sn*dx/2, midz);
	}
	else
	{
		m_Plane[2]->SetOrigin(midx, m_Bounds[2] - sn*dx/2 + m_CropPositions[2] * (dy + sn*dx), midz);
	}
	if (m_CropPositions[3] == 1)
	{
		m_Plane[3]->SetOrigin(midx, m_Bounds[3] + sn*dx/2, midz);
	}
	else
	{
		m_Plane[3]->SetOrigin(midx, m_Bounds[2] - sn*dx/2 + m_CropPositions[3] * (dy + sn*dx), midz);
	}

	if (m_CropPositions[4] == 0)
	{
		m_Plane[4]->SetOrigin(midx, midy, m_Bounds[4]);
	}
	else
	{
		m_Plane[4]->SetOrigin(midx, midy, m_Bounds[4] + m_CropPositions[4] * (m_Bounds[5] - m_Bounds[4]));
	}
	if (m_CropPositions[5] == 1)
	{
		m_Plane[5]->SetOrigin(midx, midy, m_Bounds[5]);
	}
	else
	{
		m_Plane[5]->SetOrigin(midx, midy, m_Bounds[4] + m_CropPositions[5] * (m_Bounds[5] - m_Bounds[4]));
	}
}

vtkActor *mvCroppableDisplayObject::GetCroppedAwayPiecesActor()
{
	return m_CroppedAwayPiecesActor;
}

void mvCroppableDisplayObject::SetCroppedAwayPiecesColor(float red, float green, float blue)
{
	m_CroppedAwayPiecesActor->GetProperty()->SetColor(red, green, blue);
}

const float *mvCroppableDisplayObject::GetCroppedAwayPiecesColor()
{
	return m_CroppedAwayPiecesActor->GetProperty()->GetColor();
}

void mvCroppableDisplayObject::SetCroppedAwayPiecesOpacity(float opacity)
{
	m_CroppedAwayPiecesActor->GetProperty()->SetOpacity(opacity);
}

float mvCroppableDisplayObject::GetCroppedAwayPiecesOpacity()
{
	return m_CroppedAwayPiecesActor->GetProperty()->GetOpacity();
}

void mvCroppableDisplayObject::CroppedAwayPiecesVisibilityOn()
{
	m_CroppedAwayPiecesActor->VisibilityOn();
}

void mvCroppableDisplayObject::CroppedAwayPiecesVisibilityOff()
{
	m_CroppedAwayPiecesActor->VisibilityOff();
}

void mvCroppableDisplayObject::SetCroppedAwayPiecesVisibility(int v)
{
	m_CroppedAwayPiecesActor->SetVisibility(v);
}

int mvCroppableDisplayObject::GetCroppedAwayPiecesVisibility()
{
	return m_CroppedAwayPiecesActor->GetVisibility();
}


void mvCroppableDisplayObject::SetDiffuse(float d)
{
	mvDisplayObject::SetDiffuse(d);
	m_CroppedAwayPiecesActor->GetProperty()->SetDiffuse(d);
}

void mvCroppableDisplayObject::SetAmbient(float a)
{
	mvDisplayObject::SetAmbient(a);
	m_CroppedAwayPiecesActor->GetProperty()->SetAmbient(a);
}

void mvCroppableDisplayObject::SetSpecular(float s)
{
	mvDisplayObject::SetSpecular(s);
	m_CroppedAwayPiecesActor->GetProperty()->SetSpecular(s);
}

void mvCroppableDisplayObject::SetSpecularPower(float sp)
{
	mvDisplayObject::SetSpecularPower(sp);
	m_CroppedAwayPiecesActor->GetProperty()->SetSpecularPower(sp);
}
