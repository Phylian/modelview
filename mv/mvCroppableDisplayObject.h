#ifndef __mvCroppableDisplayObject_h
#define __mvCroppableDisplayObject_h

#include "mvDisplayObject.h"
#include "vtkPlane.h"
#include "vtkClipPolyData.h"
#include "vtkCutter.h"

class vtkActor;
class vtkPolyDataMapper;
class mvCustomAppendPolyData;

class MV_EXPORT mvCroppableDisplayObject : public mvDisplayObject  
{
public:
	mvCroppableDisplayObject();
	virtual ~mvCroppableDisplayObject();
	void SetBounds(float *bounds);
	void SetCropParameters(float xmin, float xmax, float ymin, float ymax,
			float zmin, float zmax, float cropAngle);
	const float *GetCropBounds() {return m_CropPositions;}

	vtkActor *GetCroppedAwayPiecesActor();
	void SetCroppedAwayPiecesColor(float red, float green, float blue);
	void SetCroppedAwayPiecesOpacity(float opacity);
	const float *GetCroppedAwayPiecesColor();
	float GetCroppedAwayPiecesOpacity();
	void CroppedAwayPiecesVisibilityOn();
	void CroppedAwayPiecesVisibilityOff();
	void SetCroppedAwayPiecesVisibility(int v);
	int GetCroppedAwayPiecesVisibility();

	float GetHorizontalCropAngle() {return m_HorizontalCropAngle;}

	void SetScale(float xScale, float yScale, float zScale);

	void SetDiffuse(float d);
	void SetAmbient(float a);
	void SetSpecular(float s);
	void SetSpecularPower(float sp);

protected:
	vtkCutter *m_Cutter[3];
	vtkPlane *m_Plane[6];
	vtkClipPolyData *m_Cropper[6];
	float m_CropPositions[6];
	float m_HorizontalCropAngle;
	float m_Bounds[6];

	mvCustomAppendPolyData *m_CroppedAwayPieces;
	vtkPolyDataMapper *m_CroppedAwayPiecesMapper;
	vtkActor *m_CroppedAwayPiecesActor;

	void UpdateCrop();
};

#endif
