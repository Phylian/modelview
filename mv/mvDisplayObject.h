#ifndef __mvDisplayObject_h
#define __mvDisplayObject_h

#include "mvHeader.h"
#include "vtkActor.h"
#include "vtkLODActor.h"
#include "vtkPolyDataMapper.h"
#include "vtkDataSetMapper.h"

class MV_EXPORT mvDisplayObject  
{
public:
	mvDisplayObject();
	virtual ~mvDisplayObject();

	vtkActor *GetActor() {return m_ActiveActor;}
	void VisibilityOn();
	void VisibilityOff();
	void SetVisibility(int v);
	int GetVisibility();
	void SetColor(float red, float green, float blue);
	void SetOpacity(float p);
	void SetDiffuse(float d);
	void SetAmbient(float a);
	void SetSpecular(float s);
	void SetSpecularPower(float sp);
	const float *GetColor();
	float GetOpacity();
	float GetDiffuse();
	float GetAmbient();
	float GetSpecular();
	float GetSpecularPower();
	void SetScale(float xScale, float yScale, float zScale);
	void SetImmediateModeRendering(int b);
	void UseLODActor(int b);
	void SetNumberOfCloudPoints(int n);
	void SetScalarRange(float min, float max);

protected:

	vtkMapper *m_Mapper;
	vtkActor *m_Actor;
	vtkLODActor *m_LODActor;
	vtkActor *m_ActiveActor;
	void SetMapperInput(vtkPolyData *in);
	void SetMapperInput(vtkDataSet *in);
};

// Note: for a display object to be used, its actor must be added
// to m_PropCollection. Usually this is done in mvManager::mvManager().

#endif
