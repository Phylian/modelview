#ifndef __mvClipParticles_h
#define __mvClipParticles_h

#include "vtkImplicitFunction.h"

class mvClipParticles : public vtkImplicitFunction  
{
public:
	mvClipParticles();
	static mvClipParticles *New() {return new mvClipParticles;};
	void SetModelBounds(float bounds[6]);
	void SetBounds(float bounds[6]);
	virtual float EvaluateFunction(float x[3]);
    virtual void EvaluateGradient(float x[3], float g[3]);
	void SetAngle(int angle);
protected:
	float m_ModelBounds[6];
	float m_Bounds[6];
	int m_Angle;
	void UpdateInternalBounds();
private:
	float m_InternalBounds[6];
	float m_RadianAngle;

};

#endif
