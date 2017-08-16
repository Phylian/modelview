#ifndef __mvDisplayLine_h
#define __mvDisplayLine_h

#include "mvDisplayObject.h"

#ifdef MV_DLL
#define MV_EXPORT __declspec( dllexport ) 
#else
#define MV_EXPORT __declspec( dllimport )
#endif

class vtkPolyData;
class vtkTubeFilter;

class MV_EXPORT mvDisplayLine : public mvDisplayObject  
{
public:
	mvDisplayLine();
	virtual ~mvDisplayLine();

	void SetDefaultTubeDiameter(float d);
	void SetNormalizedTubeDiameter(float d);
	float GetNormalizedTubeDiameter();
	void SetRepresentationToTube();
	void SetRepresentationToLine();
	int GetRepresentation();
	void SetInput(vtkPolyData *in);

protected:
	vtkTubeFilter *m_Tube;
	vtkPolyData *m_Input;
	float m_DefaultTubeDiameter;
};

#endif
