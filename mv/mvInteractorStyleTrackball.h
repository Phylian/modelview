#ifndef __mvInteractorStyleTrackball_h
#define __mvInteractorStyleTrackball_h

#include "mvHeader.h"
#include "vtkInteractorStyleTrackball.h"

class MV_EXPORT mvInteractorStyleTrackball : public vtkInteractorStyleTrackball  
{
public:
	mvInteractorStyleTrackball() {};
	~mvInteractorStyleTrackball() {}
	static mvInteractorStyleTrackball *New() {return new mvInteractorStyleTrackball;};
	const char *GetClassName() {return "mvInteractorStyleTrackball";}

	virtual   void OnRightButtonUp  (int ctrl, int shift, int X, int Y);
	virtual   void OnLeftButtonUp  (int ctrl, int shift, int X, int Y);
};

#endif
