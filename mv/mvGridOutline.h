#ifndef __mvGridOutline_h
#define __mvGridOutline_h

#include "mvDisplayObject.h"

class vtkStructuredGrid;
class vtkStructuredGridOutlineFilter;
class vtkPolyData;
class vtkPointSet;
class vtkPolyData;

class MV_EXPORT mvGridOutline : public mvDisplayObject  
{
public:
//	vtkPolyData *m_MeshOutlineFilter;
	mvGridOutline();
	virtual ~mvGridOutline();

	void SetInput(vtkStructuredGrid *dataSet);
//	void SetMeshInput();

protected:
	vtkStructuredGridOutlineFilter *m_OutlineFilter;
};

#endif
