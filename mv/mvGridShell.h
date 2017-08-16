#ifndef __mvGridShell_h
#define __mvGridShell_h

#include "mvDisplayObject.h"

class vtkGeometryFilter;
class vtkDataSet;
class vtkPolyData;

/**
 * Encapsulates a translucent shell that represents the outer
 * surface of the active grid
 */
class MV_EXPORT mvGridShell : public mvDisplayObject  
{
public:
	mvGridShell();
	virtual ~mvGridShell();

	void SetInput(vtkDataSet *dataSet);
	vtkDataSet *GetInput();
	vtkPolyData *GetShell();

protected:
	vtkGeometryFilter *m_GeometryFilter;
};

#endif
