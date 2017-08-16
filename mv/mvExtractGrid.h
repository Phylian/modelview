#ifndef __mvExtractGrid_h
#define __mvExtractGrid_h

#include "mvHeader.h"
#include "vtkExtractGrid.h"


// This is a temporary fix of a bug in vtkExtractGrid in VTK version 4.0
// The bug occurs when the grid is subsampled and the sample rate is greater than 1.
class MV_EXPORT mvExtractGrid : public vtkExtractGrid  
{
public:
	mvExtractGrid();
	~mvExtractGrid();
	static mvExtractGrid *New() {return new mvExtractGrid;};
	const char *GetClassName() {return "mvExtractGrid";};
	void Execute();
};

#endif


