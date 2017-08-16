#ifndef __mvStructuredGridOutlineFilter_h
#define __mvStructuredGridOutlineFilter_h

#include "mvHeader.h"
#include "vtkStructuredGridOutlineFilter.h"

class MV_EXPORT mvStructuredGridOutlineFilter : public vtkStructuredGridOutlineFilter  
{
public:
	static mvStructuredGridOutlineFilter *New() {return new mvStructuredGridOutlineFilter;};
	const char *GetClassName() {return "mvStructuredGridOutlineFilter";}

protected:
	mvStructuredGridOutlineFilter() {};
	~mvStructuredGridOutlineFilter() {}
	void Execute();

};

#endif
