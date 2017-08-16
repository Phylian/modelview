#ifndef __mvColorBandFilter_h
#define __mvColorBandFilter_h

#include "mvHeader.h"
#include "vtkPolyDataToPolyDataFilter.h"
#include "vtkClipPolyData.h"
#include "vtkAppendPolyData.h"

class MV_EXPORT mvColorBandFilter : public vtkPolyDataToPolyDataFilter  
{
public:
	mvColorBandFilter();
	~mvColorBandFilter();
	static mvColorBandFilter *New() {return new mvColorBandFilter;};
	const char *GetClassName() {return "mvColorBandFilter";}
	void PrintSelf(ostream& os, vtkIndent indent);
	void SetValues(float *values, int numValues);


protected:

	void Execute();
	void ReleaseMemory();

	int m_NumberOfValues;
	float *m_Values;
	vtkClipPolyData **m_Clipper;
	vtkAppendPolyData *m_Append;

};

#endif
