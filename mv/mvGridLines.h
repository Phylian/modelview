#ifndef __mvGridLines_h
#define __mvGridLines_h

#include "mvDisplayObject.h"

class vtkStructuredGrid;
class vtkStructuredGridGeometryFilter;
class vtkThreshold;
class vtkGeometryFilter;
class vtkFeatureEdges;

class MV_EXPORT mvGridLines : public mvDisplayObject  
{
public:
	mvGridLines();
	virtual ~mvGridLines();

	void SetInput(vtkStructuredGrid *dataSet);
	void SetThresholdMin(float value);
	const int *GetExtent() const;
	void SetExtent(int imin, int imax, int jmin, int jmax, int kmin, int kmax);
	void DoThresholdUsingCellData();
	void DoThresholdUsingPointData();
	void AssumeAllCellsAreActive(int b);

protected:
	vtkStructuredGridGeometryFilter *m_SGGeometryFilter;
	vtkThreshold *m_Threshold;
	vtkFeatureEdges *m_FeatureEdges;
	vtkGeometryFilter *m_GeometryFilter;
};

#endif
