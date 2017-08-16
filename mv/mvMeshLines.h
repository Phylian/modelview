// mvMeshLines.h: interface for the mvMeshLines class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __mvMeshLines_h
#define __mvMeshLines_h

#include "mvDisplayObject.h"
#include "vtkPolyDataConnectivityFilter.h"

class vtkThreshold;
class vtkGeometryFilter;
class vtkFeatureEdges;
class vtkPolyData;


class mvMeshLines : public mvDisplayObject  
{
public:
	vtkPolyData * DataSet() const {return m_DataSet;} 
	mvMeshLines();
	virtual ~mvMeshLines();
	int GetSelectedLayer(){return m_SelectedLayer;}
	void SetSelectedLayer(int Value);
protected:
	int m_SelectedLayer;
	vtkPolyDataConnectivityFilter *m_LayeredGridFilter;
	vtkPolyData *m_DataSet;
};

#endif 
