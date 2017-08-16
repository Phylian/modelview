// mvExternalMesh.h: interface for the mvExternalMesh class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(__mvExternalMesh_h)
#define __mvExternalMesh_h

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "mvDisplayObject.h"
#include <vtkPolyData.h>

class vtkPolyData;


class mvExternalMesh : public mvDisplayObject  
{
public:
	vtkPolyData * DataSet() const {return m_DataSet;} 
	mvExternalMesh();
	virtual ~mvExternalMesh();
protected:
	vtkPolyData *m_DataSet;

};

#endif // !defined(AFX_MVEXTERNALMESH_H__2AC11BB4_033D_4AA5_B904_73B12D35DCB3__INCLUDED_)
