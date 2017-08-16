// mvExternalMesh.cpp: implementation of the mvExternalMesh class.
//
//////////////////////////////////////////////////////////////////////

#include "mvExternalMesh.h"


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

mvExternalMesh::mvExternalMesh()
{
	m_DataSet = vtkPolyData::New();
	SetMapperInput(m_DataSet);
	
	m_Mapper->ScalarVisibilityOn();
	m_Actor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_Actor->GetProperty()->SetEdgeColor(0.5, 0.5, 0.5);
	m_Actor->GetProperty()->SetAmbient(1.0);
	m_Actor->GetProperty()->SetDiffuse(0.0);
	m_Actor->GetProperty()->SetRepresentationToWireframe();
	m_LODActor->GetProperty()->SetColor(0.5, 0.5, 0.5);
	m_LODActor->GetProperty()->SetEdgeColor(0.5, 0.5, 0.5);
	m_LODActor->GetProperty()->SetAmbient(1.0);
	m_LODActor->GetProperty()->SetDiffuse(0.0);
	m_LODActor->GetProperty()->SetRepresentationToWireframe();
}

mvExternalMesh::~mvExternalMesh()
{
	m_DataSet->Delete();

}

