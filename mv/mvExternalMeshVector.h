// mvExternalMeshVector.h: interface for the mvExternalMeshVector class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MVEXTERNALMESHVECTOR_H__E8DCA2BD_2CAB_4712_B157_ED6D13741A4C__INCLUDED_)
#define AFX_MVEXTERNALMESHVECTOR_H__E8DCA2BD_2CAB_4712_B157_ED6D13741A4C__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>

class mvExternalMesh;

typedef std::vector<mvExternalMesh*> MeshVector;


class mvExternalMeshVector  
{
public:
	mvExternalMeshVector();
	virtual ~mvExternalMeshVector();
	int GetSize();
	void SetSize(int Value);
	mvExternalMesh* GetItem(int i);
	void ThreshholdOn();
	void ThreshholdOff();
	void SetThreshhold(int LowValue, int HighValue);
	bool GetThreshholdOn();
	void VisibilityOn(); 
	void VisibilityOff(); 
	bool GetVisibilityOn(); 
	void SetImmediateModeRendering(int b);
	void SetColor(float red, float green, float blue);
	void SetScale(float xScale, float yScale, float zScale);
protected:
	MeshVector m_MeshVector;
	int m_LowThreshHold;
	int m_HighThreshHold;
	bool m_ThreshholdOn;
	bool m_Visible;
	void UpdateVisibility();

};

#endif // !defined(AFX_MVEXTERNALMESHVECTOR_H__E8DCA2BD_2CAB_4712_B157_ED6D13741A4C__INCLUDED_)
