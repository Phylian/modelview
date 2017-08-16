// FaceLess.h: interface for the FaceLess class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FACELESS_H__0A889C58_C6EA_4688_A3C9_F006E52E200A__INCLUDED_)
#define AFX_FACELESS_H__0A889C58_C6EA_4688_A3C9_F006E52E200A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "SutraFace.h"

class FaceLess  
{
public:
	FaceLess();
	virtual ~FaceLess();
	bool operator()(const class SutraFace* Face1, const  class SutraFace* Face2) 
		const {return Face1->m_sorted < Face2->m_sorted;};

};

#endif // !defined(AFX_FACELESS_H__0A889C58_C6EA_4688_A3C9_F006E52E200A__INCLUDED_)
