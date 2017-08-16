// mvFaceLess.h: interface for the mvFaceLess class.
// mvFaceLess is used to compare two mvSutraFaces to see how they
// should be arranged in order.  m_sorted contains the nodes in sorted
// order and it is used for the comparison.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FACELESS_H__0A889C58_C6EA_4688_A3C9_F006E52E200A__INCLUDED_)
#define AFX_FACELESS_H__0A889C58_C6EA_4688_A3C9_F006E52E200A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "mvSutraFace.h"

class mvFaceLess  
{
public:
	mvFaceLess();
	virtual ~mvFaceLess();
	bool operator()(const class mvSutraFace* Face1, 
		const  class mvSutraFace* Face2) 
		const {return Face1->m_sorted < Face2->m_sorted;};

};

#endif // !defined(AFX_FACELESS_H__0A889C58_C6EA_4688_A3C9_F006E52E200A__INCLUDED_)
