// SutraFace.h: interface for the SutraFace class.
//
// SutraFace represents one face of a SUTRA element.  The node numberss 
// defining a face are stored twice: once in m_sorted, the other time in
// m_unsorted.  In the former, the node numbers will be arranged in 
// ascending order.  In the latter, they are in their original order.
//
// SutraFace is used in extracting the mesh shell. If and only if two 
// elements share the same face, the nodes in m_sorted will be the same
// and in the same order.  All element faces that are not shared will 
// be part of the mesh shell.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SUTRAFACE_H__3D0435E0_8C6A_419B_9FB8_60273A3D0946__INCLUDED_)
#define AFX_SUTRAFACE_H__3D0435E0_8C6A_419B_9FB8_60273A3D0946__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <vector>
#include <algorithm>

typedef std::vector<int> VecInt;

class SutraFace  
{
protected:
public:
	VecInt m_sorted;
	VecInt m_unsorted;
	SutraFace();
	virtual ~SutraFace();
	void sort();
	void push_back(const int x);
	int count();
	int node(int i);
};


#endif // !defined(AFX_SUTRAFACE_H__3D0435E0_8C6A_419B_9FB8_60273A3D0946__INCLUDED_)
