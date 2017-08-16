// SutraFace.cpp: implementation of the SutraFace class.
//
//////////////////////////////////////////////////////////////////////

#include "mvSutraFace.h"
#include "mvFaceLess.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

mvSutraFace::mvSutraFace()
{

}

mvSutraFace::~mvSutraFace()
{

}

void mvSutraFace::push_back(const int x)
{
	m_sorted.push_back(x);
	m_unsorted.push_back(x);
}

void mvSutraFace::sort()
{
	std::sort(m_sorted.begin(), m_sorted.end());
}

int mvSutraFace::count()
{
	return m_unsorted.size();
}

int mvSutraFace::node(int i)
{
	return m_unsorted.at(i);
}

int mvSutraFace::FirstSortedNode()
{
	return m_sorted.at(0);
}
