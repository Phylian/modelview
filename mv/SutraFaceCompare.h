#if !defined(FACE_COMPARE)
#define FACE_COMPARE

#include "SutraFace.h"

class FaceLess1
{
public:
	bool operator()(const SutraFace& Face1, const SutraFace& Face2) 
		const {return Face1.m_sorted < Face2.m_sorted;};
}

#endif // !defined(FACE_COMPARE)
