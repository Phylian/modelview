#ifndef __mvBoundingBox_h
#define __mvBoundingBox_h

#include "mvDisplayObject.h"

class vtkOutlineSource;

class MV_EXPORT mvBoundingBox : public mvDisplayObject  
{
public:
	mvBoundingBox();
	virtual ~mvBoundingBox();

	void SetBounds(const float *bounds);
	const float *GetBounds() const;
	void GetBounds(float *bounds) const;

protected:
	vtkOutlineSource *m_Outline;
};

#endif
