#ifndef __mvAxes_h
#define __mvAxes_h

#include "mvDisplayObject.h"

class vtkAxes;
class vtkTubeFilter;
class vtkLookupTable;

/**
 * Encapsulates a cartesian (x-y-z) axis system. The axes are
 * represented by either tubes or line colored according to
 * red = x, green = y, blue = z)
 */
class MV_EXPORT mvAxes : public mvDisplayObject  
{
public:
	mvAxes();
	virtual ~mvAxes();

	void SetDefaultPositions(const float *p);
	void SetDefaultSize(float s);
	void SetDefaultTubeDiameter(float d);
	void SetNormalizedPosition(float x, float y, float z);
	void SetNormalizedSize(float size);
	void SetNormalizedTubeDiameter(float d);
	void SetRepresentationToTube();
	void SetRepresentationToLine();
	float GetNormalizedSize() const;
	float GetNormalizedTubeDiameter() const;
	const float *GetNormalizedPosition() const;
	int GetRepresentation() const;

private:
	vtkAxes *m_Axes;
	vtkTubeFilter *m_Tube;
	vtkLookupTable *m_LookupTable;

	float m_DefaultPositions[6];
	float m_NormalizedPosition[3];
	float m_DefaultTubeDiameter;
	float m_DefaultSize;
};

#endif
