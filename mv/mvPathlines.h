#ifndef __mvPathlines_h
#define __mvPathlines_h

#include "mvDisplayObject.h"

class vtkCleanPolyData;
class vtkClipPolyData;
class vtkPlane;
class vtkPolyData;
class vtkTransform;
class vtkTransformPolyDataFilter;
class vtkTubeFilter;
class vtkLookupTable;
class vtkLogLookupTable;

#define PATHLINE_LINE 1
#define PATHLINE_TUBE 0

class MV_EXPORT mvPathlines : public mvDisplayObject  
{
public:
	mvPathlines();
	virtual ~mvPathlines();

	void SetInput(vtkPolyData *polydata);
	void SetRepresentationToTube();
	void SetRepresentationToLine();
	int GetRepresentation() const;
	void SetDefaultTubeDiameter(float d);
	void SetNormalizedTubeDiameter(float d);
	float GetNormalizedTubeDiameter() const;
	void SetScale(float xScale, float yScale, float zScale);
	void DoCrop(const float *bounds);
	void CropOff();
	int IsCroppingOn() const;
	void TimeClippingOn();
	void TimeClippingOff();
	int IsTimeClippingOn() const;
	void SetTimeClippingRange(float minTime, float maxTime);
	void SetColorBarEndPoints(float valueBlue, float valueRed);
	float GetTimeRed() const {return m_TimeRed;}
	float GetTimeBlue() const {return m_TimeBlue;}
	int GetLogTransform() {return m_LogTransform;}
	void SetLogTransform(int Value);
	void SetMinPositiveValue(float Value);
	float GetMinPositiveValue() const {return m_MinPositiveValue;}

protected:
	vtkLookupTable *m_LutRedToBlue;
	vtkLookupTable *m_LutBlueToRed;
	vtkLogLookupTable *m_LogLutRedToBlue;
	vtkLogLookupTable *m_LogLutBlueToRed;
	vtkPolyData *m_Input;
	vtkClipPolyData *m_ClipMin;
	vtkClipPolyData *m_ClipMax;
	vtkTransform *m_Transform;
	vtkTransformPolyDataFilter *m_TransformFilter;
	vtkTubeFilter *m_Tube;
	vtkPlane *m_Plane[6];
	vtkClipPolyData *m_Cropper[6];
	int m_TimeClipping;
	int m_Cropping;
	int m_Representation;
	float m_DefaultTubeDiameter;
	float m_TimeRed;
	float m_TimeBlue;
	float m_MinPositiveValue;
	int m_LogTransform;
	void BuildPipeLine();
};

#endif
