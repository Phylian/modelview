#ifndef __mvColorBar_h
#define __mvColorBar_h

#include "mvHeader.h"
#include "vtkActor2D.h"
#include "vtkLookupTable.h"
#include "vtkPolyDataMapper2D.h"
#include "vtkTextMapper.h"

#define MV_DEFAULT_COLOR_SCHEME 0 
#define MV_MODIFIED_COLOR_SCHEME 1 
#define MV_CUSTOM_COLOR_SCHEME 2

class MV_EXPORT mvColorBar : public vtkActor2D
{
public:
	static mvColorBar *New() {return new mvColorBar;}

	virtual void SetLookupTable(vtkLookupTable *lut);
	vtkLookupTable *GetLookupTable() const {return m_LookupTable;}

	void SetWidth(int w);
	int GetWidth() const {return m_BarWidth;}
	void SetHeight(int h);
	int GetHeight() const {return m_BarHeight;}
	void SetFontSize(int f);
	int GetFontSize() const {return m_FontSize;}
	void SetOffset(int f);
	int GetOffset() const {return m_BarOffset;}
	void SetLabelPrecision(int d);
	int GetLabelPrecision() const {return m_Precision;}
	void SetNumberOfLabels(int n);
	int GetNumberOfLabels() const {return m_NumberOfLabels;}

	void SetColorScheme(int value);
	int GetColorScheme() const {return m_ColorScheme;}

	int RenderOpaqueGeometry(vtkViewport* viewport);
	int RenderTranslucentGeometry(vtkViewport*) {return 0;};
	int RenderOverlay(vtkViewport* viewport);
	virtual void ReleaseGraphicsResources(vtkWindow *);
		int GetColorBarColorScheme() const;

	void SetScalarRange(float min, float max);
	void SetDataSourceIndex(int DataSourceIndex);

protected:
	mvColorBar();
	~mvColorBar();
	mvColorBar(const mvColorBar&) {};
	void operator=(const mvColorBar&) {};

	int m_BarWidth;
	int m_BarHeight;
	int m_BarOffset;
	int m_FontSize;
	int m_NumberOfLabels;
	int m_Precision;
	int m_NumberOfColors;
	int m_NumberOfLabelsBuilt;
	int m_ColorScheme;
	int m_DataSourceIndex;
	char *m_LabelFormat;
	float m_minColor;
	float m_maxColor;
	unsigned long m_FirstCustomColor;
	unsigned long m_LastCustomColor;
	void ReleaseTextMappersAndActors();

	vtkLookupTable *m_LookupTable;
	vtkPolyData *m_ColorBar;
	vtkPolyDataMapper2D *m_ColorBarMapper;
	vtkActor2D *m_ColorBarActor;
	vtkTextMapper **m_TextMappers;
	vtkActor2D **m_TextActors;
	vtkTimeStamp m_BuildTime;
};


#endif

