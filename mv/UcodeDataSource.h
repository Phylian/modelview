// mvUcodeSource.h: interface for the mvUcodeSource class.
//
//////////////////////////////////////////////////////////////////////

#ifndef __UcodeDataSource_h
#define __UcodeDataSource_h

#include "mvDataSource.h"
#include "mvModelFeatures.h"

class UcodeDataSource : public mvDataSource  
{
protected:
	double m_MinX;
	double m_MaxX;
	double m_MinY;
	double m_MaxY;
	double m_MinZ;
	double m_MaxZ;
	int m_ParameterCount;
	char *m_AxisLabels;
	int ReadParameterNames(ifstream *in, char *parameterNames);
	virtual void ReleaseMemory();
	void CountDimensions(ifstream *in);
	int ReadDataLine(ifstream *in, double *number);
	void ReadData(ifstream *in);
	void ReloadData();
	bool m_CanLogTransformX;
	bool m_CanLogTransformY;
	bool m_CanLogTransformZ;
	bool m_LogTransformX;
	bool m_LogTransformY;
	bool m_LogTransformZ;
	bool m_HasReadFile;
public:
	UcodeDataSource();
	virtual ~UcodeDataSource();
	virtual int GetPrimaryScalarMode() {return MV_POINT_SCALARS;}
	virtual char *LoadData(char *dataFileList);
	static char *GetNameStatic() {return "UCODE 2005 - SOS";}
	virtual char *GetName() {return GetNameStatic();}
	virtual int GetGridType() {return MV_RECTILINEAR_GRID_ALL_ACTIVE;}
	virtual void AdvanceOneTimePoint();
	virtual void SetTimePointTo(int timePointIndex);
	virtual void SetScalarDataTypeTo(int dataTypeIndex);
	virtual int GetModelFeatureDisplayMode() {return MV_DISPLAY_MODEL_FEATURES_AS_CELLS;}
	virtual void GetDefaultModelFeatureColor(int i, float *rgba);

	/**
	* GetCanLogTransformXAxis should return true if a log transformation can
	* be applied to the X Axis.
	*/
	virtual bool GetCanLogTransformXAxis();

	/**
	* GetCanLogTransformYAxis should return true if a log transformation can
	* be applied to the Y Axis.
	*/
	virtual bool GetCanLogTransformYAxis();

	/**
	* GetCanLogTransformZAxis should return true if a log transformation can
	* be applied to the Z Axis.
	*/
	virtual bool GetCanLogTransformZAxis();

	/**
	* GetLogTransformXAxis is used to determine whether 
	* a log transformation of the X Axis is turned  on or off.
	*/
	virtual bool GetLogTransformXAxis();

	/**
	* GetLogTransformXAxis is used to determine whether 
	* a log transformation of the Y Axis is turned  on or off.
	*/
	virtual bool GetLogTransformYAxis();

	/**
	* GetLogTransformXAxis is used to determine whether 
	* a log transformation of the Z Axis is turned  on or off.
	*/
	virtual bool GetLogTransformZAxis();

	/**
	* SetLogTransformXAxis is used to turn on or off 
	* a log transformation of the X Axis.
	*/
	virtual void SetLogTransformXAxis(bool transform);

	/**
	* SetLogTransformXAxis is used to turn on or off 
	* a log transformation of the Y Axis.
	*/
	virtual void SetLogTransformYAxis(bool transform);

	/**
	* SetLogTransformXAxis is used to turn on or off 
	* a log transformation of the Z Axis.
	*/
	virtual void SetLogTransformZAxis(bool transform);
	virtual char *XAxisLabel();
	virtual char *YAxisLabel();
	virtual char *ZAxisLabel();


};

#endif 
