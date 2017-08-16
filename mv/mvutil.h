#ifndef __mvUtil_h
#define __mvUtil_h

#include "mvHeader.h"
#include <fstream.h>

class vtkObject;

class MV_EXPORT mvUtil  
{
public:
	static void DeleteVtkObject(vtkObject *obj);
    static void interp2d(float *x, float *y, float *z, float *dx, float *dy, float *zc,
			int numRow, int numCol, float znull);
	static void interp3d(float *cellDraw, float *ptDraw, float *dx, float *dy, float *zc,
			  int numCol, int numRow, int numLay, float drawNull, float znull, int *useLayer = 0);

	static int ExtractNumber(ifstream *in, int *buffer);

	static int ExtractNumber(ifstream *in, float *buffer);

	static int ExtractNumber(ifstream *in, double *buffer);

	static int ExtractString(ifstream *in, char *buffer, int len);

	static int ExtractLabelAndValue(ifstream *in, char *label, char *value);

	static void TrimLeft(char *aString);

	static void TrimRight(char *aString);

	static void ToLowerCase(char *aString);

    static float mvUtil::Fortran_atof(char *aString);
	static void ExtractFirstString(char *aString, char *FirstString);

};

#endif
