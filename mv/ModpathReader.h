#ifndef __ModpathReader_h
#define __ModpathReader_h

#include "mvHeader.h"
#include "mvModelFeatures.h"

/**
 * Abstract base class to specify dataset behavior. 
 */
class MV_EXPORT ModpathReader  
{
public:
	static void ReadData(char *pathlineFile, int &numPathlines, int &numCoordinates,
			float *&coordinates, float *&scalarArrayTime,  float *&scalarArrayMaxTime, 
			float *&scalarArrayMinTime, int *&pointArray, bool backwards, float &minPositiveTime);
};
#endif
