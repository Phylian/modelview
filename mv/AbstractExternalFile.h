// AbstractExternalFile.h: interface for the AbstractExternalFile class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(__AbstractExternalFile_h)
#define __AbstractExternalFile_h

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "mvHeader.h"

class MV_EXPORT AbstractExternalFile  
{
public:
private:
	
public:
	// iError = 0 - no error
	// iError = 1 - error reading file.
	// iError = 2 - array sizes don't match

	// Get the number of data sets
//    static void CountDataSets(char *externalFile, int &numDataSets, int &iError);
	// Read the names of all the data sets,
	static void GetDataLabels(char *externalFile, int dataTypeLength, char **dataTypes, int &iError);

};

#endif // !defined(__AbstractExternalFile_h)
