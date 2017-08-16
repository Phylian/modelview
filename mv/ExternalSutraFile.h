// ExternalSutraFile.h: interface for the ExternalSutraFile class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(__ExternalSutraFile_h)
#define __ExternalSutraFile_h

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "mvHeader.h"
#include "AbstractExternalFile.h"

class MV_EXPORT ExternalSutraFile : public AbstractExternalFile  
{
private:
	
public:
	// iError = 0 - no error
	// iError = 1 - error reading file.
	// iError = 2 - array sizes don't match

	// Get the number of data sets
    static void CountDataSets(char *externalFile, int &numDataSets, int &iError);
	// Read the names of all the data sets,
//	static void GetDataLabels(char *externalFile, int dataTypeLength, char **dataTypes, int &iError);
	// open the file and get the number of data sets and 
	// read the data for all the data sets and close the file.
    static void ReadData(char *externalFile, int &iError, 
		int num_Nodes, int num_Elements, float *cellValues, int *incidence,
		int NodesPerElement);

};

#endif // !defined(__ExternalSutraFile_h)
