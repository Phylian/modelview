// ExternalModflowFile.h: interface for the ExternalModflowFile class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(__ExternalModflowFile_h)
#define __ExternalModflowFile_h

#include "mvHeader.h"
#include "AbstractExternalFile.h"

class MV_EXPORT ExternalModflowFile : public AbstractExternalFile  
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
		int nlay, int nrow, int ncol, float *cellValues);

};

#endif // !defined(__ExternalModflowFile_h)
