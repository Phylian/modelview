// ExternalModflowFile.cpp: implementation of the ExternalModflowFile class.
//
//////////////////////////////////////////////////////////////////////

#include "ExternalModflowFile.h"
#include "mvUtil.h"
#include <stdlib.h>
#include <string.h>


// open the file and get the number of data sets
void ExternalModflowFile::CountDataSets(char *externalFile, int &numDataSets, int &iError)
{
	iError = 1;
	ifstream in(externalFile, ios::in|ios::nocreate);
	char line[1024];
	do
	{
		in.getline(line, 1024);
		mvUtil::TrimLeft(line);
		mvUtil::TrimRight(line);
	} while ((strlen(line) == 0) || (line[0] == '#'));

	// line now contains the format code. At present only one format code is defined - 1.
	// There is no need to handle the format code at present so just read the next line.
	int FormatCode = atoi(line);
	if (FormatCode != 1)
	{
		numDataSets = -1;
		return;
	}
	
	in.getline(line, 1024);
	mvUtil::TrimLeft(line);
	mvUtil::TrimRight(line);
	numDataSets = atoi(line);
	if (numDataSets != 0)
	{
		iError = 0;
	}

	in.close();
}

/*
void ExternalModflowFile::GetDataLabels(char *externalFile, int dataTypeLength, char **dataTypes, int &iError)
{
	iError = 1;
	ifstream in(externalFile, ios::in|ios::nocreate);
	char line[1024];
	do
	{
		in.getline(line, 1024);
		mvUtil::TrimLeft(line);
		mvUtil::TrimRight(line);
	} while ((strlen(line) == 0) || (line[0] == '#'));

	// line now contains the format code. At present only one format code is defined - 1.
	// There is no need to handle the format code at present so just read the next line.
	
	in.getline(line, 1024);
	mvUtil::TrimLeft(line);
	mvUtil::TrimRight(line);
	int numDataSets = atoi(line);
	if (numDataSets == 0)
	{
		in.close();
		return;
	}

	int dataIndex;

	for (dataIndex=0; dataIndex<numDataSets; dataIndex++ )
	{
		in.getline(line, 1024);
		line[dataTypeLength-1]='\0';
		mvUtil::TrimLeft(line);
		mvUtil::TrimRight(line);
		strcpy(dataTypes[dataIndex], line);
	}
	iError = 0;
}
*/

// read the data for all the data sets 
void ExternalModflowFile::ReadData(char *externalFile, int &iError, 
	int nlay, int nrow, int ncol, float *cellValues)
{
	iError = 1;
	ifstream in(externalFile, ios::in|ios::nocreate);
	char line[1024];
	do
	{
		in.getline(line, 1024);
		mvUtil::TrimLeft(line);
		mvUtil::TrimRight(line);
	} while ((strlen(line) == 0) || (line[0] == '#'));
	// line now contains the format code. At present only one format code is defined - 1.
	// There is no need to handle the format code at present so just read the next line.
	
	in.getline(line, 1024);
	mvUtil::TrimLeft(line);
	mvUtil::TrimRight(line);
	int numDataSets = atoi(line);
	if (numDataSets == 0)
	{
		in.close();
		return;
	}

	int dataIndex;

	// skp the names of the data sets.
	for (dataIndex=0; dataIndex<numDataSets; dataIndex++ )
	{
		in.getline(line, 1024);
	}

	int LayerCount, RowCount, ColCount;
	int CellIndex, CellIndex2;

	double AValue;
	for (dataIndex=0; dataIndex<numDataSets; dataIndex++ )
	{
		in >> LayerCount >> RowCount >> ColCount;
		if ((ColCount    != ncol) || (RowCount   != nrow) || 
			((LayerCount != nlay) && (LayerCount != 1)))
		{
			iError = 2;
			in.close();
			return;
		}
		int NColRow = RowCount*ColCount;
		int ncell = nlay*RowCount*ColCount;
		//CellCount = 0;
		for (int LayIndex = 0; LayIndex < LayerCount; LayIndex++)
		{
			for (int RowIndex = 0; RowIndex < RowCount; RowIndex++)
			{
				for (int ColIndex = 0; ColIndex < ColCount; ColIndex++)
				{
					in >> AValue;
					// The rows and layers are numbered in the opposite direction 
					// in MODFLOW than they are in Model Viewer. 
					// This makes the necessary adjustment.
					CellIndex = dataIndex*ncell 
						+ (nlay-LayIndex-1)*NColRow
						+ (RowCount-RowIndex-1)*ColCount
						+ ColIndex;
					cellValues[CellIndex] = float(AValue);
				}
			}
		}


		// If the data set has one layer but the model has more than one layer
		// copy the data from the first layer into the other layers. 
		if (LayerCount != nlay)
		{
			//CellCount2 = 0;
			for (int LayIndex = 1; LayIndex < nlay; LayIndex++)
			{
				for (int RowIndex = 0; RowIndex < RowCount; RowIndex++)
				{
					for (int ColIndex = 0; ColIndex < ColCount; ColIndex++)
					{
						CellIndex = dataIndex*ncell 
							+ (nlay-LayIndex-1)*NColRow
							+ (RowCount-RowIndex-1)*ColCount
							+ ColIndex;
						CellIndex2 = dataIndex*ncell 
							+ (nlay-1)*NColRow
							+ (RowCount-RowIndex-1)*ColCount
							+ ColIndex;
						cellValues[CellIndex] = cellValues[CellIndex2];
					}
				}
			}
		}
	}
	iError = 0;

	in.close();
}



