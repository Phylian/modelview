// AbstractExternalFile.cpp: implementation of the AbstractExternalFile class.
//
//////////////////////////////////////////////////////////////////////

#include "AbstractExternalFile.h"
#include "mvUtil.h"
#include <stdlib.h>
#include <string.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

/*
void AbstractExternalFile::CountDataSets(char *externalFile, int &numDataSets, int &iError)
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
	if (!this->TestFormat(FormatCode))
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
*/

void AbstractExternalFile::GetDataLabels(char *externalFile, int dataTypeLength, char **dataTypes, int &iError)
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

