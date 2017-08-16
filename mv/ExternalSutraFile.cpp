// ExternalSutraFile.cpp: implementation of the ExternalSutraFile class.
//
//////////////////////////////////////////////////////////////////////

#include "ExternalSutraFile.h"
#include "mvUtil.h"
#include <string.h>
#include <stdlib.h>



void ExternalSutraFile::ReadData(char *externalFile, int &iError, 
	int num_Nodes, int num_Elements, float *cellValues, int *incidence,
	int NodesPerElement)
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

	// read data sets here.
	int NumValues;
	double AValue;
	int StartIndex = 0;
	for (dataIndex=0; dataIndex<numDataSets; dataIndex++ )
	{
		in.getline(line, 1024);
		mvUtil::TrimLeft(line);
		mvUtil::TrimRight(line);
		mvUtil::ToLowerCase(line);
		in >> NumValues;
		if (strcmp(line, "nodes") == 0)
		{
			if (NumValues != num_Nodes)
			{
				iError = 2;
				in.close();
				return;
			}
			int NodeIndex = StartIndex;
			for (int ValueIndex = 0; ValueIndex < NumValues; ValueIndex++)
			{
				in >> AValue;
				cellValues[NodeIndex] = float(AValue);
				NodeIndex++;
			}
			int ElementIndex = StartIndex + num_Nodes;
			for (int i=0; i<num_Elements; i++)
			{
				cellValues[i+ElementIndex] = 0;
			}

			// Calculate average values in each elements for nodal data.
			int NodeI = -1;
			for (int j=0; j<num_Elements; j++)
			{
				for (int NIndex=0; NIndex < NodesPerElement; NIndex++)
				{
					NodeI += 1;
					int Node = incidence[NodeI];
					if ((Node < 0) || (Node >= num_Nodes))
					{
						return;
					}
					cellValues[j+StartIndex + num_Nodes] += cellValues[Node+StartIndex];

				}
				cellValues[j+StartIndex + num_Nodes] /= NodesPerElement;	
			}


		}
		else if (strcmp(line, "elements") == 0)
		{
			if (NumValues != num_Elements)
			{
				iError = 2;
				in.close();
				return;
			}
			int ElementIndex = StartIndex + num_Nodes;
			for (int ValueIndex = 0; ValueIndex < NumValues; ValueIndex++)
			{
				in >> AValue;
				cellValues[ElementIndex] = float(AValue);
				ElementIndex++;
			}
		}
		else
		{
			iError = 2;
			in.close();
			return;
		}
		StartIndex = StartIndex + num_Nodes + num_Elements;

	}

	iError = 0;

	in.close();

}

void ExternalSutraFile::CountDataSets(char *externalFile, int &numDataSets, int &iError)
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
	if (FormatCode != 2)
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
