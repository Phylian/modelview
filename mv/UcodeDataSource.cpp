// mvUcodeSource.cpp: implementation of the mvUcodeSource class.
//
//////////////////////////////////////////////////////////////////////

#include "UcodeDataSource.h"
#include "mvUtil.h"

#define MAX_NUMBER_OF_MODEL_FEATURES 16

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

UcodeDataSource::UcodeDataSource()
{
	m_AxisLabels = 0;
	m_ParameterCount = 0;
	m_CanLogTransformX = FALSE;
	m_CanLogTransformY = FALSE;
	m_CanLogTransformZ = FALSE;
	m_LogTransformX = FALSE;
	m_LogTransformY = FALSE;
	m_LogTransformZ = FALSE;
	m_HasReadFile = FALSE;

}

UcodeDataSource::~UcodeDataSource()
{
	ReleaseMemory();
}

void UcodeDataSource::ReleaseMemory()
{
	if (m_AxisLabels != 0)
	{
		delete [] m_AxisLabels;
		m_AxisLabels = 0;
	}
}

void UcodeDataSource::ReloadData()
{
    char UcodeFile[256];

	char *pList = m_DataFileList;
	ParseDataFileList(pList, UcodeFile);

	{
		ifstream in(UcodeFile, ios::in|ios::nocreate);
		if (!in.is_open())
		{
			return;
		}
	
		char line[1024];
		in.getline(line, 1024);

		// read data

		ReadData(&in); 

	    in.close();
	}
	return;

}

char *UcodeDataSource::LoadData(char *dataFileList)
{
    char UcodeFile[256];

	// Save the dataFileList. This is necessary because the mvDoc checks
	// this variable to determine if data has been loaded.
	m_DataFileList = new char[strlen(dataFileList) + 1];

	strcpy(m_DataFileList, dataFileList);


	if (m_AxisLabels != 0)
	{
		delete [] m_AxisLabels;
		m_AxisLabels = 0;
	}
	m_AxisLabels = new char[1001];

	// Parse the data file list;
	char *pList = dataFileList;
	ParseDataFileList(pList, UcodeFile);
	
	{
		ifstream in(UcodeFile, ios::in|ios::nocreate);
		if (!in.is_open())
		{
			return "Unable to open the UCODE file.";
		}
	
		m_ParameterCount = ReadParameterNames(&in, m_AxisLabels);

		CountDimensions(&in);

		in.close();
	}

	{
		ifstream in(UcodeFile, ios::in|ios::nocreate);
		if (!in.is_open())
		{
			return "Unable to open the UCODE file.";
		}
	
		m_ParameterCount = ReadParameterNames(&in, m_AxisLabels);

		// read data
//		m_ScalarArray = new float[m_UnitOffset*2];
		m_ScalarArray = new float[m_UnitOffset];
		m_ScalarGridCoordinates = new float[3*m_UnitOffset];

		ReadData(&in); 

	    in.close();
	}

	m_NumberOfTimePoints = 1;
	m_TimePointLabels = new char *[m_NumberOfTimePoints];
	m_TimePointLabels[0] = new char[10];
	sprintf(m_TimePointLabels[0], "0");

	m_NumberOfScalarDataTypes = 1;
	m_DataTypeLabels = new char *[m_NumberOfScalarDataTypes];
	m_DataTypeLabels[0] = new char[10];
	sprintf(m_DataTypeLabels[0], "data");

	return 0;
}

void UcodeDataSource::ReadData(ifstream *in)
{
	double values[4] = {0,0,0,0};
//	int nxyz = m_ScalarGridDim[0] * m_ScalarGridDim[1] * m_ScalarGridDim[2];
	for (int counter = 0; counter < m_UnitOffset; counter++)
	{
		ReadDataLine(in, values);
		m_ScalarArray[counter] = values[0];
		m_ScalarGridCoordinates[counter*3] = values[1]; 
		m_ScalarGridCoordinates[counter*3+1] = values[2]; 
		m_ScalarGridCoordinates[counter*3+2] = values[3]; 

	}
	bool log_transform;
	float tolerance = 1e-5;
	if (!m_HasReadFile)
	{
		if (m_ScalarGridDim[2] > 1)
		{
			m_MinZ = m_ScalarGridCoordinates[2];
			m_MaxZ = m_ScalarGridCoordinates[(m_UnitOffset-1)*3+2];
		}
		log_transform = (m_ScalarGridDim[2] > 2);
		if (log_transform)
		{
			for (int counter = 0; counter < m_UnitOffset; counter++)
			{
				if (m_ScalarGridCoordinates[counter*3+2] <= 0)
				{
					log_transform = FALSE;
					break;
				}
			}
		}
		m_CanLogTransformZ = log_transform;
		if (log_transform)
		{
			float coord1 = m_ScalarGridCoordinates[2];
			float coord2 = m_ScalarGridCoordinates[5];
			float coord3 = m_ScalarGridCoordinates[8];
			float delta1 = coord2 - coord1;
			float delta2 = coord3 - coord2;
			log_transform = fabs(delta2-delta1)/(fabs(delta2)+fabs(delta1)) > tolerance;
		}
		m_LogTransformZ = log_transform; 


		if (m_ScalarGridDim[1] > 1)
		{
			m_MinY = m_ScalarGridCoordinates[1];
			m_MaxY = m_ScalarGridCoordinates[(m_UnitOffset-1)*3+1];
		}
		log_transform = (m_ScalarGridDim[1] > 2);
		if (log_transform)
		{
			for (int counter = 0; counter < m_UnitOffset; counter++)
			{
				if (m_ScalarGridCoordinates[counter*3+1] <= 0)
				{
					log_transform = FALSE;
					break;
				}
			}
		}
		m_CanLogTransformY = log_transform;
		if (log_transform)
		{
			float coord1 = m_ScalarGridCoordinates[1];
			float coord2 = m_ScalarGridCoordinates[1+3*m_ScalarGridDim[2]];
			float coord3 = m_ScalarGridCoordinates[1+6*m_ScalarGridDim[2]];
			float delta1 = coord2 - coord1;
			float delta2 = coord3 - coord2;
			log_transform = fabs(delta2-delta1)/(fabs(delta2)+fabs(delta1)) > tolerance;
		}
		m_LogTransformY = log_transform; 

		if (m_ScalarGridDim[0] > 1)
		{
			m_MinX = m_ScalarGridCoordinates[0];
			m_MaxX = m_ScalarGridCoordinates[(m_UnitOffset-1)*3];
		}
		log_transform = (m_ScalarGridDim[0] > 2);
		if (log_transform)
		{
			for (int counter = 0; counter < m_UnitOffset; counter++)
			{
				if (m_ScalarGridCoordinates[counter*3] <= 0)
				{
					log_transform = FALSE;
					break;
				}
			}
		}
		m_CanLogTransformX = log_transform;
		if (log_transform)
		{
			float coord1 = m_ScalarGridCoordinates[0];
			float coord2 = m_ScalarGridCoordinates[3*m_ScalarGridDim[1]*m_ScalarGridDim[2]];
			float coord3 = m_ScalarGridCoordinates[6*m_ScalarGridDim[1]*m_ScalarGridDim[2]];
			float delta1 = coord2 - coord1;
			float delta2 = coord3 - coord2;
			log_transform = fabs(delta2-delta1)/(fabs(delta2)+fabs(delta1)) > tolerance;
		}
		m_LogTransformX = log_transform; 

		m_HasReadFile = TRUE;
	}

	if (m_LogTransformZ)
	{
		double offset = m_ScalarGridCoordinates[2] - log10(m_ScalarGridCoordinates[2]);
		for (int counter = 0; counter < m_UnitOffset; counter++)
		{
			m_ScalarGridCoordinates[counter*3+2] = log10(m_ScalarGridCoordinates[counter*3+2]) + offset;
		}
	}
	if (m_LogTransformY)
	{
		double offset = m_ScalarGridCoordinates[1] - log10(m_ScalarGridCoordinates[1]);
		for (int counter = 0; counter < m_UnitOffset; counter++)
		{
			m_ScalarGridCoordinates[counter*3+1] = log10(m_ScalarGridCoordinates[counter*3+1]) + offset;
		}
	}
	if (m_LogTransformX)
	{
		double offset = m_ScalarGridCoordinates[0] - log10(m_ScalarGridCoordinates[0]);
		for (int counter = 0; counter < m_UnitOffset; counter++)
		{
			m_ScalarGridCoordinates[counter*3] = log10(m_ScalarGridCoordinates[counter*3]) + offset;
		}
	}
}


int UcodeDataSource::ReadDataLine(ifstream *in, double *number)
{
	char line[1024], buffer[1024];
	in->getline(line, 1024);
	for (int counter = 0 ; counter < m_ParameterCount; counter++)
	{
		mvUtil::TrimLeft(line);
		strcpy(buffer, line);
		char *code = strchr(buffer, ' ');
		if (code == 0 || (counter != m_ParameterCount-1 && code[1] == '\0'))
		{
			if (buffer == 0)
			{
				return 0;
			}
		}
		if (code != 0)
		{
			if (counter != m_ParameterCount-1 && code[1] == '\0')
			{
				return 0;
			}
		    *code = '\0';
		}
		number[counter] = atof(buffer);
		if ((code == 0) && (counter == m_ParameterCount-1))
		{
			return 1;
		}

		code = strchr(line, ' ');
		if (code == 0 || (counter != m_ParameterCount-1 && code[1] == '\0'))
		{
			return 0;
		}
		strcpy(line, code+1);
	
	}
	return 1;
}


void UcodeDataSource::CountDimensions(ifstream *in)
{
	double values[2][4] = {{0,0,0,0},{0,0,0,0}};
	int dimensions[3] = {1, 1, 1};
	int i = 0;
	bool readFirst = 0;
	while (!(*in).eof())
	{
		i = 1-i;
		if (ReadDataLine(in, values[i]) == 1)
			{
			if (readFirst)
			{
				if (values[0][1] != values[1][1])
				{
					dimensions[0] = dimensions[0] + 1; 
				}

				if (( dimensions[0] == 1) && (values[0][2] != values[1][2]))
				{
					dimensions[1] = dimensions[1] + 1; 
				}

				if (( dimensions[0] == 1) && ( dimensions[1] == 1) && (values[0][3] != values[1][3]))
				{
				dimensions[2] = dimensions[2] + 1; 
				}
	
			}
			else
			{
				readFirst = 1;
			}
		}
	}
	m_UnitOffset = 1;
	for (int counter = 0 ; counter < 3; counter++)
	{
		m_ScalarGridDim[counter] = dimensions[counter];
		m_UnitOffset = m_UnitOffset*dimensions[counter];
	}
}

int UcodeDataSource::ReadParameterNames(ifstream *in, char *parameterNames)
{
	int count = 0;
	char line[1024], buffer[1024];
	in->getline(line, 1024);

	// convert first quote to space and trim left.
	char *code = strchr(line, '"');
	if (code == 0 || code[1] == '\0')
	{
		// error: no quote
		return -1;
	}
	*code = ' ';
	mvUtil::TrimLeft(line);

	//copy text before second quote to buffer and add an end-of-line character
	strcpy(buffer, line);
	code = strchr(buffer, '"');
	if (code == 0 || code[1] == '\0')
	{
		// error: no matchinq quote
		return -1;
	}
	*code = '\0';
	mvUtil::TrimRight(buffer);
	strncat(buffer, "\n", 1);

	// remove text after second quote from line.
	code = strchr(line, '"');
	if (code == 0 || code[1] == '\0')
	{
		// error
		return -1;
	}
	strcpy(line, code+1);

	strcpy(parameterNames, buffer);
	char *nextName = parameterNames + strlen(parameterNames);
	
	count = 1;

	// Read up to 3 parameter names.
	for (int counter = 0 ; counter < 4; counter++)
	{
		// convert first quote to space and trim left.
		strcpy(buffer, line);
		code = strchr(buffer, '"');
		if (code == 0 || code[1] == '\0')
		{
			// reached last parameter before reading 3'rd parameter name.
			return count;
		}
		*code = ' ';
		mvUtil::TrimLeft(buffer);

		//copy text before second quote to buffer and add an end-of-line character
		code = strchr(buffer, '"');
		if (code == 0)
		{
			// error: no matchinq quote
			return -1;
		}
		*code = '\0';
		mvUtil::TrimRight(buffer);
		strncat(buffer, "\n", 1);

		// remove text after second quote from line.
		code = strchr(line, '"');
		if (code == 0 || code[1] == '\0')
		{
			// error
			return -1;
		}
		*code = ' ';
		code = strchr(line, '"');
		if (code == 0)
		{
			// error
			return -1;
		}
		strcpy(line, code+1);

		strcpy(nextName, buffer);
		nextName = nextName + strlen(buffer);
	
		count = count + 1;
	}
	
	return count;
}

void UcodeDataSource::AdvanceOneTimePoint()
{
	SetTimePointTo(-1);
}

void UcodeDataSource::SetTimePointTo(int timePointIndex)
{
}

void UcodeDataSource::SetScalarDataTypeTo(int dataTypeIndex)
{
	m_ScalarArrayOffset = dataTypeIndex * m_UnitOffset;
}

void UcodeDataSource::GetDefaultModelFeatureColor(int i, float *rgba)
{
	float m_Rgba[MAX_NUMBER_OF_MODEL_FEATURES][4] = {
		{.67, .67, .67, 1},		// fixed heads  --  light gray
		{1.0, 0.5, 1.0, 1},		// wells  --  light purple
		{0.5, 1.0, 0.5, 1},		// drains  --  light green
		{0.4, 0.7, 0.7, 1},		// rivers  --  blue gray
		{0.0, 0.5, 1.0, 1},		// streams --  royal blue
		{1.0, 1.0, 0.0, 1},		// reservoirs -- yellow
		{1.0, 0.5, .25, 1},		// general head -- orange
		{1.0, .16, .16, 1},	 	// time varying spec head  --  red
		{.82, .64, .64, 1},	    // transient specified flow  --  light brown
		{0.5, .25, .25, 1},		// transient specified head  -- dark brown
		{0.0, 0.5, 0.25, 1},	// drains with return flow --  dark green
		{0.0, 1.0, 1.0, 1},		// diff analogy flow --  cyan
		{1.0, 0.0, 1.0, 1},		// multi-node wells  -- purple 
		{0.5, 0.5, 1.0, 1},		// streams (SFR1) -- purplish blue 
		{0.58, 0.82, 0.95, 1},	// lake -- light grayish blue 
		{0.5, 0.84, 0.41, 1}	// GWT observations wells -- light grayish green 
	};

	rgba[0] = m_Rgba[i][0];
	rgba[1] = m_Rgba[i][1];
	rgba[2] = m_Rgba[i][2];
	rgba[3] = m_Rgba[i][3];
}

/**
* GetCanLogTransformXAxis should return true if a log transformation can
* be applied to the X Axis.
*/
bool UcodeDataSource::GetCanLogTransformXAxis()
{
	return 	m_CanLogTransformX;

}

/**
* GetCanLogTransformYAxis should return true if a log transformation can
* be applied to the Y Axis.
*/
bool UcodeDataSource::GetCanLogTransformYAxis()
{
	return 	m_CanLogTransformY;
}

/**
* GetCanLogTransformZAxis should return true if a log transformation can
* be applied to the Z Axis.
*/
bool UcodeDataSource::GetCanLogTransformZAxis()
{
	return 	m_CanLogTransformZ;
}

/**
* GetLogTransformXAxis is used to determine whether 
* a log transformation of the X Axis is turned  on or off.
*/
bool UcodeDataSource::GetLogTransformXAxis()
{
	return 	m_CanLogTransformX && m_LogTransformX;
}

/**
* GetLogTransformXAxis is used to determine whether 
* a log transformation of the Y Axis is turned  on or off.
*/
bool UcodeDataSource::GetLogTransformYAxis()
{
	return 	m_CanLogTransformY && m_LogTransformY;
}

/**
* GetLogTransformXAxis is used to determine whether 
* a log transformation of the Z Axis is turned  on or off.
*/
bool UcodeDataSource::GetLogTransformZAxis()
{
	return 	m_CanLogTransformZ && m_LogTransformZ;
}

/**
* SetLogTransformXAxis is used to turn on or off 
* a log transformation of the X Axis.
*/
void UcodeDataSource::SetLogTransformXAxis(bool transform)
{
	m_LogTransformX = transform;
	ReloadData();
}

/**
* SetLogTransformXAxis is used to turn on or off 
* a log transformation of the Y Axis.
*/
void UcodeDataSource::SetLogTransformYAxis(bool transform)
{
	m_LogTransformY = transform;
	ReloadData();
}

/**
* SetLogTransformXAxis is used to turn on or off 
* a log transformation of the Z Axis.
*/
void UcodeDataSource::SetLogTransformZAxis(bool transform)
{
	m_LogTransformZ = transform;
	ReloadData();
}

char *UcodeDataSource::XAxisLabel()
{
	if (m_ParameterCount <2)
	{
		return "X";
	}
	else
	{
		char* Label = new char[strlen(m_AxisLabels) + 100];
		char *AxisLabels = m_AxisLabels;
		ParseDataFileList(AxisLabels, Label);
		ParseDataFileList(AxisLabels, Label);
		char* finalLabel = new char[strlen(Label) + 5];
		strcpy(finalLabel,"X (");
		strcat(finalLabel, Label); 

		if (m_ScalarGridDim[0] > 1)
		{
			strcat(finalLabel, ": "); 
			char* buffer = new char[100];
			gcvt(m_MinX, 5, buffer);
			strcat(finalLabel, buffer); 
			strcat(finalLabel, " to "); 
			gcvt(m_MaxX, 5, buffer);
			strcat(finalLabel, buffer); 
		}

		strcat(finalLabel, ")"); 
		return finalLabel;
	}
}

char *UcodeDataSource::YAxisLabel()
{
	if (m_ParameterCount <3)
	{
		return "Y";
	}
	else
	{
		char* Label = new char[strlen(m_AxisLabels) + 100];
		char *AxisLabels = m_AxisLabels;
		ParseDataFileList(AxisLabels, Label);
		ParseDataFileList(AxisLabels, Label);
		ParseDataFileList(AxisLabels, Label);
		char* finalLabel = new char[strlen(Label) + 5];
		strcpy(finalLabel,"Y (");
		strcat(finalLabel, Label); 
		
		if (m_ScalarGridDim[0] > 1)
		{
			strcat(finalLabel, ": "); 
			char* buffer = new char[100];
			gcvt(m_MinY, 5, buffer);
			strcat(finalLabel, buffer); 
			strcat(finalLabel, " to "); 
			gcvt(m_MaxY, 5, buffer);
			strcat(finalLabel, buffer); 
		}

strcat(finalLabel, ")"); 
		return finalLabel;
	}
}

char *UcodeDataSource::ZAxisLabel()
{
	if (m_ParameterCount <4)
	{
		return "Z";
	}
	else
	{
		char* Label = new char[strlen(m_AxisLabels) + 100];
		char *AxisLabels = m_AxisLabels;
		ParseDataFileList(AxisLabels, Label);
		ParseDataFileList(AxisLabels, Label);
		ParseDataFileList(AxisLabels, Label);
		ParseDataFileList(AxisLabels, Label);
		char* finalLabel = new char[strlen(Label) + 5];
		strcpy(finalLabel,"Z (");
		strcat(finalLabel, Label); 

		if (m_ScalarGridDim[0] > 1)
		{
			strcat(finalLabel, ": "); 
			char* buffer = new char[100];
			gcvt(m_MinZ, 5, buffer);
			strcat(finalLabel, buffer); 
			strcat(finalLabel, " to "); 
			gcvt(m_MaxZ, 5, buffer);
			strcat(finalLabel, buffer); 
		}

		strcat(finalLabel, ")"); 
		return finalLabel;
	}
}
