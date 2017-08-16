#include "stdafx.h"
#include "ModelViewer.h"
#include "DataFilesDialog.h"
#include <fstream.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <direct.h>

#include "Modflow2000DataFilesDlg.h"
#include "Mf2kgwtDataFilesDlg.h"
#include "Modflow96DataFilesDlg.h"
#include "Moc3dDataFilesDlg.h"
#include "Mt3dmsDataFilesDlg.h"
#include "SutraDataFilesDlg.h"
#include "PhastDataFilesDlg.h"
#include "UcodeDataFileDlg.h"

#include "Modflow2000DataSource.h"
#include "Mf2kgwtDataSource.h"
#include "Modflow96DataSource.h"
#include "Moc3dDataSource.h"
#include "Mt3dmsDataSource.h"
#include "SutraDataSource.h"
#include "ProgressWnd.h"
#include "SutraReader.h"
#include "PhastDataSource.h"
#include "UcodeDataSource.h"

char *DataFilesDialog::GetDataFileList(char *model)
{
	if (stricmp(model, Modflow2000DataSource::GetNameStatic()) == 0)
	{
		return GetModflow2000DataFiles();
	}
	if (stricmp(model, Mf2kgwtDataSource::GetNameStatic()) == 0)
	{
		return GetMf2kgwtDataFiles();
	}
	if (stricmp(model, Modflow96DataSource::GetNameStatic()) == 0)
	{
		return GetModflow96DataFiles();
	}
	if (stricmp(model, Moc3dDataSource::GetNameStatic()) == 0)
	{
		return GetMoc3dDataFiles();
	}
	if (stricmp(model, Mt3dmsDataSource::GetNameStatic()) == 0)
	{
		return GetMt3dmsDataFiles();
	}
	if (stricmp(model, SutraDataSource::GetNameStatic()) == 0)
	{
		return GetSutraDataFiles();
	}
	if (stricmp(model, PhastDataSource::GetNameStatic()) == 0)
	{
		return GetPhastDataFiles();
	}
	if (stricmp(model, UcodeDataSource::GetNameStatic()) == 0)
	{
		return GetUcodeSosFiles();
	}

	return NULL;
}

char *DataFilesDialog::GetModflow2000DataFiles()
{
	CModflow2000DataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}

	// For now, we just use the file name and not the path.
	// TO DO: Determine the path relative to the document file.
	BOOL setdir = FALSE;
	CString name = dlg.m_NameFile;
	int pos = name.ReverseFind('\\');
	if (pos != -1)
	{
		_chdir((LPCTSTR) name.Left(pos));
		setdir = TRUE;
		name = name.Right(name.GetLength() - pos - 1);
	}
	CString path = dlg.m_PathFile;
	pos = path.ReverseFind('\\');
	if (pos != -1)
	{
		if (!setdir)
		{
			_chdir((LPCTSTR) path.Left(pos));
			setdir = TRUE;
		}
		path = path.Right(path.GetLength() - pos - 1);
	}

	CString external = dlg.m_ExternalFile;
	pos = external.ReverseFind('\\');
	if (pos != -1)
	{
		if (!setdir)
		{
			_chdir((LPCTSTR) external.Left(pos));
			setdir = TRUE;
		}
		external = external.Right(external.GetLength() - pos - 1);
	}

	char *dataFileList = new char[name.GetLength() + path.GetLength() + external.GetLength() + 20];
	strcpy(dataFileList, (LPCTSTR) name);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) path);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) external);
	strcat(dataFileList, "\n");
	char dataCode[10];
	sprintf(dataCode, "%d\n", dlg.m_DataType);
	strcat(dataFileList, dataCode);

	if (dlg.m_PathlineBackwards)
	{
		strcat(dataFileList, "1");
	}
	else
	{
		strcat(dataFileList, "0");
	}
	strcat(dataFileList, "\n");
	
	return dataFileList;
}

char *DataFilesDialog::GetMf2kgwtDataFiles()
{
	CMf2kgwtDataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}
	// For now, we just use the file name and not the path.
	// TO DO: Determine the path relative to the document file.
	CString name = dlg.m_NameFile;
	int pos = name.ReverseFind('\\');
	if (pos != -1)
	{
		name = name.Right(name.GetLength() - pos - 1);
	}
	CString path = dlg.m_PathFile;
	pos = path.ReverseFind('\\');
	if (pos != -1)
	{
		path = path.Right(path.GetLength() - pos - 1);
	}
	char *dataFileList = new char[name.GetLength() + path.GetLength() + 20];
	strcpy(dataFileList, (LPCTSTR) name);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) path);
	strcat(dataFileList, "\n");
	strcat(dataFileList, "\n");
	char dataCode[10];
	sprintf(dataCode, "%d\n", dlg.m_DataType);
	strcat(dataFileList, dataCode);

	if (dlg.m_PathlineBackwards)
	{
		strcat(dataFileList, "1");
	}
	else
	{
		strcat(dataFileList, "0");
	}
	strcat(dataFileList, "\n");
	
	return dataFileList;
}

char *DataFilesDialog::GetModflow96DataFiles()
{
	CModflow96DataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}

	// For now, we just use the file name and not the path.
	// TO DO: Determine the path relative to the document file.
	BOOL setdir = FALSE;
	CString name = dlg.m_NameFile;
	int pos = name.ReverseFind('\\');
	if (pos != -1)
	{
		_chdir((LPCTSTR) name.Left(pos));
		setdir = TRUE;
		name = name.Right(name.GetLength() - pos - 1);
	}
	CString elev = dlg.m_ElevationFile;
	pos = elev.ReverseFind('\\');
	if (pos != -1)
	{
		if (!setdir)
		{
			_chdir((LPCTSTR) elev.Left(pos));
			setdir = TRUE;
		}
		elev = elev.Right(elev.GetLength() - pos - 1);
	}
	CString path = dlg.m_PathlineFile;
	pos = path.ReverseFind('\\');
	if (pos != -1)
	{
		if (!setdir)
		{
			_chdir((LPCTSTR) path.Left(pos));
			setdir = TRUE;
		}
		path = path.Right(path.GetLength() - pos - 1);
	}
	char *dataFileList = new char[name.GetLength() + elev.GetLength() 
										+ path.GetLength() + 20];
	strcpy(dataFileList, (LPCTSTR) name);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) elev);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) path);
	strcat(dataFileList, "\n");
//	strcat(dataFileList, "\n");
	char dataCode[10];
	sprintf(dataCode, "%d\n", dlg.m_DataType);
	strcat(dataFileList, dataCode);

	if (dlg.m_PathlineBackwards)
	{
		strcat(dataFileList, "1");
	}
	else
	{
		strcat(dataFileList, "0");
	}
	strcat(dataFileList, "\n");
	
	return dataFileList;
}

char *DataFilesDialog::GetMoc3dDataFiles()
{
	CMoc3dDataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}
	// For now, we just use the file name and not the path.
	// TO DO: Determine the path relative to the document file.
	CString name = dlg.m_NameFile;
	int pos = name.ReverseFind('\\');
	if (pos != -1)
	{
		name = name.Right(name.GetLength() - pos - 1);
	}
	CString top = dlg.m_TopFile;
	pos = top.ReverseFind('\\');
	if (pos != -1)
	{
		top = top.Right(top.GetLength() - pos - 1);
	}
	CString path = dlg.m_PathFile;
	pos = path.ReverseFind('\\');
	if (pos != -1)
	{
		path = path.Right(path.GetLength() - pos - 1);
	}
	char *dataFileList = new char[name.GetLength() + top.GetLength() + 
											path.GetLength() + 20];
	strcpy(dataFileList, (LPCTSTR) name);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) top);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) path);
	strcat(dataFileList, "\n");
	strcat(dataFileList, "\n");
	char dataCode[10];
	sprintf(dataCode, "%d\n", dlg.m_DataType);
	strcat(dataFileList, dataCode);

	if (dlg.m_PathlineBackwards)
	{
		strcat(dataFileList, "1");
	}
	else
	{
		strcat(dataFileList, "0");
	}
	strcat(dataFileList, "\n");
	
	return dataFileList;
}

char *DataFilesDialog::GetMt3dmsDataFiles()
{
	CMt3dmsDataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}

	// For now, we just use the file name and not the path.
	// TO DO: Determine the path relative to the document file.
	CString cnf = dlg.m_CnfFile;
	int pos = cnf.ReverseFind('\\');
	if (pos != -1)
	{
		cnf = cnf.Right(cnf.GetLength() - pos - 1);
	}
	CString ucn = dlg.m_UcnFile;
	pos = ucn.ReverseFind('\\');
	if (pos != -1)
	{
		ucn = ucn.Right(ucn.GetLength() - pos - 1);
	}
	CString ftl = dlg.m_LinkFile;
	pos = ftl.ReverseFind('\\');
	if (pos != -1)
	{
		ftl = ftl.Right(ftl.GetLength() - pos - 1);
	}
	CString path = dlg.m_PathFile;
	pos = path.ReverseFind('\\');
	if (pos != -1)
	{
		path = path.Right(path.GetLength() - pos - 1);
	}
	char *dataFileList = new char[cnf.GetLength() + ucn.GetLength() + 
				ftl.GetLength() + + path.GetLength() + 10];
	strcpy(dataFileList, (LPCTSTR) cnf);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) ucn);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) ftl);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) path);
	strcat(dataFileList, "\n");
	char dataCode[4];
	sprintf(dataCode, "%d\n", dlg.m_DataType);
	strcat(dataFileList, dataCode);

	if (dlg.m_PathlineBackwards)
	{
		strcat(dataFileList, "1");
	}
	else
	{
		strcat(dataFileList, "0");
	}
	strcat(dataFileList, "\n");
	
	return dataFileList;
}

char *DataFilesDialog::GetSutraDataFiles()
{
	CSutraDataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}

	BOOL setdir = FALSE;
	CString nodFile = dlg.m_NodFile;
	CString eleFile = dlg.m_EleFile;
	CString inpFile = dlg.m_SutraInpFile;
	int fileType = dlg.m_FileType;
	int timeUnits = dlg.m_TimeUnits;
	BOOL convert = FALSE;
	char *dataFileList;

	// If user loads ascii files, prompt for conversion
	if (fileType == 0)
	{
		if (AfxMessageBox("Do you want to convert the ascii files to binary?", MB_YESNO) == IDYES)
		{
			CFileDialog fileDlg(FALSE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
				"Binary File (*.bin)|*.bin|All Files (*.*)|*.*||");
			if (fileDlg.DoModal() != IDOK)
			{
				return NULL;
			}
			CString binFile = fileDlg.GetPathName();

			// If the file name does not have a suffix (file extension), add the bin suffix
			if (fileDlg.GetFileExt().IsEmpty())
			{
				binFile = binFile + ".bin";
			}


			if (!SutraConvert((LPCTSTR) nodFile, (LPCTSTR) eleFile, (LPCTSTR) inpFile, (LPCTSTR) binFile, timeUnits))
			{
				return NULL;
			}
			nodFile = binFile;
			eleFile.Empty();
			inpFile.Empty();
			fileType = 1;
		}

	}

	int pos = nodFile.ReverseFind('\\');
	if (pos != -1)
	{
		_chdir((LPCTSTR) nodFile.Left(pos));
		setdir = TRUE;
		nodFile = nodFile.Right(nodFile.GetLength() - pos - 1);
	}
	pos = eleFile.ReverseFind('\\');
	if (pos != -1)
	{
		if (!setdir)
		{
			_chdir((LPCTSTR) eleFile.Left(pos));
			setdir = TRUE;
		}
		eleFile = eleFile.Right(eleFile.GetLength() - pos - 1);
	}
	pos = inpFile.ReverseFind('\\');
	if (pos != -1)
	{
		if (!setdir)
		{
			_chdir((LPCTSTR) inpFile.Left(pos));
			setdir = TRUE;
		}
		inpFile = inpFile.Right(inpFile.GetLength() - pos - 1);
	}
	dataFileList = new char[nodFile.GetLength() + eleFile.GetLength() + 
				inpFile.GetLength() + 10];
	strcpy(dataFileList, (LPCTSTR) nodFile);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) eleFile);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) inpFile);
	strcat(dataFileList, "\n");
	if (fileType == 0)
	{
		// ascii data (nodFile and eleFile)
		strcat(dataFileList, "-1\n");
	}
	else
	{
		// binary data
		strcat(dataFileList, "0\n");
	}
	char tunits[1];
	sprintf(tunits, "%d", timeUnits);
	strcat(dataFileList, tunits);
	strcat(dataFileList, "\n");
	return dataFileList;
}

int DataFilesDialog::SutraConvert(const char *nodFile, const char *eleFile, 
								  const char *inpFile, const char *binFile, int &TimeUnits)
{
	ifstream nod;
	ifstream ele;
	ofstream bin;

	nod.open(nodFile, ios::in|ios::nocreate);
	if (!nod.is_open())
	{
		AfxMessageBox("Unable to open the \"nod\" file");
		return 0;
	}
	if (strlen(eleFile) > 0)
	{
		ele.open(eleFile, ios::in|ios::nocreate);
		if (!ele.is_open())
		{
			AfxMessageBox("Unable to open the \"ele\" file");
			return 0;
		}
	}
	if (strlen(binFile) > 0)
	{
		bin.open(binFile, ios::binary|ios::out);
		if (!bin.is_open())
		{
			AfxMessageBox("Unable to write the binary output file");
			return 0;
		}
	}

	int numNodes, numElements, numScalarDataTypes, numTimePoints, nz, numNodalScalarDataTypes;
	bool iceSatFraction;
	int *Incidence = 0;
	int numElementTimePoints;
	int sdim[3];
	if (strlen(inpFile) > 0)
	{
		char *errmsg = SutraDataSource::ReadDimensions(nod, numNodes, numElements, sdim);
		if (errmsg)
		{
			AfxMessageBox(errmsg);
			return 0;
		}

		nod.close();
		nod.open(nodFile, ios::in|ios::nocreate);
		if (numElements == 0)
		{
			Incidence = new int[numNodes*8];
		}
		else
		{
			Incidence = new int[numElements*8];
		}
	}
	
	char *errmsg = SutraDataSource::AtoBConvert1(nod, ele, inpFile, bin, 
				numNodes, numElements, numScalarDataTypes, numTimePoints, iceSatFraction, nz, 
				numNodalScalarDataTypes, numElementTimePoints, Incidence, TimeUnits);
	if (errmsg)
	{
		if (Incidence != 0)
		{
			delete [] Incidence;
		}
		AfxMessageBox(errmsg);
		return 0;
	}

	// close and then open again the nod and ele files so 
	// we can start reading from the beginning.
	nod.close();
	nod.open(nodFile, ios::in|ios::nocreate);
	if (ele.is_open())
	{
		ele.close();
		ele.open(eleFile, ios::in|ios::nocreate);
	}

	((CFrameWnd *) AfxGetApp()->m_pMainWnd)->GetActiveView()->SendMessage(WM_PAINT);
	CProgressWnd wndProgress;
	wndProgress.GoModal("Ascii To Binary Conversion");
	wndProgress.SetRange(0,numTimePoints);

	for (int k=0; k<numTimePoints; k++)
	{
		if (errmsg == 0)
		{
			errmsg = SutraDataSource::AtoBConvert2(nod, ele, bin, k, numNodes, 
				numElements, numScalarDataTypes, numTimePoints, nz, 
				numNodalScalarDataTypes, numElementTimePoints, Incidence);
		}
		wndProgress.StepIt();
		wndProgress.PeekAndPump(FALSE);
	}
	if (Incidence != 0)
	{
		delete [] Incidence;
	}

	if (errmsg)
	{
		AfxMessageBox(errmsg);
		return 0;
	}
	return 1;
}

char *DataFilesDialog::GetPhastDataFiles()
{
	CPhastDataFilesDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}

	CString strTitle = dlg.m_strHDFDataFile;
	CString strPath;
	int pos = strTitle.ReverseFind('\\');
	if (pos != -1)
	{
		strPath = strTitle.Left(pos + 1);
		strTitle = strTitle.Right(strTitle.GetLength() - pos - 1);
	}
	char *dataFileList = new char[strTitle.GetLength() + strPath.GetLength() + 20];
	strcpy(dataFileList, (LPCTSTR) strTitle);
	strcat(dataFileList, "\n");
	strcat(dataFileList, (LPCTSTR) strPath);
	strcat(dataFileList, "\n");
	return dataFileList;
}

char *DataFilesDialog::GetUcodeSosFiles()
{
	CUcodeDataFileDlg dlg;
	if (dlg.DoModal() != IDOK)
	{
		return NULL;
	}

	// For now, we just use the file name and not the path.
	// TO DO: Determine the path relative to the document file.
	BOOL setdir = FALSE;
	CString name = dlg.m_UcodeSosFile;
	int pos = name.ReverseFind('\\');
	if (pos != -1)
	{
		_chdir((LPCTSTR) name.Left(pos));
		setdir = TRUE;
		name = name.Right(name.GetLength() - pos - 1);
	}
	char *dataFileList = new char[name.GetLength() + 20];
	strcpy(dataFileList, (LPCTSTR) name);
	strcat(dataFileList, "\n");
	return dataFileList;
}
