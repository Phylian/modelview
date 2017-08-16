// PhastDataFilesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "PhastDataFilesDlg.h"

#include <commdlg.h>
#include <cderr.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPhastDataFilesDlg dialog


CPhastDataFilesDlg::CPhastDataFilesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CPhastDataFilesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPhastDataFilesDlg)
	m_strHDFDataFile = _T("");
	//}}AFX_DATA_INIT
}


void CPhastDataFilesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPhastDataFilesDlg)
	DDX_Text(pDX, IDC_HDF_FILE, m_strHDFDataFile);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPhastDataFilesDlg, CDialog)
	//{{AFX_MSG_MAP(CPhastDataFilesDlg)
	ON_BN_CLICKED(IDC_BROWSE_HDF, OnBrowseHDF)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPhastDataFilesDlg message handlers

void CPhastDataFilesDlg::OnBrowseHDF() 
{
	CString initDir(_T(""));
	CString fileName(_T(""));

	UpdateData(TRUE);
	CString testPath(m_strHDFDataFile);
	while (testPath.GetLength() > 0)
	{
		if (testPath[testPath.GetLength() - 1] == _T('\\'))
		{
			// remove trailing backslash
			testPath = testPath.Left(testPath.GetLength() - 1);
		}

		CFileStatus status;
		if (CFile::GetStatus(testPath, status))
		{
			// file exists
			if (status.m_attribute & CFile::directory)
			{
				initDir = status.m_szFullName;
			}
			else
			{
				fileName = status.m_szFullName;
			}
			break;
		}
		else
		{
			TCHAR path_buffer[_MAX_PATH];
			TCHAR drive[_MAX_DRIVE];
			TCHAR dir[_MAX_DIR];
			_tsplitpath(testPath, drive, dir, NULL, NULL);
			_tmakepath(path_buffer, drive, dir, NULL, NULL);
			testPath = path_buffer;
		}
	}

	ASSERT(initDir.IsEmpty() || fileName.IsEmpty()); // one of both should be empty

	// Show file Dialog box
	CFileDialog dlg(
		TRUE,					// bOpenFileDialog
		NULL,					// lpszDefExt
		fileName,
		OFN_HIDEREADONLY | OFN_FILEMUSTEXIST, 
		_T("HDF5 Files (*.h5)|*.h5|All Files (*.*)|*.*||")
		);
	dlg.m_ofn.lpstrTitle = _T("Select a HDF5 file");
	dlg.m_ofn.lpstrInitialDir = initDir;
	
	if (dlg.DoModal() == IDOK)
	{
		m_strHDFDataFile = dlg.GetPathName();
		UpdateData(FALSE);
	}
	else
	{
		DWORD e = CommDlgExtendedError();
		if (e != 0)
		{
			::MessageBeep(MB_ICONEXCLAMATION);
		}
		switch (e)
		{
		case 0:
			TRACE("User cancelled dialog\n");
			break;
		case CDERR_DIALOGFAILURE:
			TRACE("CDERR_DIALOGFAILURE\n");
			break;
		case CDERR_FINDRESFAILURE:
			TRACE("CDERR_FINDRESFAILURE\n");
			break;
		case CDERR_INITIALIZATION:
			TRACE("CDERR_INITIALIZATION\n");
			break;
		case CDERR_LOADRESFAILURE:
			TRACE("CDERR_LOADRESFAILURE\n");
			break;
		case CDERR_LOADSTRFAILURE:
			TRACE("CDERR_LOADSTRFAILURE\n");
			break;
		case CDERR_LOCKRESFAILURE:
			TRACE("CDERR_LOCKRESFAILURE\n");
			break;
		case CDERR_MEMALLOCFAILURE:
			TRACE("CDERR_MEMALLOCFAILURE\n");
			break;
		case CDERR_MEMLOCKFAILURE:
			TRACE("CDERR_MEMLOCKFAILURE\n");
			break;
		case CDERR_NOHINSTANCE:
			TRACE("CDERR_NOHINSTANCE\n");
			break;
		case CDERR_NOHOOK:
			TRACE("CDERR_NOHOOK\n");
			break;
		case CDERR_NOTEMPLATE:
			TRACE("CDERR_NOTEMPLATE\n");
			break;
		case CDERR_REGISTERMSGFAIL:
			TRACE("CDERR_REGISTERMSGFAIL\n");
			break;
		case CDERR_STRUCTSIZE:
			TRACE("CDERR_STRUCTSIZE\n");
			break;
		case FNERR_BUFFERTOOSMALL:
			TRACE("FNERR_BUFFERTOOSMALL\n");
			break;
		case FNERR_INVALIDFILENAME:
			TRACE("FNERR_INVALIDFILENAME\n");
			break;	
		case FNERR_SUBCLASSFAILURE:
			TRACE("FNERR_SUBCLASSFAILURE\n");
			break;
		default:
			TRACE("Unknown CommDlgExtendedError\n");
			break;
		}
	}
}

void CPhastDataFilesDlg::DDV_ValidFile(CDataExchange* pDX, LPCTSTR szFileName)
{
	if (!pDX->m_bSaveAndValidate)
	{
		return;
	}

	CString caption = _T("Phast Data Files");
	CString prompt;
	if (szFileName == NULL || strlen(szFileName) == 0)
	{
		prompt = _T("Empty file name");
		MessageBox(prompt, caption, MB_ICONEXCLAMATION);

		// exception prep
		prompt.Empty(); 
		caption.Empty();

		// throw
		pDX->Fail();
	}

	try
	{
		CFile file(szFileName, CFile::modeRead | CFile::shareDenyNone);
		file.Close();
	}
	catch (CFileException* pExc)
	{
		TCHAR szError[1024];
		pExc->GetErrorMessage(szError, 1024);
		prompt = _T("Couldn't open source file.\n");
		prompt += szError;

		MessageBox(prompt, caption, MB_ICONEXCLAMATION);

		// exception prep
		pExc->Delete();
		prompt.Empty(); 
		caption.Empty();

		// throw
		pDX->Fail();
	}

}
