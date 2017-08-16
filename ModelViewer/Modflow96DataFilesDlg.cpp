// Modflow96DataFilesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "Modflow96DataFilesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CModflow96DataFilesDlg dialog


CModflow96DataFilesDlg::CModflow96DataFilesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CModflow96DataFilesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CModflow96DataFilesDlg)
	m_ElevationFile = _T("");
	m_NameFile = _T("");
	m_PathlineFile = _T("");
	m_DataType = -1;
	m_PathlineBackwards = FALSE;
	//}}AFX_DATA_INIT
}


void CModflow96DataFilesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CModflow96DataFilesDlg)
	DDX_Control(pDX, IDC_DATA_TYPE, m_DataTypeList);
	DDX_Text(pDX, IDC_ELEVATION_FILE, m_ElevationFile);
	DDX_Text(pDX, IDC_NAME_FILE, m_NameFile);
	DDX_Text(pDX, IDC_PATHLINE_FILE, m_PathlineFile);
	DDX_CBIndex(pDX, IDC_DATA_TYPE, m_DataType);
	DDX_Check(pDX, IDC_PATHLINE_BACKWARDS, m_PathlineBackwards);
	//}}AFX_DATA_MAP

	m_NameFile.TrimLeft();
	m_NameFile.TrimRight();
	m_ElevationFile.TrimLeft();
	m_ElevationFile.TrimRight();
	m_PathlineFile.TrimLeft();
	m_PathlineFile.TrimRight();

	pDX->PrepareEditCtrl(IDC_NAME_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_NameFile.GetLength() == 0)
		{
			AfxMessageBox("Please specify the \"name\" file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_ELEVATION_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_ElevationFile.GetLength() == 0)
		{
			AfxMessageBox("Please specify the \"elevation\" file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_DATA_TYPE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_DataType == -1)
		{
			AfxMessageBox("Please specify the data type.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_ELEVATION_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		int p1 = m_NameFile.ReverseFind('\\');
		int p2 = m_ElevationFile.ReverseFind('\\');
		if (p1 != -1 || p2 != -1)
		{
			CString path1 = m_NameFile.Left(p1);
			CString path2 = m_ElevationFile.Left(p2);
			if (path1.CompareNoCase(path2))
			{
				AfxMessageBox("All files must be in the same directory.", MB_ICONEXCLAMATION);
				pDX->Fail();
				return;
			}
		}
	}

	if (m_PathlineFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_PATHLINE_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_NameFile.ReverseFind('\\');
			int p2 = m_PathlineFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_NameFile.Left(p1);
				CString path2 = m_PathlineFile.Left(p2);
				if (path1.CompareNoCase(path2))
				{
					AfxMessageBox("All files must be in the same directory.", MB_ICONEXCLAMATION);
					pDX->Fail();
					return;
				}
			}
		}
	}
}


BEGIN_MESSAGE_MAP(CModflow96DataFilesDlg, CDialog)
	//{{AFX_MSG_MAP(CModflow96DataFilesDlg)
	ON_BN_CLICKED(IDC_BROWSE_NAME, OnBrowseName)
	ON_BN_CLICKED(IDC_BROWSE_ELEVATION, OnBrowseElevation)
	ON_BN_CLICKED(IDC_BROWSE_PATHLINE, OnBrowsePathline)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CModflow96DataFilesDlg message handlers

BOOL CModflow96DataFilesDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	m_DataTypeList.ResetContent();
	m_DataTypeList.AddString("Binary (unstructured, non-formatted)");
	m_DataTypeList.AddString("Unformatted - Visual Fortran & LF95");
	m_DataTypeList.AddString("Unformatted - Lahey Fortran 90 (LF90)");
	m_DataTypeList.AddString("Big Endian - Unix");
	m_DataTypeList.SetCurSel(2);
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CModflow96DataFilesDlg::OnBrowseName() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Name Files (*.nam; *.name; *.fil)|*.nam; *.name; *.fil|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Name File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_NAME_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CModflow96DataFilesDlg::OnBrowseElevation() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Elevation File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_ELEVATION_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CModflow96DataFilesDlg::OnBrowsePathline() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Pathline Files (*.lin;*.path;*.pathline)|*.lin;*.path;*.pathline|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Pathline File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_PATHLINE_FILE)->SetWindowText(fileDlg.GetPathName());
}
