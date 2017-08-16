// Modflow2000DataFilesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "Modflow2000DataFilesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CModflow2000DataFilesDlg dialog


CModflow2000DataFilesDlg::CModflow2000DataFilesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CModflow2000DataFilesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CModflow2000DataFilesDlg)
	m_NameFile = _T("");
	m_PathFile = _T("");
	m_DataType = -1;
	m_PathlineBackwards = FALSE;
	m_ExternalFile = _T("");
	//}}AFX_DATA_INIT
}


void CModflow2000DataFilesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CModflow2000DataFilesDlg)
	DDX_Control(pDX, IDC_DATA_TYPE, m_DataTypeList);
	DDX_Text(pDX, IDC_NAME_FILE, m_NameFile);
	DDX_Text(pDX, IDC_PATHLINE_FILE, m_PathFile);
	DDX_CBIndex(pDX, IDC_DATA_TYPE, m_DataType);
	DDX_Check(pDX, IDC_PATHLINE_BACKWARDS, m_PathlineBackwards);
	DDX_Text(pDX, IDC_EXTERNAL_FILE, m_ExternalFile);
	//}}AFX_DATA_MAP

	m_NameFile.TrimLeft();
	m_NameFile.TrimRight();
	m_PathFile.TrimLeft();
	m_PathFile.TrimRight();
	m_ExternalFile.TrimLeft();
	m_ExternalFile.TrimRight();

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

	if (m_PathFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_PATHLINE_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_NameFile.ReverseFind('\\');
			int p2 = m_PathFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_NameFile.Left(p1);
				CString path2 = m_PathFile.Left(p2);
				if (path1.CompareNoCase(path2))
				{
					AfxMessageBox("The pathline file must be in the same directory as the name file.", MB_ICONEXCLAMATION);
					pDX->Fail();
					return;
				}
			}
		}
	}

	if (m_ExternalFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_EXTERNAL_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_NameFile.ReverseFind('\\');
			int p2 = m_ExternalFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_NameFile.Left(p1);
				CString path2 = m_ExternalFile.Left(p2);
				if (path1.CompareNoCase(path2))
				{
					AfxMessageBox("The external file must be in the same directory as the name file.", MB_ICONEXCLAMATION);
					pDX->Fail();
					return;
				}
			}
		}
	}
}


BEGIN_MESSAGE_MAP(CModflow2000DataFilesDlg, CDialog)
	//{{AFX_MSG_MAP(CModflow2000DataFilesDlg)
	ON_BN_CLICKED(IDC_BROWSE_NAME, OnBrowseName)
	ON_BN_CLICKED(IDC_BROWSE_PATHLINE, OnBrowsePathline)
	ON_BN_CLICKED(IDC_BROWSE_EXTERNAL, OnBrowseExternal)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CModflow2000DataFilesDlg message handlers

BOOL CModflow2000DataFilesDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	m_DataTypeList.ResetContent();
	m_DataTypeList.AddString("Binary (unstructured, non-formatted)");
	m_DataTypeList.AddString("Unformatted - Visual Fortran & LF95");
	m_DataTypeList.AddString("Unformatted - Lahey Fortran 90 (LF90)");
	m_DataTypeList.AddString("Big Endian - Unix");
	m_DataTypeList.SetCurSel(0);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CModflow2000DataFilesDlg::OnBrowseName() 
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

void CModflow2000DataFilesDlg::OnBrowsePathline() 
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

void CModflow2000DataFilesDlg::OnBrowseExternal() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"External Data Files (*.txt)|*.txt|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "External Data File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_EXTERNAL_FILE)->SetWindowText(fileDlg.GetPathName());
	
}
