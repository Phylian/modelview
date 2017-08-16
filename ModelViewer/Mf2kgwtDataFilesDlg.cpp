// Mf2kgwtDataFilesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "Mf2kgwtDataFilesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMf2kgwtDataFilesDlg dialog


CMf2kgwtDataFilesDlg::CMf2kgwtDataFilesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CMf2kgwtDataFilesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CMf2kgwtDataFilesDlg)
	m_NameFile = _T("");
	m_PathFile = _T("");
	m_DataType = -1;
	m_PathlineBackwards = FALSE;
	//}}AFX_DATA_INIT
}


void CMf2kgwtDataFilesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMf2kgwtDataFilesDlg)
	DDX_Control(pDX, IDC_DATA_TYPE, m_DataTypeList);
	DDX_Text(pDX, IDC_NAME_FILE, m_NameFile);
	DDX_Text(pDX, IDC_PATH_FILE, m_PathFile);
	DDX_CBIndex(pDX, IDC_DATA_TYPE, m_DataType);
	DDX_Check(pDX, IDC_PATHLINE_BACKWARDS, m_PathlineBackwards);
	//}}AFX_DATA_MAP

	m_NameFile.TrimLeft();
	m_NameFile.TrimRight();
	m_PathFile.TrimLeft();
	m_PathFile.TrimRight();

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
		pDX->PrepareEditCtrl(IDC_PATH_FILE);
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
}


BEGIN_MESSAGE_MAP(CMf2kgwtDataFilesDlg, CDialog)
	//{{AFX_MSG_MAP(CMf2kgwtDataFilesDlg)
	ON_BN_CLICKED(IDC_BROWSE_NAME, OnBrowseName)
	ON_BN_CLICKED(IDC_BROWSE_PATH2, OnBrowsePath2)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMf2kgwtDataFilesDlg message handlers

BOOL CMf2kgwtDataFilesDlg::OnInitDialog() 
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

void CMf2kgwtDataFilesDlg::OnBrowseName() 
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

void CMf2kgwtDataFilesDlg::OnBrowsePath2() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Pathline Files (*.lin;*.path;*.pathline)|*.lin;*.path;*.pathline|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Pathline File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_PATH_FILE)->SetWindowText(fileDlg.GetPathName());
}
