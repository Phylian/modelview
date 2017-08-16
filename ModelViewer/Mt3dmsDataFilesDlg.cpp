// Mt3dmsDataFilesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "Mt3dmsDataFilesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMt3dmsDataFilesDlg dialog


CMt3dmsDataFilesDlg::CMt3dmsDataFilesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CMt3dmsDataFilesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CMt3dmsDataFilesDlg)
	m_CnfFile = _T("");
	m_UcnFile = _T("");
	m_LinkFile = _T("");
	m_PathFile = _T("");
	m_DataType = -1;
	m_PathlineBackwards = FALSE;
	//}}AFX_DATA_INIT
}


void CMt3dmsDataFilesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMt3dmsDataFilesDlg)
	DDX_Control(pDX, IDC_DATA_TYPE, m_DataTypeList);
	DDX_Text(pDX, IDC_CNF_FILE, m_CnfFile);
	DDX_Text(pDX, IDC_UCN_FILE, m_UcnFile);
	DDX_Text(pDX, IDC_MTLINK_FILE, m_LinkFile);
	DDX_Text(pDX, IDC_PATH_FILE, m_PathFile);
	DDX_CBIndex(pDX, IDC_DATA_TYPE, m_DataType);
	DDX_Check(pDX, IDC_PATHLINE_BACKWARDS, m_PathlineBackwards);
	//}}AFX_DATA_MAP

	m_CnfFile.TrimLeft();
	m_CnfFile.TrimRight();
	m_UcnFile.TrimLeft();
	m_UcnFile.TrimRight();
	m_LinkFile.TrimLeft();
	m_LinkFile.TrimRight();
	m_PathFile.TrimLeft();
	m_PathFile.TrimRight();

	pDX->PrepareEditCtrl(IDC_CNF_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_CnfFile.GetLength() == 0)
		{
			AfxMessageBox("Please specify the \"cnf\" file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_UCN_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_UcnFile.GetLength() == 0)
		{
			AfxMessageBox("Please specify the \"ucn\" file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_DATA_TYPE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_DataType == -1)
		{
			AfxMessageBox("Please specify the type of unformatted data", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_UCN_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		int p1 = m_CnfFile.ReverseFind('\\');
		int p2 = m_UcnFile.ReverseFind('\\');
		if (p1 != -1 || p2 != -1)
		{
			CString path1 = m_CnfFile.Left(p1);
			CString path2 = m_UcnFile.Left(p2);
			if (path1.CompareNoCase(path2))
			{
				AfxMessageBox("All files must be in the same directory.", MB_ICONEXCLAMATION);
				pDX->Fail();
				return;
			}
		}
	}

	if (m_LinkFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_MTLINK_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_CnfFile.ReverseFind('\\');
			int p2 = m_LinkFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_CnfFile.Left(p1);
				CString path2 = m_LinkFile.Left(p2);
				if (path1.CompareNoCase(path2))
				{
					AfxMessageBox("All files must be in the same directory.", MB_ICONEXCLAMATION);
					pDX->Fail();
					return;
				}
			}
		}
	}

	if (m_PathFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_PATH_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_CnfFile.ReverseFind('\\');
			int p2 = m_PathFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_CnfFile.Left(p1);
				CString path2 = m_PathFile.Left(p2);
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


BEGIN_MESSAGE_MAP(CMt3dmsDataFilesDlg, CDialog)
	//{{AFX_MSG_MAP(CMt3dmsDataFilesDlg)
	ON_BN_CLICKED(IDC_BROWSE_CNF, OnBrowseCnf)
	ON_BN_CLICKED(IDC_BROWSE_UCN, OnBrowseUcn)
	ON_BN_CLICKED(IDC_BROWSE_MTLINK, OnBrowseMtlink)
	ON_BN_CLICKED(IDC_BROWSE_PATH, OnBrowsePath)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMt3dmsDataFilesDlg message handlers

BOOL CMt3dmsDataFilesDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// TODO: Add extra initialization here
	m_DataTypeList.ResetContent();
	m_DataTypeList.AddString("Binary (unstructured, non-formatted)");
	m_DataTypeList.AddString("Unformatted - Visual Fortran & LF95");
	m_DataTypeList.AddString("Unformatted - Lahey Fortran 90 (LF90)");
	m_DataTypeList.AddString("Big Endian - Unix");
	m_DataTypeList.SetCurSel(0);
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CMt3dmsDataFilesDlg::OnBrowseCnf() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Cnf File (*.cnf)|*.cnf|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Cnf File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_CNF_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CMt3dmsDataFilesDlg::OnBrowseUcn() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Concentration File (*.ucn)|*.ucn|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Conc, Head or Drawdown File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_UCN_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CMt3dmsDataFilesDlg::OnBrowseMtlink() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Flow-Transport Link File (*.ftl)|*.ftl|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Flow-Transport Link File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_MTLINK_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CMt3dmsDataFilesDlg::OnBrowsePath() 
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
