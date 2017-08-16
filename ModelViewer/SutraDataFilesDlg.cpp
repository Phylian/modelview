// SutraDataFilesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "SutraDataFilesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSutraDataFilesDlg dialog


CSutraDataFilesDlg::CSutraDataFilesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CSutraDataFilesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CSutraDataFilesDlg)
	m_EleFile = _T("");
	m_NodFile = _T("");
	m_SutraInpFile = _T("");
	m_FileType = -1;
	m_TimeUnits = 0;
	//}}AFX_DATA_INIT
}


void CSutraDataFilesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSutraDataFilesDlg)
	DDX_Text(pDX, IDC_ELE_FILE, m_EleFile);
	DDX_Text(pDX, IDC_NOD_FILE, m_NodFile);
	DDX_Text(pDX, IDC_SUTRA_INP_FILE, m_SutraInpFile);
	DDX_Radio(pDX, IDC_SUTRA_NOD, m_FileType);
	DDX_CBIndex(pDX, IDC_COMBO_TIME_UNITS, m_TimeUnits);
	//}}AFX_DATA_MAP

	m_NodFile.TrimLeft();
	m_NodFile.TrimRight();
	m_EleFile.TrimLeft();
	m_EleFile.TrimRight();
	m_SutraInpFile.TrimLeft();
	m_SutraInpFile.TrimRight();

	pDX->PrepareEditCtrl(IDC_NOD_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		bool filesOK = (m_FileType >= 0);
		if (filesOK)
		{
			if (m_FileType == 0)
			{
				filesOK = (m_NodFile.GetLength() == 0) 
					|| (m_EleFile.GetLength() == 0)
					|| (m_SutraInpFile.GetLength() == 0);
			}
			else
			{
				filesOK = (m_NodFile.GetLength() == 0); 
			}
		}
		if (filesOK)
		{
			AfxMessageBox("Please specify the \"nod\", \"ele\", and \"input\" files or the binary file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	pDX->PrepareEditCtrl(IDC_SUTRA_NOD);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_FileType == -1)
		{
			AfxMessageBox("Please indicate whether you are specifying the \"nod\" or binary file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

	if (m_EleFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_ELE_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_NodFile.ReverseFind('\\');
			int p2 = m_EleFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_NodFile.Left(p1);
				CString path2 = m_EleFile.Left(p2);
				if (path1.CompareNoCase(path2))
				{
					AfxMessageBox("All files must be in the same directory.", MB_ICONEXCLAMATION);
					pDX->Fail();
					return;
				}
			}
		}
	}

	if (m_SutraInpFile.GetLength() > 0)
	{
		pDX->PrepareEditCtrl(IDC_SUTRA_INP_FILE);
		if (pDX->m_bSaveAndValidate)
		{
			int p1 = m_NodFile.ReverseFind('\\');
			int p2 = m_SutraInpFile.ReverseFind('\\');
			if (p1 != -1 || p2 != -1)
			{
				CString path1 = m_NodFile.Left(p1);
				CString path2 = m_SutraInpFile.Left(p2);
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


BEGIN_MESSAGE_MAP(CSutraDataFilesDlg, CDialog)
	//{{AFX_MSG_MAP(CSutraDataFilesDlg)
	ON_BN_CLICKED(IDC_BROWSE_ELE, OnBrowseEle)
	ON_BN_CLICKED(IDC_BROWSE_NOD, OnBrowseNod)
	ON_BN_CLICKED(IDC_BROWSE_SUTRA_INP, OnBrowseSutraInp)
	ON_BN_CLICKED(IDC_SUTRA_NOD, OnSutraNod)
	ON_BN_CLICKED(IDC_SUTRA_BINARY, OnSutraBinary)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSutraDataFilesDlg message handlers

void CSutraDataFilesDlg::OnBrowseEle() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Ele File (*.ele)|*.ele|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Ele File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_ELE_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CSutraDataFilesDlg::OnBrowseNod() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Nod File (*.nod), Binary File (*.bin)|*.nod;*.bin|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Nod or Binary File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	CString p = fileDlg.GetPathName();
	GetDlgItem(IDC_NOD_FILE)->SetWindowText(p);
	CString q = p.Right(4);
	if (p.Right(4) == _T(".nod"))
	{
		((CButton *) GetDlgItem(IDC_SUTRA_NOD))->SetCheck(1);
		((CButton *) GetDlgItem(IDC_SUTRA_BINARY))->SetCheck(0);
		OnSutraNod();
	}
	else if (p.Right(4) == _T(".bin"))
	{
		((CButton *) GetDlgItem(IDC_SUTRA_NOD))->SetCheck(0);
		((CButton *) GetDlgItem(IDC_SUTRA_BINARY))->SetCheck(1);
		OnSutraBinary();
	}
}

void CSutraDataFilesDlg::OnBrowseSutraInp() 
{
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Inp File (*.inp)|*.inp|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "Inp File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_SUTRA_INP_FILE)->SetWindowText(fileDlg.GetPathName());
}

void CSutraDataFilesDlg::OnSutraNod() 
{
	GetDlgItem(IDC_ELE_FILE)->EnableWindow(TRUE);
	GetDlgItem(IDC_BROWSE_ELE)->EnableWindow(TRUE);
	GetDlgItem(IDC_SUTRA_INP_FILE)->EnableWindow(TRUE);
	GetDlgItem(IDC_BROWSE_SUTRA_INP)->EnableWindow(TRUE);
	GetDlgItem(IDC_COMBO_TIME_UNITS)->EnableWindow(TRUE);
}

void CSutraDataFilesDlg::OnSutraBinary() 
{
	GetDlgItem(IDC_ELE_FILE)->SetWindowText("");
	GetDlgItem(IDC_ELE_FILE)->EnableWindow(FALSE);
	GetDlgItem(IDC_BROWSE_ELE)->EnableWindow(FALSE);
	GetDlgItem(IDC_SUTRA_INP_FILE)->SetWindowText("");
	GetDlgItem(IDC_SUTRA_INP_FILE)->EnableWindow(FALSE);
	GetDlgItem(IDC_BROWSE_SUTRA_INP)->EnableWindow(FALSE);
	GetDlgItem(IDC_COMBO_TIME_UNITS)->EnableWindow(FALSE);
}
