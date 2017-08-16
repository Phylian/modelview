// UcodeDataFileDlg.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "UcodeDataFileDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CUcodeDataFileDlg dialog


CUcodeDataFileDlg::CUcodeDataFileDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CUcodeDataFileDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CUcodeDataFileDlg)
	m_UcodeSosFile = _T("");
	//}}AFX_DATA_INIT
}


void CUcodeDataFileDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CUcodeDataFileDlg)
	DDX_Text(pDX, IDC_UCODE_SOS_FILE, m_UcodeSosFile);
	//}}AFX_DATA_MAP

	m_UcodeSosFile.TrimLeft();
	m_UcodeSosFile.TrimRight();

	pDX->PrepareEditCtrl(IDC_UCODE_SOS_FILE);
	if (pDX->m_bSaveAndValidate)
	{
		if (m_UcodeSosFile.GetLength() == 0)
		{
			AfxMessageBox("Please specify the UCODE *._sos file.", MB_ICONEXCLAMATION);
			pDX->Fail();
			return;
		}
	}

}


BEGIN_MESSAGE_MAP(CUcodeDataFileDlg, CDialog)
	//{{AFX_MSG_MAP(CUcodeDataFileDlg)
	ON_BN_CLICKED(IDC_BROWSE_UCODE, OnBrowseUcode)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CUcodeDataFileDlg message handlers

void CUcodeDataFileDlg::OnBrowseUcode() 
{
	// TODO: Add your control notification handler code here
	CFileDialog fileDlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"UCODE *._sos Files (*._sos)|*._sos|All Files (*.*)|*.*||");
	fileDlg.m_ofn.lpstrTitle = "UCODE _sos File";
	if (fileDlg.DoModal() != IDOK)
	{
		return;
	}
	GetDlgItem(IDC_UCODE_SOS_FILE)->SetWindowText(fileDlg.GetPathName());
	
}
