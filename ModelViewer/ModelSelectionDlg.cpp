// ModelSelectionDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "ModelSelectionDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CModelSelectionDlg dialog


CModelSelectionDlg::CModelSelectionDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CModelSelectionDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CModelSelectionDlg)
	m_RememberSelection = FALSE;
	m_SelectedModel = _T("");
	//}}AFX_DATA_INIT

	m_NumberOfModels = 0;
	m_ModelNames = NULL;
	m_InitialSelection = 0;
}


void CModelSelectionDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CModelSelectionDlg)
	DDX_Control(pDX, IDC_MODEL_LIST, m_ModelList);
	DDX_Check(pDX, IDC_DEFAULT_MODEL, m_RememberSelection);
	DDX_CBString(pDX, IDC_MODEL_LIST, m_SelectedModel);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CModelSelectionDlg, CDialog)
	//{{AFX_MSG_MAP(CModelSelectionDlg)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CModelSelectionDlg message handlers

BOOL CModelSelectionDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	for (int i=0; i<m_NumberOfModels; i++)
	{
		m_ModelList.AddString(m_ModelNames[i]);
	}
	if (m_NumberOfModels > 0)
	{
		m_ModelList.SetCurSel(m_InitialSelection);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

BOOL CModelSelectionDlg::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	// Inactivates help when F1 is pressed	
	//return CDialog::OnHelpInfo(pHelpInfo);
	return 0;
}
