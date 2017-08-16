// AxesDlg.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "AxesDlg.h"
#include "MvDoc.h"
#include "LogTransformAxes.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAxesDlg dialog


CAxesDlg::CAxesDlg(CWnd* pParent, CMvDoc *pDoc)
	: CDialog(CAxesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CAxesDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT

	ASSERT(pParent != NULL);

	m_pParent = pParent;
	m_pDoc = pDoc;
	m_nID = CAxesDlg::IDD;

	m_PropertySheet = new CPropertySheet;
	m_LogTransformAxesPage = new CLogTransformAxes;
	m_LogTransformAxesPage->m_pDoc = pDoc;
	m_PropertySheet->AddPage(m_LogTransformAxesPage);


}


CAxesDlg::~CAxesDlg()
{
	delete m_PropertySheet;
	delete m_LogTransformAxesPage;
}


void CAxesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAxesDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CAxesDlg, CDialog)
	//{{AFX_MSG_MAP(CAxesDlg)
	ON_BN_CLICKED(ID_AxesDone, OnAxesDone)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAxesDlg message handlers

void CAxesDlg::OnAxesDone() 
{
	// TODO: Add your control notification handler code here
	ShowWindow(SW_HIDE);
	
}

BOOL CAxesDlg::Create()
{
	if (!CDialog::Create(m_nID, m_pParent))
	{
		return FALSE;
	}
	
	
	m_PropertySheet->Create(this, WS_CHILD | WS_VISIBLE, 0);

	CRect rcSheet;
	GetDlgItem( IDC_PROP_SHEET )->GetWindowRect( &rcSheet );
	ScreenToClient( &rcSheet );
        
	CTabCtrl* tabctrl = m_PropertySheet->GetTabControl();
	tabctrl->MoveWindow(0, 0, rcSheet.Width(), rcSheet.Height());
	m_PropertySheet->SetWindowPos( NULL, rcSheet.left, rcSheet.top, 
									rcSheet.Width(), rcSheet.Height(),
									SWP_NOZORDER | SWP_NOACTIVATE );

	m_PropertySheet->ModifyStyleEx(0, WS_EX_CONTROLPARENT);
	m_PropertySheet->ModifyStyle(0, WS_TABSTOP);
	// Force the controls on the property pages to be created
	//m_PropertySheet->SetActivePage(1);
	m_PropertySheet->SetActivePage(0);
	
	
	
	return TRUE;
}

BOOL CAxesDlg::Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext) 
{
	// TODO: Add your specialized code here and/or call the base class
	
	return CDialog::Create(IDD, pParentWnd);
}

void CAxesDlg::Reinitialize()
{
	m_LogTransformAxesPage->Reinitialize();
}


void CAxesDlg::PostNcDestroy() 
{
	delete this;
}

void CAxesDlg::Update()
{
	m_LogTransformAxesPage->Update();
}
