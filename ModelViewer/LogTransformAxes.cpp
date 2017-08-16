// LogTransformAxes.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "LogTransformAxes.h"
#include "MvDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLogTransformAxes property page

IMPLEMENT_DYNCREATE(CLogTransformAxes, CPropertyPage)

CLogTransformAxes::CLogTransformAxes() : CPropertyPage(CLogTransformAxes::IDD)
{
	//{{AFX_DATA_INIT(CLogTransformAxes)
	//}}AFX_DATA_INIT
}

CLogTransformAxes::~CLogTransformAxes()
{
}

void CLogTransformAxes::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLogTransformAxes)
	DDX_Control(pDX, IDC_LOG_TRANSFORM_Z, m_LogTransformZCheckBox);
	DDX_Control(pDX, IDC_LOG_TRANSFORM_Y, m_LogTransformYCheckBox);
	DDX_Control(pDX, IDC_LOG_TRANSFORM_X, m_LogTransformXCheckBox);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLogTransformAxes, CPropertyPage)
	//{{AFX_MSG_MAP(CLogTransformAxes)
	ON_BN_CLICKED(IDC_LOG_TRANSFORM_X, OnLogTransformX)
	ON_BN_CLICKED(IDC_LOG_TRANSFORM_Y, OnLogTransformY)
	ON_BN_CLICKED(IDC_LOG_TRANSFORM_Z, OnLogTransformZ)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogTransformAxes message handlers
void CLogTransformAxes::Reinitialize()
{
	GetDlgItem(IDC_LOG_TRANSFORM_X)->EnableWindow(FALSE);
	GetDlgItem(IDC_LOG_TRANSFORM_Y)->EnableWindow(FALSE);
	GetDlgItem(IDC_LOG_TRANSFORM_Z)->EnableWindow(FALSE);
	m_LogTransformXCheckBox.SetCheck(FALSE);
	m_LogTransformYCheckBox.SetCheck(FALSE);
	m_LogTransformZCheckBox.SetCheck(FALSE);
}

void CLogTransformAxes::Update()
{
	GetDlgItem(IDC_LOG_TRANSFORM_X)->EnableWindow(m_pDoc->GetCanLogTransformXAxis());
	GetDlgItem(IDC_LOG_TRANSFORM_Y)->EnableWindow(m_pDoc->GetCanLogTransformYAxis());
	GetDlgItem(IDC_LOG_TRANSFORM_Z)->EnableWindow(m_pDoc->GetCanLogTransformZAxis());
	m_LogTransformXCheckBox.SetCheck(m_pDoc->GetLogTransformXAxis());
	m_LogTransformYCheckBox.SetCheck(m_pDoc->GetLogTransformYAxis());
	m_LogTransformZCheckBox.SetCheck(m_pDoc->GetLogTransformZAxis());

	GetDlgItem(IDC_LOG_TRANSFORM_X)->SetWindowText(m_pDoc->XAxisLabel());
	GetDlgItem(IDC_LOG_TRANSFORM_Y)->SetWindowText(m_pDoc->YAxisLabel());
	GetDlgItem(IDC_LOG_TRANSFORM_Z)->SetWindowText(m_pDoc->ZAxisLabel());
}

void CLogTransformAxes::OnLogTransformX() 
{
	BOOL bx = ((CButton *) GetDlgItem(IDC_LOG_TRANSFORM_X))->GetCheck();
	m_pDoc->SetLogTransformXAxis(bx);
}

void CLogTransformAxes::OnLogTransformY() 
{
	BOOL by = ((CButton *) GetDlgItem(IDC_LOG_TRANSFORM_Y))->GetCheck();
	m_pDoc->SetLogTransformYAxis(by);
}

void CLogTransformAxes::OnLogTransformZ() 
{
	BOOL bz = ((CButton *) GetDlgItem(IDC_LOG_TRANSFORM_Z))->GetCheck();
	m_pDoc->SetLogTransformZAxis(bz);
}
