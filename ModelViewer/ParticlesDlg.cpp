// ParticlesDlg.cpp : implementation file
//

#include <math.h>
#include "stdafx.h"
#include "modelviewer.h"
#include "ParticlesDlg.h"
#include "MvDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CParticlesDlg dialog


CParticlesDlg::CParticlesDlg(CWnd* pParent, CMvDoc *pDoc)
	: CDialog(CParticlesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CParticlesDlg)
	m_MaxConc = 0.0f;
	m_MinConc = 0.0f;
	m_TrueMaxConc = _T("");
	m_TrueMinConc = _T("");
	m_DeltaX = 0.0f;
	m_XMax = 1.0f;
	m_XMin = 0.0f;
	m_DeltaY = 0.0f;
	m_YMax = 1.0f;
	m_YMin = 0.0f;
	m_DeltaZ = 0.0f;
	m_ZMin = 0.0f;
	m_ZMax = 1.0f;
	//}}AFX_DATA_INIT

	m_DeltaX = 0.1f;
	m_DeltaY = 0.1f;
	m_DeltaZ = 0.1f;

	ASSERT(pParent != NULL);

	m_pParent = pParent;
	m_pDoc = pDoc;
	m_nID = CParticlesDlg::IDD;
}


void CParticlesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CParticlesDlg)
	DDX_Text(pDX, IDC_CONC_MAX, m_MaxConc);
	DDX_Text(pDX, IDC_CONC_MIN, m_MinConc);
	DDX_Text(pDX, IDC_MAX_CONC_VALUE, m_TrueMaxConc);
	DDX_Text(pDX, IDC_MIN_CONC_VALUE, m_TrueMinConc);
	DDX_Text(pDX, IDC_XDELTA, m_DeltaX);
	DDV_MinMaxFloat(pDX, m_DeltaX, 0.f, 1.f);
	DDX_Text(pDX, IDC_XMAX, m_XMax);
	DDV_MinMaxFloat(pDX, m_XMax, 0.f, 1.f);
	DDX_Text(pDX, IDC_XMIN, m_XMin);
	DDV_MinMaxFloat(pDX, m_XMin, 0.f, 1.f);
	DDX_Text(pDX, IDC_YDELTA, m_DeltaY);
	DDV_MinMaxFloat(pDX, m_DeltaY, 0.f, 1.f);
	DDX_Text(pDX, IDC_YMAX, m_YMax);
	DDV_MinMaxFloat(pDX, m_YMax, 0.f, 1.f);
	DDX_Text(pDX, IDC_YMIN, m_YMin);
	DDV_MinMaxFloat(pDX, m_YMin, 0.f, 1.f);
	DDX_Text(pDX, IDC_ZDELTA, m_DeltaZ);
	DDV_MinMaxFloat(pDX, m_DeltaZ, 0.f, 1.f);
	DDX_Text(pDX, IDC_ZMIN, m_ZMin);
	DDV_MinMaxFloat(pDX, m_ZMin, 0.f, 1.f);
	DDX_Text(pDX, IDC_ZMAX, m_ZMax);
	DDV_MinMaxFloat(pDX, m_ZMax, 0.f, 1.f);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CParticlesDlg, CDialog)
	//{{AFX_MSG_MAP(CParticlesDlg)
	ON_NOTIFY(UDN_DELTAPOS, IDC_PARTICLE_SIZE_SPIN, OnDeltaposParticleSizeSpin)
	ON_BN_CLICKED(IDC_APPLY, OnApply)
	ON_NOTIFY(UDN_DELTAPOS, IDC_XMIN_SPIN, OnDeltaposXminSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_XMAX_SPIN, OnDeltaposXmaxSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_YMIN_SPIN, OnDeltaposYminSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_YMAX_SPIN, OnDeltaposYmaxSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_ZMIN_SPIN, OnDeltaposZminSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_ZMAX_SPIN, OnDeltaposZmaxSpin)
	ON_BN_CLICKED(IDC_FULL, OnFull)
	ON_BN_CLICKED(IDC_DONE, OnDone)
	ON_WM_HELPINFO()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CParticlesDlg message handlers

void CParticlesDlg::Reinitialize()
{
	m_MaxConc = m_pDoc->GetMaxDisplayedParticleConcentration();
	m_MinConc = m_pDoc->GetMinDisplayedParticleConcentration();

	m_ModelMinConc = m_pDoc->GetMinParticleConcentration();
	m_TrueMinConc.Format("%g", m_ModelMinConc);

	m_ModelMaxConc = m_pDoc->GetMaxParticleConcentration();
	m_TrueMaxConc.Format("%g", m_ModelMaxConc);

	m_XMin = m_pDoc->GetParticleClippingXMin();
	m_XMax = m_pDoc->GetParticleClippingXMax();
	m_YMin = m_pDoc->GetParticleClippingYMin();
	m_YMax = m_pDoc->GetParticleClippingYMax();
	m_ZMin = m_pDoc->GetParticleClippingZMin();
	m_ZMax = m_pDoc->GetParticleClippingZMax();

	// Cannot use UpdateData(FALSE) when animating, so update manually
	GetDlgItem(IDC_MIN_CONC_VALUE)->SetWindowText(m_TrueMinConc);
	GetDlgItem(IDC_MAX_CONC_VALUE)->SetWindowText(m_TrueMaxConc);
	CString text;
	text.Format("%g", m_MinConc);
	GetDlgItem(IDC_CONC_MIN)->SetWindowText(text);
	text.Format("%g", m_MaxConc);
	GetDlgItem(IDC_CONC_MAX)->SetWindowText(text);
	text.Format("%g", m_XMin);
	GetDlgItem(IDC_XMIN)->SetWindowText(text);
	text.Format("%g", m_XMax);
	GetDlgItem(IDC_XMAX)->SetWindowText(text);
	text.Format("%g", m_YMin);
	GetDlgItem(IDC_YMIN)->SetWindowText(text);
	text.Format("%g", m_YMax);
	GetDlgItem(IDC_YMAX)->SetWindowText(text);
	text.Format("%g", m_ZMin);
	GetDlgItem(IDC_ZMIN)->SetWindowText(text);
	text.Format("%g", m_ZMax);
	GetDlgItem(IDC_ZMAX)->SetWindowText(text);
}

BOOL CParticlesDlg::Create()
{
	return CDialog::Create(m_nID, m_pParent);
}

void CParticlesDlg::ApplyConcentrationFilter() 
{
	UpdateData(1);
	m_pDoc->FilterParticles(m_MinConc, m_MaxConc);

}

void CParticlesDlg::ApplyCropFilter() 
{
	UpdateData(TRUE);
	m_pDoc->CropParticles(m_XMin, m_XMax, m_YMin, m_YMax, m_ZMin, m_ZMax);

}

void CParticlesDlg::OnDeltaposParticleSizeSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (pNMUpDown->iDelta < 0)
	{
		m_pDoc->EnlargeParticleGlyphs();
	}
	else
	{
		m_pDoc->ShrinkParticleGlyphs();
	}
	
	*pResult = 0;
}

void CParticlesDlg::OnApply() 
{
	ApplyConcentrationFilter();
	ApplyCropFilter();
	
}

void CParticlesDlg::OnDeltaposXminSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (pNMUpDown->iDelta < 0)
	{
		m_XMin = __min(m_XMax, m_XMin + m_DeltaX);
	}
	else
	{
		m_XMin = __max(0, m_XMin - m_DeltaX);
	}
	
	int count = (int) (m_XMin/m_DeltaX + 0.5);
	m_XMin = count*m_DeltaX;

	UpdateData(0);
	
	
	*pResult = 0;
}

void CParticlesDlg::OnDeltaposXmaxSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;

	if (pNMUpDown->iDelta < 0)
	{
		m_XMax = __min(1, m_XMax + m_DeltaX);
	}
	else
	{
		m_XMax = __max(m_XMin, m_XMax - m_DeltaX);
	}

	int count = (int) (m_XMax/m_DeltaX + 0.5);
	m_XMax = count*m_DeltaX;
	
	UpdateData(0);
	
	*pResult = 0;
}

void CParticlesDlg::OnDeltaposYminSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;

	if (pNMUpDown->iDelta < 0)
	{
		m_YMin = __min(m_YMax, m_YMin + m_DeltaY);
	}
	else
	{
		m_YMin = __max(0, m_YMin - m_DeltaY);
	}
	int count = (int) (m_YMin/m_DeltaY + 0.5);
	m_YMin = count*m_DeltaY;
	UpdateData(0);
	
	*pResult = 0;
}

void CParticlesDlg::OnDeltaposYmaxSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;

	if (pNMUpDown->iDelta < 0)
	{
		m_YMax = __min(1, m_YMax + m_DeltaY);
	}
	else
	{
		m_YMax = __max(m_YMin, m_YMax - m_DeltaY);
	}
	int count = (int) (m_YMax/m_DeltaY + 0.5);
	m_YMax = count*m_DeltaY;
	UpdateData(0);
	
	
	*pResult = 0;
}

void CParticlesDlg::OnDeltaposZminSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;

	if (pNMUpDown->iDelta < 0)
	{
		m_ZMin = __min(m_ZMax, m_ZMin + m_DeltaZ);
	}
	else
	{
		m_ZMin = __max(0, m_ZMin - m_DeltaZ);
	}
	int count = (int) (m_ZMin/m_DeltaZ + 0.5);
	m_ZMin = count*m_DeltaZ;
	UpdateData(0);
	
	*pResult = 0;
}

void CParticlesDlg::OnDeltaposZmaxSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;

	if (pNMUpDown->iDelta < 0)
	{
		m_ZMax = __min(1, m_ZMax + m_DeltaZ);
	}
	else
	{
		m_ZMax = __max(m_ZMin, m_ZMax - m_DeltaZ);
	}
	int count = (int) (m_ZMax/m_DeltaZ + 0.5);
	m_ZMax = count*m_DeltaZ;
	UpdateData(0);
	
	*pResult = 0;
}

void CParticlesDlg::OnFull() 
{
	m_MinConc = m_ModelMinConc;
	m_MaxConc = m_ModelMaxConc;

	UpdateData(FALSE);
	m_pDoc->FilterParticles(m_MinConc, m_MaxConc);
	
}

void CParticlesDlg::PostNcDestroy() 
{
	delete this;
}

BOOL CParticlesDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	WINDOWPLACEMENT wndpl;
	GetWindowPlacement(&wndpl);
	int screenWidth = ::GetSystemMetrics(SM_CXSCREEN);
	int screenHeight = ::GetSystemMetrics(SM_CYSCREEN);
	int dlgWidth = wndpl.rcNormalPosition.right - wndpl.rcNormalPosition.left;
	int dlgHeight = wndpl.rcNormalPosition.bottom - wndpl.rcNormalPosition.top;
	wndpl.rcNormalPosition.right = screenWidth - 200;
	wndpl.rcNormalPosition.left = wndpl.rcNormalPosition.right - dlgWidth;
	wndpl.rcNormalPosition.top = screenHeight/4 - 10;
	wndpl.rcNormalPosition.bottom = wndpl.rcNormalPosition.top + dlgHeight;
	wndpl.showCmd = SW_HIDE;
	SetWindowPlacement(&wndpl);

	Reinitialize();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CParticlesDlg::OnDone() 
{
	ShowWindow(SW_HIDE);
}

BOOL CParticlesDlg::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	// Inactivates help when F1 is pressed	
	//return CDialog::OnHelpInfo(pHelpInfo);
	return 0;
}


void CParticlesDlg::Activate(BOOL b)
{
	GetDlgItem(IDC_CONC_MAX)->EnableWindow(b);
	GetDlgItem(IDC_CONC_MIN)->EnableWindow(b);
	GetDlgItem(IDC_MAX_CONC_VALUE)->EnableWindow(b);
	GetDlgItem(IDC_MIN_CONC_VALUE)->EnableWindow(b);
	GetDlgItem(IDC_XDELTA)->EnableWindow(b);
	GetDlgItem(IDC_XMAX)->EnableWindow(b);
	GetDlgItem(IDC_XMIN)->EnableWindow(b);
	GetDlgItem(IDC_YDELTA)->EnableWindow(b);
	GetDlgItem(IDC_YMAX)->EnableWindow(b);
	GetDlgItem(IDC_YMIN)->EnableWindow(b);
	GetDlgItem(IDC_ZDELTA)->EnableWindow(b);
	GetDlgItem(IDC_ZMIN)->EnableWindow(b);
	GetDlgItem(IDC_ZMAX)->EnableWindow(b);
	GetDlgItem(IDC_APPLY)->EnableWindow(b);
	GetDlgItem(IDC_FULL)->EnableWindow(b);
}
