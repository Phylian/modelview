// SubgridPage.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "SubgridPage.h"
#include "MvDoc.h"
#include "GridDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSubgridPage property page

IMPLEMENT_DYNCREATE(CSubgridPage, CPropertyPage)

CSubgridPage::CSubgridPage() : CPropertyPage(CSubgridPage::IDD)
{
	//{{AFX_DATA_INIT(CSubgridPage)
	//}}AFX_DATA_INIT
	m_ilow = 0;
	m_ihigh = 0;
	m_jlow = 0;
	m_jhigh = 0;
	m_klow = 0;
	m_khigh = 0;
	m_ExchangeData = FALSE;
	m_ActivateSubgrid = FALSE;
	m_2D = FALSE;
	m_IrregularMesh = FALSE;
	m_LayeredMesh = FALSE;
}

CSubgridPage::~CSubgridPage()
{
}

void CSubgridPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSubgridPage)
	//}}AFX_DATA_MAP
	if (m_ExchangeData)
	{
		DDX_Check(pDX, IDC_ACTIVATE_SUBGRID, m_ActivateSubgrid);
		DDX_Text(pDX, IDC_ILOW, m_ilow);
		DDX_Text(pDX, IDC_IHIGH, m_ihigh);
		DDX_Text(pDX, IDC_JLOW, m_jlow);
		DDX_Text(pDX, IDC_JHIGH, m_jhigh);
		DDX_Text(pDX, IDC_KLOW, m_klow);
		DDX_Text(pDX, IDC_KHIGH, m_khigh);

		pDX->PrepareEditCtrl(IDC_IHIGH);
		if (pDX->m_bSaveAndValidate)
		{
			if (m_ihigh < m_ilow)
			{
				AfxMessageBox("Please insure that \"Max\" value is greater than or equal to \"Min\" value.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_JHIGH);
		if (pDX->m_bSaveAndValidate)
		{
			if (m_jhigh < m_jlow)
			{
				AfxMessageBox("Please insure that \"Max\" value is greater than or equal to \"Min\" value.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_KHIGH);
		if (pDX->m_bSaveAndValidate)
		{
			if (m_khigh < m_klow)
			{
				AfxMessageBox("Please insure that \"Max\" value is greater than or equal to \"Min\" value.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
	}
}


BEGIN_MESSAGE_MAP(CSubgridPage, CPropertyPage)
	//{{AFX_MSG_MAP(CSubgridPage)
	ON_BN_CLICKED(IDC_ACTIVATE_SUBGRID, OnActivateSubgrid)
	ON_NOTIFY(UDN_DELTAPOS, IDC_ILOW_SPIN, OnDeltaposIlowSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_IHIGH_SPIN, OnDeltaposIhighSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_JLOW_SPIN, OnDeltaposJlowSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_JHIGH_SPIN, OnDeltaposJhighSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_KLOW_SPIN, OnDeltaposKlowSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_KHIGH_SPIN, OnDeltaposKhighSpin)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSubgridPage message handlers

BOOL CSubgridPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	Reinitialize();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


void CSubgridPage::Reinitialize()
{
	m_2D = FALSE;
	m_IrregularMesh = FALSE;
	m_LayeredMesh = FALSE;
	((CButton *) GetDlgItem(IDC_ACTIVATE_SUBGRID))->SetCheck(FALSE);
	GetDlgItem(IDC_ILOW)->SetWindowText("");
	GetDlgItem(IDC_IHIGH)->SetWindowText("");
	GetDlgItem(IDC_JLOW)->SetWindowText("");
	GetDlgItem(IDC_JHIGH)->SetWindowText("");
	GetDlgItem(IDC_KLOW)->SetWindowText("");
	GetDlgItem(IDC_KHIGH)->SetWindowText("");
	Activate(FALSE);
}

void CSubgridPage::Activate(BOOL b)
{
	GetDlgItem(IDC_ACTIVATE_SUBGRID)->EnableWindow(b);
	BOOL bb = ((CButton *) GetDlgItem(IDC_ACTIVATE_SUBGRID))->GetCheck();
	GetDlgItem(IDC_ILOW)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_IHIGH)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_JLOW)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_JHIGH)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_KLOW)->EnableWindow(b && bb && !m_2D);
	GetDlgItem(IDC_KHIGH)->EnableWindow(b && bb && !m_2D);
	GetDlgItem(IDC_ILOW_SPIN)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_IHIGH_SPIN)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_JLOW_SPIN)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_JHIGH_SPIN)->EnableWindow(b && bb && !m_IrregularMesh);
	GetDlgItem(IDC_KLOW_SPIN)->EnableWindow(b && bb && !m_2D);
	GetDlgItem(IDC_KHIGH_SPIN)->EnableWindow(b && bb && !m_2D);
	m_IsActive = b;
	if (m_Parent->m_PropertySheet->GetActiveIndex() == 2)
	{
		m_Parent->m_ApplyButton.EnableWindow(b && bb);
	}
}

BOOL CSubgridPage::CustomUpdateData(BOOL b)
{
	m_ExchangeData = TRUE;
	BOOL result = UpdateData(b);
	m_ExchangeData = FALSE;
	return result;
}

void CSubgridPage::Apply()
{
	if (CustomUpdateData(TRUE) && m_ActivateSubgrid)
	{
		if (m_ilow < 1)
		{
			m_ilow = 1;
		}
		if (m_ihigh > m_imax)
		{
			m_ihigh = m_imax;
		}
		if (m_jlow < 1)
		{
			m_jlow = 1;
		}
		if (m_jhigh > m_jmax)
		{
			m_jhigh = m_jmax;
		}
		if (m_klow < 1)
		{
			m_klow = 1;
		}
		if (m_khigh > m_kmax)
		{
			m_khigh = m_kmax;
		}
		CustomUpdateData(FALSE);
		m_pDoc->ApplySubgrid(m_ilow-1, m_ihigh, m_jlow-1, m_jhigh, m_klow-1, m_khigh);
	}
}

void CSubgridPage::OnActivateSubgrid() 
{
	// TODO: Add your control notification handler code here
	BOOL b = ((CButton *) GetDlgItem(IDC_ACTIVATE_SUBGRID))->GetCheck();

	GetDlgItem(IDC_ILOW)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_IHIGH)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JLOW)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JHIGH)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_KLOW)->EnableWindow(b && !m_2D);
	GetDlgItem(IDC_KHIGH)->EnableWindow(b && !m_2D);
	GetDlgItem(IDC_ILOW_SPIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_IHIGH_SPIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JLOW_SPIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JHIGH_SPIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_KLOW_SPIN)->EnableWindow(b && !m_2D);
	GetDlgItem(IDC_KHIGH_SPIN)->EnableWindow(b && !m_2D);
	m_Parent->m_ApplyButton.EnableWindow(b);

	if (b)
	{
		Apply();
	}
	else
	{
		m_pDoc->SubgridOff();
	}
}

void CSubgridPage::OnDeltaposIlowSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_ilow = __min(m_imax, m_ilow + 1);
			if (m_ilow > m_ihigh)
			{
				m_ihigh = m_ilow;
			}
		}
		else
		{
			m_ilow = __max(1, m_ilow - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CSubgridPage::OnDeltaposIhighSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_ihigh = __min(m_imax, m_ihigh + 1);
		}
		else
		{
			m_ihigh = __max(1, m_ihigh - 1);
			if (m_ihigh < m_ilow)
			{
				m_ilow = m_ihigh;
			}
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CSubgridPage::OnDeltaposJlowSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_jlow = __min(m_jmax, m_jlow + 1);
			if (m_jlow > m_jhigh)
			{
				m_jhigh = m_jlow;
			}
		}
		else
		{
			m_jlow = __max(1, m_jlow - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CSubgridPage::OnDeltaposJhighSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_jhigh = __min(m_jmax, m_jhigh + 1);
		}
		else
		{
			m_jhigh = __max(1, m_jhigh - 1);
			if (m_jhigh < m_jlow)
			{
				m_jlow = m_jhigh;
			}
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CSubgridPage::OnDeltaposKlowSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_klow = __min(m_kmax, m_klow + 1);
			if (m_klow > m_khigh)
			{
				m_khigh = m_klow;
			}
		}
		else
		{
			m_klow = __max(1, m_klow - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CSubgridPage::OnDeltaposKhighSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_khigh = __min(m_kmax, m_khigh + 1);
		}
		else
		{
			m_khigh = __max(1, m_khigh - 1);
			if (m_khigh < m_klow)
			{
				m_klow = m_khigh;
			}
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

BOOL CSubgridPage::OnSetActive() 
{
	BOOL b = ((CButton *) GetDlgItem(IDC_ACTIVATE_SUBGRID))->GetCheck();
	m_Parent->m_ApplyButton.EnableWindow(m_IsActive && b);
	return CPropertyPage::OnSetActive();
}
