// VectorControlsPage.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "VectorControlsPage.h"
#include "MvDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVectorControlsPage property page

IMPLEMENT_DYNCREATE(CVectorControlsPage, CPropertyPage)

CVectorControlsPage::CVectorControlsPage() : CPropertyPage(CVectorControlsPage::IDD)
{
	//{{AFX_DATA_INIT(CVectorControlsPage)
	//}}AFX_DATA_INIT
	m_imin = 0;
	m_imax = 0;
	m_irate = 0;
	m_jmin = 0;
	m_jmax = 0;
	m_jrate = 0;
	m_kmin = 0;
	m_kmax = 0;
	m_krate = 0;
	m_ExchangeData = FALSE;
}

CVectorControlsPage::~CVectorControlsPage()
{
}

void CVectorControlsPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVectorControlsPage)
	//}}AFX_DATA_MAP
	if (m_ExchangeData)
	{
		DDX_Text(pDX, IDC_IMIN, m_imin);
		DDX_Text(pDX, IDC_IMAX, m_imax);
		DDX_Text(pDX, IDC_IRATE, m_irate);
		DDX_Text(pDX, IDC_JMIN, m_jmin);
		DDX_Text(pDX, IDC_JMAX, m_jmax);
		DDX_Text(pDX, IDC_JRATE, m_jrate);
		DDX_Text(pDX, IDC_KMIN, m_kmin);
		DDX_Text(pDX, IDC_KMAX, m_kmax);
		DDX_Text(pDX, IDC_KRATE, m_krate);

		pDX->PrepareEditCtrl(IDC_IRATE);
		if (pDX->m_bSaveAndValidate)
		{
			if (m_irate <= 0)
			{
				AfxMessageBox("Please enter a positive integer.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_JRATE);
		if (pDX->m_bSaveAndValidate)
		{
			if (m_jrate <= 0)
			{
				AfxMessageBox("Please enter a positive integer.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_KRATE);
		if (pDX->m_bSaveAndValidate)
		{
			if (m_krate <= 0)
			{
				AfxMessageBox("Please enter a positive integer.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_IMAX);
		if (pDX->m_bSaveAndValidate)
		{
			if (!m_IrregularMesh && (m_imax < m_imin))
			{
				AfxMessageBox("Please insure that \"Max\" index is greater than or equal to \"Min\" index.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_JMAX);
		if (pDX->m_bSaveAndValidate)
		{
			if (!m_IrregularMesh && (m_jmax < m_jmin))
			{
				AfxMessageBox("Please insure that \"Max\" index is greater than or equal to \"Min\" index.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
		pDX->PrepareEditCtrl(IDC_KMAX);
		if (pDX->m_bSaveAndValidate)
		{
			if (!m_IrregularMesh && (m_kmax < m_kmin))
			{
				AfxMessageBox("Please insure that \"Max\" index is greater than or equal to \"Min\" index.", MB_ICONEXCLAMATION);
				pDX->Fail();
			}
		}
	}
}


BEGIN_MESSAGE_MAP(CVectorControlsPage, CPropertyPage)
	//{{AFX_MSG_MAP(CVectorControlsPage)
	ON_NOTIFY(UDN_DELTAPOS, IDC_IMIN_SPIN, OnDeltaposIminSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_IMAX_SPIN, OnDeltaposImaxSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_JMIN_SPIN, OnDeltaposJminSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_JMAX_SPIN, OnDeltaposJmaxSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_KMIN_SPIN, OnDeltaposKminSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_KMAX_SPIN, OnDeltaposKmaxSpin)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVectorControlsPage message handlers

BOOL CVectorControlsPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	Reinitialize();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CVectorControlsPage::Reinitialize()
{
	GetDlgItem(IDC_IMIN)->SetWindowText("");
	GetDlgItem(IDC_IMAX)->SetWindowText("");
	GetDlgItem(IDC_IRATE)->SetWindowText("");
	GetDlgItem(IDC_JMIN)->SetWindowText("");
	GetDlgItem(IDC_JMAX)->SetWindowText("");
	GetDlgItem(IDC_JRATE)->SetWindowText("");
	GetDlgItem(IDC_KMIN)->SetWindowText("");
	GetDlgItem(IDC_KMAX)->SetWindowText("");
	GetDlgItem(IDC_KRATE)->SetWindowText("");
	Activate(FALSE);
}

void CVectorControlsPage::Activate(BOOL b)
{
	GetDlgItem(IDC_IMIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_IMAX)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_IRATE)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JMIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JMAX)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_JRATE)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_KMIN)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_KMAX)->EnableWindow(b && !m_IrregularMesh);
	GetDlgItem(IDC_KRATE)->EnableWindow(b);
}

void CVectorControlsPage::OnDeltaposIminSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_imin = __min(m_imax, m_imin + 1);
		}
		else
		{
			m_imin = __max(m_ilimit0, m_imin - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CVectorControlsPage::OnDeltaposImaxSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_imax = __min(m_ilimit1, m_imax + 1);
		}
		else
		{
			m_imax = __max(m_imin, m_imax - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CVectorControlsPage::OnDeltaposJminSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_jmin = __min(m_jmax, m_jmin + 1);
		}
		else
		{
			m_jmin = __max(m_jlimit0, m_jmin - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CVectorControlsPage::OnDeltaposJmaxSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_jmax = __min(m_jlimit1, m_jmax + 1);
		}
		else
		{
			m_jmax = __max(m_jmin, m_jmax - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CVectorControlsPage::OnDeltaposKminSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_kmin = __min(m_kmax, m_kmin + 1);
		}
		else
		{
			m_kmin = __max(m_klimit0, m_kmin - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CVectorControlsPage::OnDeltaposKmaxSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_kmax = __min(m_klimit1, m_kmax + 1);
		}
		else
		{
			m_kmax = __max(m_kmin, m_kmax - 1);
		}
		CustomUpdateData(FALSE);
		Apply();
	}
	*pResult = 0;
}

void CVectorControlsPage::OnDefault()
{
	m_imin = m_ilimit0;
	m_imax = m_ilimit1;
	m_jmin = m_jlimit0;
	m_jmax = m_jlimit1;
	m_kmin = m_klimit0;
	m_kmax = m_klimit1;
	m_irate = 1;
	m_jrate = 1;
	m_krate = 1;
	CustomUpdateData(FALSE);
	Apply();
}

BOOL CVectorControlsPage::CustomUpdateData(BOOL b)
{
	m_ExchangeData = TRUE;
	BOOL result = UpdateData(b);
	m_ExchangeData = FALSE;
	return result;
}

void CVectorControlsPage::Apply()
{
	if (CustomUpdateData(TRUE))
	{
		if (m_imin > m_ilimit1)
		{
			m_imin = m_ilimit1;
			m_imax = m_ilimit1;
		}
		else if (m_imax < m_ilimit0)
		{
			m_imin = m_ilimit0;
			m_imax = m_ilimit0;
		}
		else
		{
			m_jmin = max(m_jmin, m_jlimit0);
			m_jmax = min(m_jmax, m_jlimit1);
		}
		if (m_jmin > m_jlimit1)
		{
			m_jmin = m_jlimit1;
			m_jmax = m_jlimit1;
		}
		else if (m_jmax < m_jlimit0)
		{
			m_jmin = m_jlimit0;
			m_jmax = m_jlimit0;
		}
		else
		{
			m_jmin = max(m_jmin, m_jlimit0);
			m_jmax = min(m_jmax, m_jlimit1);
		}
		if (m_kmin > m_klimit1)
		{
			m_kmin = m_klimit1;
			m_kmax = m_klimit1;
		}
		else if (m_kmax < m_klimit0)
		{
			m_kmin = m_klimit0;
			m_kmax = m_klimit0;
		}
		else
		{
			m_kmin = max(m_kmin, m_klimit0);
			m_kmax = min(m_kmax, m_klimit1);
		}
		CustomUpdateData(FALSE);

		m_pDoc->SubsampleVectors(m_imin-1, m_imax-1,
			m_jmin-1, m_jmax-1, m_kmin-1, m_kmax-1, m_irate, m_jrate, m_krate);
	}
}

