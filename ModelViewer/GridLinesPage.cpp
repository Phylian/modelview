// GridLinesPage.cpp : implementation file
//

#include "stdafx.h"
#include "ModelViewer.h"
#include "GridLinesPage.h"
#include "MvDoc.h"
#include "GridDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGridLinesPage property page

IMPLEMENT_DYNCREATE(CGridLinesPage, CPropertyPage)

CGridLinesPage::CGridLinesPage() : CPropertyPage(CGridLinesPage::IDD)
{
	//{{AFX_DATA_INIT(CGridLinesPage)
	//}}AFX_DATA_INIT
	m_ExchangeData = FALSE;
	m_2D = FALSE;
	m_IrregularMesh = FALSE;
	m_LayeredMesh = FALSE;
}

CGridLinesPage::~CGridLinesPage()
{
}

void CGridLinesPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGridLinesPage)
	DDX_Control(pDX, IDC_ACTIVATE_OUTLINE, m_ActivateOutlineCheckBox);
	DDX_Control(pDX, IDC_ACTIVATE_Z, m_ActivateZCheckBox);
	DDX_Control(pDX, IDC_ACTIVATE_Y, m_ActivateYCheckBox);
	DDX_Control(pDX, IDC_ACTIVATE_X, m_ActivateXCheckBox);
	//}}AFX_DATA_MAP
	if (m_ExchangeData)
	{
		DDX_Text(pDX, IDC_POSITION_X, m_PositionX);
		DDX_Text(pDX, IDC_POSITION_Y, m_PositionY);
		DDX_Text(pDX, IDC_POSITION_Z, m_PositionZ);
		DDX_Radio(pDX, IDC_BLACK, m_ColorOption);
	}
}


BEGIN_MESSAGE_MAP(CGridLinesPage, CPropertyPage)
	//{{AFX_MSG_MAP(CGridLinesPage)
	ON_BN_CLICKED(IDC_ACTIVATE_X, OnActivateX)
	ON_BN_CLICKED(IDC_ACTIVATE_Y, OnActivateY)
	ON_BN_CLICKED(IDC_ACTIVATE_Z, OnActivateZ)
	ON_NOTIFY(UDN_DELTAPOS, IDC_POSITION_X_SPIN, OnDeltaposPositionXSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_POSITION_Y_SPIN, OnDeltaposPositionYSpin)
	ON_NOTIFY(UDN_DELTAPOS, IDC_POSITION_Z_SPIN, OnDeltaposPositionZSpin)
	ON_BN_CLICKED(IDC_GRAY, OnGray)
	ON_BN_CLICKED(IDC_BLACK, OnBlack)
	ON_BN_CLICKED(IDC_WHITE, OnWhite)
	ON_BN_CLICKED(IDC_ACTIVATE_OUTLINE, OnActivateOutline)
	ON_BN_CLICKED(IDC_RADIO_DISPLAY_NONE, OnRadioDisplayNone)
	ON_BN_CLICKED(IDC_RADIO_DISPLAY_MESH, OnRadioDisplayMesh)
	ON_BN_CLICKED(IDC_RADIO_DISPLAY_LINES, OnRadioDisplayLines)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGridLinesPage message handlers

BOOL CGridLinesPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	Reinitialize();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CGridLinesPage::Reinitialize()
{
	m_2D = FALSE;
	m_IrregularMesh = FALSE;
	m_LayeredMesh = FALSE;
	m_XMax = 0;
	m_YMax = 0;
	m_ZMax = 0;
	m_PositionX = 0;
	m_PositionY = 0;
	m_PositionZ = 0;
	m_ActivateXCheckBox.SetCheck(FALSE);
	m_ActivateYCheckBox.SetCheck(FALSE);
	m_ActivateZCheckBox.SetCheck(FALSE);
	m_ColorOption = 0;
	CustomUpdateData(FALSE);
	GetDlgItem(IDC_POSITION_X)->SetWindowText("");
	GetDlgItem(IDC_POSITION_Y)->SetWindowText("");
	GetDlgItem(IDC_POSITION_Z)->SetWindowText("");
	m_ActivateOutlineCheckBox.ShowWindow(SW_HIDE);
	Activate(FALSE);
}

void CGridLinesPage::Activate(BOOL Activate)
{
	BOOL bx = ((CButton *) GetDlgItem(IDC_ACTIVATE_X))->GetCheck();
	BOOL by = ((CButton *) GetDlgItem(IDC_ACTIVATE_Y))->GetCheck();
	BOOL bz = ((CButton *) GetDlgItem(IDC_ACTIVATE_Z))->GetCheck();

	// If it becomes possible to filter grid lines with layer meshes, the following 
	// line should be uncommented and the one after that should be deleted.
//	BOOL ActivateZ = !m_IrregularMesh || (m_IrregularMesh && m_LayeredMesh);
	BOOL ActivateZ = !m_IrregularMesh;
	GetDlgItem(IDC_POSITION_X)->EnableWindow(Activate && !m_IrregularMesh && bx && !m_2D);
	GetDlgItem(IDC_POSITION_Y)->EnableWindow(Activate && !m_IrregularMesh && by && !m_2D);
	GetDlgItem(IDC_POSITION_Z)->EnableWindow(Activate && bz && !m_2D);
	GetDlgItem(IDC_POSITION_X_SPIN)->EnableWindow(Activate && !m_IrregularMesh && bx && !m_2D);
	GetDlgItem(IDC_POSITION_Y_SPIN)->EnableWindow(Activate && !m_IrregularMesh && by && !m_2D);
	GetDlgItem(IDC_POSITION_Z_SPIN)->EnableWindow(Activate && bz && !m_2D);
	GetDlgItem(IDC_ACTIVATE_X)->EnableWindow(Activate && !m_IrregularMesh && !m_2D);
	GetDlgItem(IDC_ACTIVATE_Y)->EnableWindow(Activate && !m_IrregularMesh && !m_2D);
	GetDlgItem(IDC_ACTIVATE_Z)->EnableWindow(Activate && ActivateZ);
	GetDlgItem(IDC_RADIO_DISPLAY_NONE)->EnableWindow(Activate && m_IrregularMesh && m_LayeredMesh);
	GetDlgItem(IDC_RADIO_DISPLAY_LINES)->EnableWindow(Activate && m_IrregularMesh && m_LayeredMesh);
	GetDlgItem(IDC_RADIO_DISPLAY_MESH)->EnableWindow(Activate && m_IrregularMesh && m_LayeredMesh);

	GetDlgItem(IDC_GRAY)->EnableWindow(Activate);
	GetDlgItem(IDC_WHITE)->EnableWindow(Activate);
	GetDlgItem(IDC_BLACK)->EnableWindow(Activate);
	GetDlgItem(IDC_ACTIVATE_OUTLINE)->EnableWindow(Activate);
	m_IsActive = Activate;
	if (m_Parent->m_PropertySheet->GetActiveIndex() == 0)
	{
		m_Parent->m_ApplyButton.EnableWindow(Activate);
	}
}

BOOL CGridLinesPage::CustomUpdateData(BOOL b)
{
	m_ExchangeData = TRUE;
	BOOL result = UpdateData(b);
	m_ExchangeData = FALSE;
	return result;
}

void CGridLinesPage::Apply() 
{
	if (CustomUpdateData(TRUE))
	{
		if (m_PositionX < m_XMin) m_PositionX = m_XMin;
		if (m_PositionX > m_XMax) m_PositionX = m_XMax;
		if (m_PositionY < m_YMin) m_PositionY = m_YMin;
		if (m_PositionY > m_YMax) m_PositionY = m_YMax;
		if (m_PositionZ < m_ZMin) m_PositionZ = m_ZMin;
		if (m_PositionZ > m_ZMax) m_PositionZ = m_ZMax;
		CustomUpdateData(FALSE);
		m_pDoc->SetGridLinePositions(m_PositionX-1, m_PositionY-1, m_PositionZ-1);
	}
}

void CGridLinesPage::OnActivateX() 
{
	BOOL b = ((CButton *) GetDlgItem(IDC_ACTIVATE_X))->GetCheck();
	GetDlgItem(IDC_POSITION_X)->EnableWindow(b);
	GetDlgItem(IDC_POSITION_X_SPIN)->EnableWindow(b);
	m_pDoc->ActivateGridLines(0, b); 
}

void CGridLinesPage::OnActivateY() 
{
	BOOL b = ((CButton *) GetDlgItem(IDC_ACTIVATE_Y))->GetCheck();
	GetDlgItem(IDC_POSITION_Y)->EnableWindow(b);
	GetDlgItem(IDC_POSITION_Y_SPIN)->EnableWindow(b);
	m_pDoc->ActivateGridLines(1, b); 
}

void CGridLinesPage::OnActivateZ() 
{
	BOOL b = ((CButton *) GetDlgItem(IDC_ACTIVATE_Z))->GetCheck();
	GetDlgItem(IDC_POSITION_Z)->EnableWindow(b && !m_2D);
	GetDlgItem(IDC_POSITION_Z_SPIN)->EnableWindow(b && !m_2D);
	m_pDoc->ActivateGridLines(2, b); 
}

void CGridLinesPage::OnDeltaposPositionXSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_PositionX = __min(m_XMax, m_PositionX + 1);
		}
		else
		{
			m_PositionX = __max(m_XMin, m_PositionX - 1);
		}
	}
	CustomUpdateData(FALSE);
	Apply();
	*pResult = 0;
}

void CGridLinesPage::OnDeltaposPositionYSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_PositionY = __min(m_YMax, m_PositionY + 1);
		}
		else
		{
			m_PositionY = __max(m_YMin, m_PositionY - 1);
		}
	}
	CustomUpdateData(FALSE);
	Apply();
	*pResult = 0;
}

void CGridLinesPage::OnDeltaposPositionZSpin(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	if (CustomUpdateData(TRUE))
	{
		if (pNMUpDown->iDelta < 0)
		{
			m_PositionZ = __min(m_ZMax, m_PositionZ + 1);
		}
		else
		{
			m_PositionZ = __max(m_ZMin, m_PositionZ - 1);
		}
	}
	CustomUpdateData(FALSE);
	Apply();
	*pResult = 0;
}

void CGridLinesPage::OnGray() 
{
	m_pDoc->SetGridLineColor(0.5f, 0.5f, 0.5f);
}

void CGridLinesPage::OnBlack() 
{
	m_pDoc->SetGridLineColor(0, 0, 0);
}

void CGridLinesPage::OnWhite() 
{
	m_pDoc->SetGridLineColor(1, 1, 1);
}

BOOL CGridLinesPage::OnSetActive() 
{
	m_Parent->m_ApplyButton.EnableWindow(m_IsActive);
	return CPropertyPage::OnSetActive();
}

void CGridLinesPage::OnActivateOutline() 
{
	BOOL b = ((CButton *) GetDlgItem(IDC_ACTIVATE_OUTLINE))->GetCheck();
	m_pDoc->ActivateGridOutline(b); 
}

void CGridLinesPage::OnMeshOutlineChoice() 
{
	m_MeshOutlineChoice = -1;
	BOOL b = ((CButton *) GetDlgItem(IDC_RADIO_DISPLAY_NONE))->GetCheck();
	if (b)
	{
		m_MeshOutlineChoice = 0;
	}
	else
	{
		b = ((CButton *) GetDlgItem(IDC_RADIO_DISPLAY_LINES))->GetCheck();
		if (b)
		{
			m_MeshOutlineChoice = 1;
		}
		else
		{
			b = ((CButton *) GetDlgItem(IDC_RADIO_DISPLAY_MESH))->GetCheck();
			if (b)
			{
				m_MeshOutlineChoice = 2;
			}
		}
	}
	m_pDoc->SetMeshOutlineChoice(m_MeshOutlineChoice);
}

void CGridLinesPage::OnRadioDisplayLines() 
{
	OnMeshOutlineChoice();
}

void CGridLinesPage::OnRadioDisplayMesh() 
{
	OnMeshOutlineChoice();
}

void CGridLinesPage::OnRadioDisplayNone() 
{
	OnMeshOutlineChoice();
}

void CGridLinesPage::SetMeshOutlineChoice(int i)
{
	m_MeshOutlineChoice = i;
	switch (m_MeshOutlineChoice)
	{
	case 0:
		((CButton *) GetDlgItem(IDC_RADIO_DISPLAY_NONE))->SetCheck(TRUE);;
		break;
	case 1:
		((CButton *) GetDlgItem(IDC_RADIO_DISPLAY_LINES))->SetCheck(TRUE);;
		break;
	case 2:
		((CButton *) GetDlgItem(IDC_RADIO_DISPLAY_MESH))->SetCheck(TRUE);;
		break;
	}
}
