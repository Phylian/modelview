#if !defined(AFX_SUBGRIDPAGE_H__C4133468_03FF_11D4_8108_00C04F61038F__INCLUDED_)
#define AFX_SUBGRIDPAGE_H__C4133468_03FF_11D4_8108_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// SubgridPage.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSubgridPage dialog
class CMvDoc;
class CGridDlg;

class CSubgridPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CSubgridPage)

// Construction
public:
	CSubgridPage();
	~CSubgridPage();
	void Reinitialize();
	void Activate(BOOL b);
	BOOL CustomUpdateData(BOOL b);
	void Apply();
	CMvDoc *m_pDoc;
	CGridDlg *m_Parent;
	BOOL	m_ExchangeData;
	BOOL	m_ActivateSubgrid;
	int		m_ilow;
	int		m_ihigh;
	int		m_jlow;
	int		m_jhigh;
	int		m_klow;
	int		m_khigh;
	int m_imax;
	int m_jmax;
	int m_kmax;
	BOOL m_IsActive;
	BOOL m_2D;
	BOOL m_IrregularMesh;
	BOOL m_LayeredMesh;

// Dialog Data
	//{{AFX_DATA(CSubgridPage)
	enum { IDD = IDD_SUBGRID };
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CSubgridPage)
	public:
	virtual BOOL OnSetActive();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CSubgridPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnActivateSubgrid();
	afx_msg void OnDeltaposIlowSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposIhighSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposJlowSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposJhighSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposKlowSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposKhighSpin(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SUBGRIDPAGE_H__C4133468_03FF_11D4_8108_00C04F61038F__INCLUDED_)
