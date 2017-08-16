#if !defined(AFX_VECTORCONTROLSPAGE_H__121CFCA4_0014_11D4_8108_00C04F61038F__INCLUDED_)
#define AFX_VECTORCONTROLSPAGE_H__121CFCA4_0014_11D4_8108_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// VectorControlsPage.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CVectorControlsPage dialog
class CMvDoc;

class CVectorControlsPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CVectorControlsPage)

// Construction
public:
	CVectorControlsPage();
	~CVectorControlsPage();
	void Reinitialize();
	void Activate(BOOL b);
	BOOL CustomUpdateData(BOOL b);
	void Apply();
	void OnDefault();
	CMvDoc *m_pDoc;

	int		m_imin;
	int		m_imax;
	int		m_irate;
	int		m_jmin;
	int		m_jmax;
	int		m_jrate;
	int		m_kmin;
	int		m_kmax;
	int		m_krate;

	int m_ilimit0;
	int m_ilimit1;
	int m_jlimit0;
	int m_jlimit1;
	int m_klimit0;
	int m_klimit1;

	BOOL m_IrregularMesh;

// Dialog Data
	//{{AFX_DATA(CVectorControlsPage)
	enum { IDD = IDD_VECTOR_CONTROLS };
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CVectorControlsPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	BOOL m_ExchangeData;

	// Generated message map functions
	//{{AFX_MSG(CVectorControlsPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnDeltaposIminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposImaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposJminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposJmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposKminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposKmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VECTORCONTROLSPAGE_H__121CFCA4_0014_11D4_8108_00C04F61038F__INCLUDED_)
