#if !defined(AFX_PARTICLESDLG_H__ED72B962_0E53_4472_A7E3_C31020DF889A__INCLUDED_)
#define AFX_PARTICLESDLG_H__ED72B962_0E53_4472_A7E3_C31020DF889A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ParticlesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CParticlesDlg dialog

class CMvDoc;

class CParticlesDlg : public CDialog
{
// Construction
public:
	CParticlesDlg(CWnd* pParent, CMvDoc *pDoc);   // standard constructor
	BOOL Create();
	void Activate(BOOL b);
	void Reinitialize();
	void ApplyConcentrationFilter(); 
	void ApplyCropFilter(); 

// Dialog Data
	//{{AFX_DATA(CParticlesDlg)
	enum { IDD = IDD_PARTICLES };
	float	m_MaxConc;
	float	m_MinConc;
	CString	m_TrueMaxConc;
	CString	m_TrueMinConc;
	float	m_DeltaX;
	float	m_XMax;
	float	m_XMin;
	float	m_DeltaY;
	float	m_YMax;
	float	m_YMin;
	float	m_DeltaZ;
	float	m_ZMin;
	float	m_ZMax;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CParticlesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:
	float m_ModelMinConc;
	float m_ModelMaxConc;
	CWnd* m_pParent;
	int m_nID;
	CMvDoc *m_pDoc;

	// Generated message map functions
	//{{AFX_MSG(CParticlesDlg)
	afx_msg void OnDeltaposParticleSizeSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnApply();
	afx_msg void OnDeltaposXminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposXmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposYminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposYmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposZminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposZmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnFull();
	virtual BOOL OnInitDialog();
	afx_msg void OnDone();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PARTICLESDLG_H__ED72B962_0E53_4472_A7E3_C31020DF889A__INCLUDED_)
