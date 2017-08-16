#if !defined(AFX_CROPVECTORSPAGE_H__2447BF73_04BC_40D3_B68F_5B93BC61F475__INCLUDED_)
#define AFX_CROPVECTORSPAGE_H__2447BF73_04BC_40D3_B68F_5B93BC61F475__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// CropVectorsPage.h : header file
//

class CMvDoc;
class CVectorDlg;


/////////////////////////////////////////////////////////////////////////////
// CCropVectorsPage dialog

class CCropVectorsPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CCropVectorsPage)

// Construction
public:
	float	m_XDelta;
	float	m_XMax;
	float	m_XMin;
	float	m_YDelta;
	float	m_YMax;
	float	m_YMin;
	float	m_ZDelta;
	float	m_ZMax;
	float	m_ZMin;
	int   m_CropAngle;

	void OnDefault();
	void Activate(BOOL b);
	void Reinitialize();
	void Apply();
	BOOL CustomUpdateData(BOOL b);
	CCropVectorsPage();
	~CCropVectorsPage();
	CMvDoc *m_pDoc;

// Dialog Data
	//{{AFX_DATA(CCropVectorsPage)
	enum { IDD = IDD_CROP_VECTORS };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CCropVectorsPage)
	public:
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

//	CVectorDlg *m_Parent;
	BOOL	m_ExchangeData;
	BOOL m_IsActive;


// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CCropVectorsPage)
	afx_msg void OnDeltaposXminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposXmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposYminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposYmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposZminSpin(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDeltaposZmaxSpin(NMHDR* pNMHDR, LRESULT* pResult);
	virtual BOOL OnInitDialog();
	afx_msg void OnDeltaposCropAngleSpin(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	void AssignDefaultValues();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CROPVECTORSPAGE_H__2447BF73_04BC_40D3_B68F_5B93BC61F475__INCLUDED_)
