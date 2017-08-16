#if !defined(AFX_LOGTRANSFORMAXES_H__F9DF0A0F_CB75_41F7_848D_6CB411D0F3E6__INCLUDED_)
#define AFX_LOGTRANSFORMAXES_H__F9DF0A0F_CB75_41F7_848D_6CB411D0F3E6__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// LogTransformAxes.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CLogTransformAxes dialog
class CMvDoc;

class CLogTransformAxes : public CPropertyPage
{
	DECLARE_DYNCREATE(CLogTransformAxes)

// Construction
public:
	CLogTransformAxes();
	~CLogTransformAxes();
	CMvDoc *m_pDoc;
	void Reinitialize();
	void Update();

// Dialog Data
	//{{AFX_DATA(CLogTransformAxes)
	enum { IDD = IDD_LOG_TRANSFORM_AXES_DATA };
	CButton	m_LogTransformZCheckBox;
	CButton	m_LogTransformYCheckBox;
	CButton	m_LogTransformXCheckBox;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CLogTransformAxes)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CLogTransformAxes)
	afx_msg void OnLogTransformX();
	afx_msg void OnLogTransformY();
	afx_msg void OnLogTransformZ();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LOGTRANSFORMAXES_H__F9DF0A0F_CB75_41F7_848D_6CB411D0F3E6__INCLUDED_)
