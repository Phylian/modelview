#if !defined(AFX_SUTRADATAFILESDLG_H__B3D44C20_1A8E_11D4_9D64_9F7C6479B67F__INCLUDED_)
#define AFX_SUTRADATAFILESDLG_H__B3D44C20_1A8E_11D4_9D64_9F7C6479B67F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// SutraDataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSutraDataFilesDlg dialog

class CSutraDataFilesDlg : public CDialog
{
// Construction
public:
	CSutraDataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CSutraDataFilesDlg)
	enum { IDD = IDD_SUTRA_DATA_FILES };
	CString	m_EleFile;
	CString	m_NodFile;
	CString	m_SutraInpFile;
	int		m_FileType;
	int		m_TimeUnits;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSutraDataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CSutraDataFilesDlg)
	afx_msg void OnBrowseEle();
	afx_msg void OnBrowseNod();
	afx_msg void OnBrowseSutraInp();
	afx_msg void OnSutraNod();
	afx_msg void OnSutraBinary();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SUTRADATAFILESDLG_H__B3D44C20_1A8E_11D4_9D64_9F7C6479B67F__INCLUDED_)
