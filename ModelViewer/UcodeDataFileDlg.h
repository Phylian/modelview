#if !defined(AFX_UCODEDATAFILEDLG_H__38C17AA9_080D_42E3_BF13_F6332CFE7219__INCLUDED_)
#define AFX_UCODEDATAFILEDLG_H__38C17AA9_080D_42E3_BF13_F6332CFE7219__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// UcodeDataFileDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CUcodeDataFileDlg dialog

class CUcodeDataFileDlg : public CDialog
{
// Construction
public:
	CUcodeDataFileDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CUcodeDataFileDlg)
	enum { IDD = IDD_UCODE_SOS_DATA_FILES };
	CString	m_UcodeSosFile;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CUcodeDataFileDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CUcodeDataFileDlg)
	afx_msg void OnBrowseUcode();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_UCODEDATAFILEDLG_H__38C17AA9_080D_42E3_BF13_F6332CFE7219__INCLUDED_)
