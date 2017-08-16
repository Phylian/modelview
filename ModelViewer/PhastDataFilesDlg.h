#if !defined(AFX_PHASTDATAFILESDLG_H__C4A60869_5C6B_4AD7_82D3_BD39BB242964__INCLUDED_)
#define AFX_PHASTDATAFILESDLG_H__C4A60869_5C6B_4AD7_82D3_BD39BB242964__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PhastDataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPhastDataFilesDlg dialog

class CPhastDataFilesDlg : public CDialog
{
// Construction
public:
	CPhastDataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CPhastDataFilesDlg)
	enum { IDD = IDD_PHAST_DATA_FILES };
	CString	m_strHDFDataFile;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPhastDataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	void DDV_ValidFile(CDataExchange* pDX, LPCTSTR szFileName);

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CPhastDataFilesDlg)
	afx_msg void OnBrowseHDF();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PHASTDATAFILESDLG_H__C4A60869_5C6B_4AD7_82D3_BD39BB242964__INCLUDED_)
