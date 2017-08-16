#if !defined(AFX_MODFLOW2000DATAFILESDLG_H__0A5F53CC_995C_11D4_8137_00C04F61038F__INCLUDED_)
#define AFX_MODFLOW2000DATAFILESDLG_H__0A5F53CC_995C_11D4_8137_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Modflow2000DataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CModflow2000DataFilesDlg dialog

class CModflow2000DataFilesDlg : public CDialog
{
// Construction
public:
	CModflow2000DataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CModflow2000DataFilesDlg)
	enum { IDD = IDD_MODFLOW2000_DATA_FILES };
	CComboBox	m_DataTypeList;
	CString	m_NameFile;
	CString	m_PathFile;
	int		m_DataType;
	BOOL	m_PathlineBackwards;
	CString	m_ExternalFile;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CModflow2000DataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CModflow2000DataFilesDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnBrowseName();
	afx_msg void OnBrowsePathline();
	afx_msg void OnBrowseExternal();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MODFLOW2000DATAFILESDLG_H__0A5F53CC_995C_11D4_8137_00C04F61038F__INCLUDED_)
