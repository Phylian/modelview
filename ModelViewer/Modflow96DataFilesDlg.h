#if !defined(AFX_MODFLOW96DATAFILESDLG_H__8932E0A2_F5F2_11D3_8106_00C04F61038F__INCLUDED_)
#define AFX_MODFLOW96DATAFILESDLG_H__8932E0A2_F5F2_11D3_8106_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Modflow96DataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CModflow96DataFilesDlg dialog

class CModflow96DataFilesDlg : public CDialog
{
// Construction
public:
	CModflow96DataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CModflow96DataFilesDlg)
	enum { IDD = IDD_MODFLOW96_DATA_FILES };
	CComboBox	m_DataTypeList;
	CString	m_ElevationFile;
	CString	m_NameFile;
	CString	m_PathlineFile;
	int		m_DataType;
	BOOL	m_PathlineBackwards;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CModflow96DataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CModflow96DataFilesDlg)
	afx_msg void OnBrowseName();
	afx_msg void OnBrowseElevation();
	virtual BOOL OnInitDialog();
	afx_msg void OnBrowsePathline();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MODFLOW96DATAFILESDLG_H__8932E0A2_F5F2_11D3_8106_00C04F61038F__INCLUDED_)
