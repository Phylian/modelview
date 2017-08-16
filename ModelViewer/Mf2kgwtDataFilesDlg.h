#if !defined(AFX_MF2KGWTDATAFILESDLG_H__541840CA_151A_11D5_8158_00C04F61038F__INCLUDED_)
#define AFX_MF2KGWTDATAFILESDLG_H__541840CA_151A_11D5_8158_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mf2kgwtDataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMf2kgwtDataFilesDlg dialog

class CMf2kgwtDataFilesDlg : public CDialog
{
// Construction
public:
	CMf2kgwtDataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CMf2kgwtDataFilesDlg)
	enum { IDD = IDD_MF2KGWT_DATA_FILES };
	CComboBox	m_DataTypeList;
	CString	m_NameFile;
	CString	m_PathFile;
	int		m_DataType;
	BOOL	m_PathlineBackwards;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMf2kgwtDataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CMf2kgwtDataFilesDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnBrowseName();
	afx_msg void OnBrowsePath2();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MF2KGWTDATAFILESDLG_H__541840CA_151A_11D5_8158_00C04F61038F__INCLUDED_)
