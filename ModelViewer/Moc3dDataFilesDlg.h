#if !defined(AFX_MOC3DDATAFILESDLG_H__664C74AA_FB98_11D3_8108_00C04F61038F__INCLUDED_)
#define AFX_MOC3DDATAFILESDLG_H__664C74AA_FB98_11D3_8108_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Moc3dDataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMoc3dDataFilesDlg dialog

class CMoc3dDataFilesDlg : public CDialog
{
// Construction
public:
	CMoc3dDataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CMoc3dDataFilesDlg)
	enum { IDD = IDD_MOC3D_DATA_FILES };
	CComboBox	m_DataTypeList;
	CString	m_NameFile;
	CString	m_PathFile;
	CString	m_TopFile;
	int		m_DataType;
	BOOL	m_PathlineBackwards;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMoc3dDataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CMoc3dDataFilesDlg)
	afx_msg void OnBrowseName();
	virtual BOOL OnInitDialog();
	afx_msg void OnBrowsePath2();
	afx_msg void OnBrowseTop();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MOC3DDATAFILESDLG_H__664C74AA_FB98_11D3_8108_00C04F61038F__INCLUDED_)
