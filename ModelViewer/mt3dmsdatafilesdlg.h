#if !defined(AFX_MT3DMSDATAFILESDLG_H__FFA93BE9_F175_11D3_8105_00C04F61038F__INCLUDED_)
#define AFX_MT3DMSDATAFILESDLG_H__FFA93BE9_F175_11D3_8105_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mt3dmsDataFilesDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMt3dmsDataFilesDlg dialog

class CMt3dmsDataFilesDlg : public CDialog
{
// Construction
public:
	CMt3dmsDataFilesDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CMt3dmsDataFilesDlg)
	enum { IDD = IDD_MT3DMS_DATA_FILES };
	CComboBox	m_DataTypeList;
	CString	m_CnfFile;
	CString	m_UcnFile;
	CString	m_LinkFile;
	CString	m_PathFile;
	int		m_DataType;
	BOOL	m_PathlineBackwards;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMt3dmsDataFilesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CMt3dmsDataFilesDlg)
	afx_msg void OnBrowseCnf();
	afx_msg void OnBrowseUcn();
	virtual BOOL OnInitDialog();
	afx_msg void OnBrowseMtlink();
	afx_msg void OnBrowsePath();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MT3DMSDATAFILESDLG_H__FFA93BE9_F175_11D3_8105_00C04F61038F__INCLUDED_)
