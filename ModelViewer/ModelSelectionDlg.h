#if !defined(AFX_MODELSELECTIONDLG_H__68E44271_F125_11D3_8105_00C04F61038F__INCLUDED_)
#define AFX_MODELSELECTIONDLG_H__68E44271_F125_11D3_8105_00C04F61038F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ModelSelectionDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CModelSelectionDlg dialog

class CModelSelectionDlg : public CDialog
{
// Construction
public:
	CModelSelectionDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CModelSelectionDlg)
	enum { IDD = IDD_MODEL_SELECTION };
	CComboBox	m_ModelList;
	BOOL	m_RememberSelection;
	CString	m_SelectedModel;
	//}}AFX_DATA

	int m_NumberOfModels;
	char **m_ModelNames;
	int m_InitialSelection;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CModelSelectionDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CModelSelectionDlg)
	virtual BOOL OnInitDialog();
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MODELSELECTIONDLG_H__68E44271_F125_11D3_8105_00C04F61038F__INCLUDED_)
