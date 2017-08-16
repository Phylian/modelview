#if !defined(AFX_ABSTRACTCROPPAGE_H__2384D652_88BE_4C0A_AF8A_479D5D03A870__INCLUDED_)
#define AFX_ABSTRACTCROPPAGE_H__2384D652_88BE_4C0A_AF8A_479D5D03A870__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// AbstractCropPage.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CAbstractCropPage dialog

class CAbstractCropPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CAbstractCropPage)

// Construction
public:
	CAbstractCropPage();
	~CAbstractCropPage();

// Dialog Data
	//{{AFX_DATA(CAbstractCropPage)
	enum { IDD = IDD_DIALOG1 };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CAbstractCropPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CAbstractCropPage)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_ABSTRACTCROPPAGE_H__2384D652_88BE_4C0A_AF8A_479D5D03A870__INCLUDED_)
