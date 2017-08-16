// AbstractCropPage.cpp : implementation file
//

#include "stdafx.h"
#include "modelviewer.h"
#include "AbstractCropPage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAbstractCropPage property page

IMPLEMENT_DYNCREATE(CAbstractCropPage, CPropertyPage)

CAbstractCropPage::CAbstractCropPage() : CPropertyPage(CAbstractCropPage::IDD)
{
	//{{AFX_DATA_INIT(CAbstractCropPage)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

CAbstractCropPage::~CAbstractCropPage()
{
}

void CAbstractCropPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAbstractCropPage)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CAbstractCropPage, CPropertyPage)
	//{{AFX_MSG_MAP(CAbstractCropPage)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAbstractCropPage message handlers
