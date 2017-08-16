# Microsoft Developer Studio Project File - Name="ModelViewer" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=ModelViewer - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ModelViewer.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ModelViewer.mak" CFG="ModelViewer - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ModelViewer - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "ModelViewer - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "ModelViewer"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ModelViewer - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt /winapp
# ADD F90 /browser /compile_only /include:"Release/" /nologo /warn:nofileopt /winapp
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "..\mv" /I "C:\Program Files\vtk40\include\vtk" /I "C:\Program Files\HTML Help Workshop\include" /I "$(DEV_HDF5_INC)" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /FR /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 vtkCommon.lib vtkGraphics.lib vtkImaging.lib vtkFiltering.lib vtkRendering.lib mv.lib htmlhelp.lib sutr_r.lib /nologo /subsystem:windows /machine:I386 /out:"Release\modview.exe" /libpath:"C:\Program Files\vtk40\lib\vtk" /libpath:"..\mv\Release" /libpath:"..\SutraReader\Release" /libpath:"C:\Program Files\HTML Help Workshop\lib"

!ELSEIF  "$(CFG)" == "ModelViewer - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt /winapp
# ADD F90 /browser /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt /winapp
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\mv" /I "C:\Program Files\vtk40\include\vtk" /I "C:\Program Files\HTML Help Workshop\include" /I "$(DEV_HDF5_INC)" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_AFXDLL" /Fr /Yu"stdafx.h" /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 vtkCommon.lib vtkGraphics.lib vtkImaging.lib vtkFiltering.lib vtkRendering.lib mv.lib htmlhelp.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"LIBCMT" /out:"Debug\Modview.exe" /libpath:"C:\Program Files\vtk40\lib\vtk" /libpath:"..\mv\Debug" /libpath:"..\SutraReader\Debug" /libpath:"C:\Program Files\HTML Help Workshop\lib"
# SUBTRACT LINK32 /profile

!ENDIF 

# Begin Target

# Name "ModelViewer - Win32 Release"
# Name "ModelViewer - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\AnimationControlsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\AnimationDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\AnimationOptionsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\AxesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\AxesPage.cpp
# End Source File
# Begin Source File

SOURCE=.\BackgroundPage.cpp
# End Source File
# Begin Source File

SOURCE=.\BitmapResolutionDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\BoundingBoxPage.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarColorsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarLimitsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarPreviewListBox.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarSizePage.cpp
# End Source File
# Begin Source File

SOURCE=.\ColorBarTextPage.cpp
# End Source File
# Begin Source File

SOURCE=.\CropControlsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\CropDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\CropOptionsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\CropVectorsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\CustomIsosurfacePage.cpp
# End Source File
# Begin Source File

SOURCE=.\DataDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\DataFilesDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\DataSelectionDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\DisplaySizeDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ExportAnimationDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\GeometryDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\GridDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\GridLinesPage.cpp
# End Source File
# Begin Source File

SOURCE=.\GridShellPage.cpp
# End Source File
# Begin Source File

SOURCE=.\ImageGenerationDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\IsosurfaceDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\LightingDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\LightsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\LogTransformAxes.cpp
# End Source File
# Begin Source File

SOURCE=.\MainFrm.cpp
# End Source File
# Begin Source File

SOURCE=.\Mf2kgwtDataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Moc3dDataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ModelFeatureListBox.cpp
# End Source File
# Begin Source File

SOURCE=.\ModelFeaturesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ModelSelectionDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ModelViewer.cpp
# End Source File
# Begin Source File

SOURCE=.\ModelViewer.rc
# End Source File
# Begin Source File

SOURCE=.\Modflow2000DataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Modflow96DataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Mt3dmsDataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Mvdoc.cpp
# End Source File
# Begin Source File

SOURCE=.\Mvview.cpp
# End Source File
# Begin Source File

SOURCE=.\OverlayBoundsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\OverlayControlsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\OverlayDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\OverlayFilePage.cpp
# End Source File
# Begin Source File

SOURCE=.\ParticlesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\PathlineDataPage.cpp
# End Source File
# Begin Source File

SOURCE=.\PathlinesClippingPage.cpp
# End Source File
# Begin Source File

SOURCE=.\PathlinesColorPage.cpp
# End Source File
# Begin Source File

SOURCE=.\PathlinesColorPage.h
# End Source File
# Begin Source File

SOURCE=.\PathlinesDisplayPage.cpp
# End Source File
# Begin Source File

SOURCE=.\PathlinesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\PhastDataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\PreferencesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ProgressWnd.cpp
# End Source File
# Begin Source File

SOURCE=.\RegularIsosurfacePage.cpp
# End Source File
# Begin Source File

SOURCE=.\ScalarDataPage.cpp
# End Source File
# Begin Source File

SOURCE=.\ScalePage.cpp
# End Source File
# Begin Source File

SOURCE=.\SolidDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp
# ADD CPP /Yc"stdafx.h"
# End Source File
# Begin Source File

SOURCE=.\SubgridPage.cpp
# End Source File
# Begin Source File

SOURCE=.\SurfacePage.cpp
# End Source File
# Begin Source File

SOURCE=.\SutraDataFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\UcodeDataFileDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VectorControlsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\VectorDataPage.cpp
# End Source File
# Begin Source File

SOURCE=.\VectorDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VectorOptionsPage.cpp
# End Source File
# Begin Source File

SOURCE=.\VectorThresholdPage.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\AbstractCropPage.h
# End Source File
# Begin Source File

SOURCE=.\AnimationControlsPage.h
# End Source File
# Begin Source File

SOURCE=.\AnimationDlg.h
# End Source File
# Begin Source File

SOURCE=.\AnimationOptionsPage.h
# End Source File
# Begin Source File

SOURCE=.\AxesDlg.h
# End Source File
# Begin Source File

SOURCE=.\AxesPage.h
# End Source File
# Begin Source File

SOURCE=.\BackgroundPage.h
# End Source File
# Begin Source File

SOURCE=.\BitmapResolutionDlg.h
# End Source File
# Begin Source File

SOURCE=.\BoundingBoxPage.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarColorsPage.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarDataSource.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarDlg.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarLimitsPage.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarPreviewListBox.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarSizePage.h
# End Source File
# Begin Source File

SOURCE=.\ColorBarTextPage.h
# End Source File
# Begin Source File

SOURCE=.\CropControlsPage.h
# End Source File
# Begin Source File

SOURCE=.\CropDlg.h
# End Source File
# Begin Source File

SOURCE=.\CropOptionsPage.h
# End Source File
# Begin Source File

SOURCE=.\CropVectorsPage.h
# End Source File
# Begin Source File

SOURCE=.\CustomIsosurfacePage.h
# End Source File
# Begin Source File

SOURCE=.\DataDlg.h
# End Source File
# Begin Source File

SOURCE=.\DataFilesDialog.h
# End Source File
# Begin Source File

SOURCE=.\DataSelectionDlg.h
# End Source File
# Begin Source File

SOURCE=.\DisplaySizeDlg.h
# End Source File
# Begin Source File

SOURCE=.\ExportAnimationDlg.h
# End Source File
# Begin Source File

SOURCE=.\GeometryDlg.h
# End Source File
# Begin Source File

SOURCE=.\GridDlg.h
# End Source File
# Begin Source File

SOURCE=.\GridLinesPage.h
# End Source File
# Begin Source File

SOURCE=.\GridShellPage.h
# End Source File
# Begin Source File

SOURCE=.\ImageGenerationDlg.h
# End Source File
# Begin Source File

SOURCE=.\IsosurfaceDlg.h
# End Source File
# Begin Source File

SOURCE=.\LightingDlg.h
# End Source File
# Begin Source File

SOURCE=.\LightsPage.h
# End Source File
# Begin Source File

SOURCE=.\LogTransformAxes.h
# End Source File
# Begin Source File

SOURCE=.\MainFrm.h
# End Source File
# Begin Source File

SOURCE=.\Mf2kgwtDataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\Moc3dDataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\ModelFeatureListBox.h
# End Source File
# Begin Source File

SOURCE=.\ModelFeaturesDlg.h
# End Source File
# Begin Source File

SOURCE=.\ModelSelectionDlg.h
# End Source File
# Begin Source File

SOURCE=.\ModelViewer.h
# End Source File
# Begin Source File

SOURCE=.\Modflow2000DataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\Modflow96DataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\Mt3dmsDataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\MvDoc.h
# End Source File
# Begin Source File

SOURCE=.\MvView.h
# End Source File
# Begin Source File

SOURCE=.\OverlayBoundsPage.h
# End Source File
# Begin Source File

SOURCE=.\OverlayControlsPage.h
# End Source File
# Begin Source File

SOURCE=.\OverlayDlg.h
# End Source File
# Begin Source File

SOURCE=.\OverlayFilePage.h
# End Source File
# Begin Source File

SOURCE=.\ParticlesDlg.h
# End Source File
# Begin Source File

SOURCE=.\PathlineDataPage.h
# End Source File
# Begin Source File

SOURCE=.\PathlinesClippingPage.h
# End Source File
# Begin Source File

SOURCE=.\PathlinesDisplayPage.h
# End Source File
# Begin Source File

SOURCE=.\PathlinesDlg.h
# End Source File
# Begin Source File

SOURCE=.\PhastDataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\PreferencesDlg.h
# End Source File
# Begin Source File

SOURCE=.\ProgressWnd.h
# End Source File
# Begin Source File

SOURCE=.\RegularIsosurfacePage.h
# End Source File
# Begin Source File

SOURCE=.\Resource.h
# PROP Ignore_Default_Tool 1
# End Source File
# Begin Source File

SOURCE=.\ScalarDataPage.h
# End Source File
# Begin Source File

SOURCE=.\ScalePage.h
# End Source File
# Begin Source File

SOURCE=.\SolidDlg.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\SubgridPage.h
# End Source File
# Begin Source File

SOURCE=.\SurfacePage.h
# End Source File
# Begin Source File

SOURCE=.\SutraDataFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\UcodeDataFileDlg.h
# End Source File
# Begin Source File

SOURCE=.\VectorControlsPage.h
# End Source File
# Begin Source File

SOURCE=.\VectorDataPage.h
# End Source File
# Begin Source File

SOURCE=.\VectorDlg.h
# End Source File
# Begin Source File

SOURCE=.\VectorOptionsPage.h
# End Source File
# Begin Source File

SOURCE=.\VectorThresholdPage.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\ModelViewer.ico
# End Source File
# Begin Source File

SOURCE=.\res\ModelViewer.rc2
# End Source File
# Begin Source File

SOURCE=.\res\Mv.ico
# End Source File
# Begin Source File

SOURCE=.\res\MvDoc.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\ModelViewer.reg
# End Source File
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# End Target
# End Project
