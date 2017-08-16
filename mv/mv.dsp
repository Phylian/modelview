# Microsoft Developer Studio Project File - Name="mv" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=mv - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mv.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mv.mak" CFG="mv - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mv - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "mv - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "Mv_branch2"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "mv - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /dll /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /include:"Release/" /dll /names:lowercase /nologo /warn:nofileopt
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MV_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "C:\Program Files\vtk40\include\vtk" /I "$(DEV_HDF5_INC)" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MV_EXPORTS" /D "MV_DLL" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 vtkCommon.lib vtkGraphics.lib vtkImaging.lib vtkFiltering.lib vtkRendering.lib mf2k_r.lib mf2k_r1.lib mf2k_r2.lib mf96_r.lib mf96_r1.lib mf96_r2.lib mt_r.lib mt_r1.lib mt_r2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib hdf5dll.lib sutr_r22.lib /nologo /dll /machine:I386 /libpath:"C:\Program Files\vtk40\lib\vtk" /libpath:"..\Modflow2000Reader\Release" /libpath:"..\Modflow2000Reader1" /libpath:"..\Modflow2000Reader2\Release" /libpath:"..\Modflow96Reader\Release" /libpath:"..\Modflow96Reader1" /libpath:"..\Modflow96Reader2\Release" /libpath:"..\Mt3dmsReader\Release" /libpath:"..\Mt3dmsReader1" /libpath:"..\Mt3dmsReader2\Release" /libpath:"..\SutraReader22\Debug" /libpath:"$(DEV_HDF5_LIBDLL)"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy .\Release\mv.dll ..\ModelViewer\Release\mv.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "mv - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /dll /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /include:"Debug/" /dll /names:lowercase /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MV_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "C:\Program Files\vtk40\include\vtk" /I "$(DEV_HDF5_INC)" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "MV_DLL" /Fr /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 vtkCommon.lib vtkGraphics.lib vtkImaging.lib vtkFiltering.lib vtkRendering.lib mf2k_r.lib mf2k_r1.lib mf2k_r2.lib mf96_r.lib mf96_r1.lib mf96_r2.lib mt_r.lib mt_r1.lib mt_r2.lib sutr_r22.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib hdf5dll.lib sutr_r22.lib /nologo /dll /debug /machine:I386 /libpath:"C:\Program Files\vtk40\lib\vtk" /libpath:"..\Modflow2000Reader\Debug" /libpath:"..\Modflow2000Reader1" /libpath:"..\Modflow2000Reader2\Debug" /libpath:"..\Modflow96Reader\Debug" /libpath:"..\Modflow96Reader1" /libpath:"..\Modflow96Reader2\Debug" /libpath:"..\Mt3dmsReader\Debug" /libpath:"..\Mt3dmsReader1" /libpath:"..\Mt3dmsReader2\Debug" /libpath:"..\SutraReader22\Debug" /libpath:"$(DEV_HDF5_LIBDLL_D)"
# SUBTRACT LINK32 /profile
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy .\Debug\mv.dll ..\ModelViewer\Debug\mv.dll
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "mv - Win32 Release"
# Name "mv - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\AbstractExternalFile.cpp
# End Source File
# Begin Source File

SOURCE=.\AbstractModflowDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\AbstractSutraDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\ExternalModflowFile.cpp
# End Source File
# Begin Source File

SOURCE=.\ExternalSutraFile.cpp
# End Source File
# Begin Source File

SOURCE=.\GenericDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\Mf2kgwtDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\Moc3dDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\Modflow2000DataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\Modflow96DataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\ModpathReader.cpp
# End Source File
# Begin Source File

SOURCE=.\Mt3dmsDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\mvAxes.cpp
# End Source File
# Begin Source File

SOURCE=.\mvBoundingBox.cpp
# End Source File
# Begin Source File

SOURCE=.\mvClipParticles.cpp
# End Source File
# Begin Source File

SOURCE=.\mvColorBandFilter.cpp
# End Source File
# Begin Source File

SOURCE=.\mvColorBar.cpp
# End Source File
# Begin Source File

SOURCE=.\mvColorTable.cpp
# End Source File
# Begin Source File

SOURCE=.\mvCroppableDisplayObject.cpp
# End Source File
# Begin Source File

SOURCE=.\mvCustomAppendPolyData.cpp
# End Source File
# Begin Source File

SOURCE=.\mvDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\mvDisplayObject.cpp
# End Source File
# Begin Source File

SOURCE=.\mvDisplayText.cpp
# End Source File
# Begin Source File

SOURCE=.\mvDxfReader.cpp
# End Source File
# Begin Source File

SOURCE=.\mvExternalMesh.cpp
# End Source File
# Begin Source File

SOURCE=.\mvExternalMeshVector.cpp
# End Source File
# Begin Source File

SOURCE=.\mvExtractGrid.cpp
# End Source File
# Begin Source File

SOURCE=.\mvFaceLess.cpp
# End Source File
# Begin Source File

SOURCE=.\mvGridLines.cpp
# End Source File
# Begin Source File

SOURCE=.\mvGridOutline.cpp
# End Source File
# Begin Source File

SOURCE=.\mvGridShell.cpp
# End Source File
# Begin Source File

SOURCE=.\mvGUISettings.cpp
# End Source File
# Begin Source File

SOURCE=.\mvHashTable.cpp
# End Source File
# Begin Source File

SOURCE=.\mvLinkList.cpp
# End Source File
# Begin Source File

SOURCE=.\mvLogColorTable.cpp
# End Source File
# Begin Source File

SOURCE=.\mvManager.cpp
# End Source File
# Begin Source File

SOURCE=.\mvMeshLines.cpp
# End Source File
# Begin Source File

SOURCE=.\mvModelFeatures.cpp
# End Source File
# Begin Source File

SOURCE=.\mvModelList.cpp
# End Source File
# Begin Source File

SOURCE=.\mvOverlay.cpp
# End Source File
# Begin Source File

SOURCE=.\mvParticles.cpp
# End Source File
# Begin Source File

SOURCE=.\mvPathlines.cpp
# End Source File
# Begin Source File

SOURCE=.\mvSutraFace.cpp
# End Source File
# Begin Source File

SOURCE=.\mvUtil.cpp
# End Source File
# Begin Source File

SOURCE=.\mvVRMLExporter.cpp
# End Source File
# Begin Source File

SOURCE=.\PhastDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\shpopen.c
# End Source File
# Begin Source File

SOURCE=.\SutraDataSource.cpp
# End Source File
# Begin Source File

SOURCE=.\UcodeDataSource.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\AbstractExternalFile.h
# End Source File
# Begin Source File

SOURCE=.\AbstractModflowDataSource.h
# End Source File
# Begin Source File

SOURCE=.\AbstractSutraDataSource.h
# End Source File
# Begin Source File

SOURCE=.\ExternalModflowFile.h
# End Source File
# Begin Source File

SOURCE=.\ExternalSutraFile.h
# End Source File
# Begin Source File

SOURCE=.\GenericDataSource.h
# End Source File
# Begin Source File

SOURCE=.\Mf2kgwtDataSource.h
# End Source File
# Begin Source File

SOURCE=.\Moc3dDataSource.h
# End Source File
# Begin Source File

SOURCE=.\Modflow2000DataSource.h
# End Source File
# Begin Source File

SOURCE=.\Modflow96DataSource.h
# End Source File
# Begin Source File

SOURCE=.\ModpathReader.h
# End Source File
# Begin Source File

SOURCE=.\Mt3dmsDataSource.h
# End Source File
# Begin Source File

SOURCE=.\mvAxes.h
# End Source File
# Begin Source File

SOURCE=.\mvBoundingBox.h
# End Source File
# Begin Source File

SOURCE=.\mvClipParticles.h
# End Source File
# Begin Source File

SOURCE=.\mvColorBandFilter.h
# End Source File
# Begin Source File

SOURCE=.\mvColorBar.h
# End Source File
# Begin Source File

SOURCE=.\mvColorTable.h
# End Source File
# Begin Source File

SOURCE=.\mvCroppableDisplayObject.h
# End Source File
# Begin Source File

SOURCE=.\mvCustomAppendPolyData.h
# End Source File
# Begin Source File

SOURCE=.\mvDataSource.h
# End Source File
# Begin Source File

SOURCE=.\mvDisplayObject.h
# End Source File
# Begin Source File

SOURCE=.\mvDisplayText.h
# End Source File
# Begin Source File

SOURCE=.\mvDxfReader.h
# End Source File
# Begin Source File

SOURCE=.\mvExternalMesh.h
# End Source File
# Begin Source File

SOURCE=.\mvExternalMeshVector.h
# End Source File
# Begin Source File

SOURCE=.\mvExtractGrid.h
# End Source File
# Begin Source File

SOURCE=.\mvFaceLess.h
# End Source File
# Begin Source File

SOURCE=.\mvGridLines.h
# End Source File
# Begin Source File

SOURCE=.\mvGridOutline.h
# End Source File
# Begin Source File

SOURCE=.\mvGridShell.h
# End Source File
# Begin Source File

SOURCE=.\mvGUISettings.h
# End Source File
# Begin Source File

SOURCE=.\mvHashTable.h
# End Source File
# Begin Source File

SOURCE=.\mvHeader.h
# End Source File
# Begin Source File

SOURCE=.\mvLinkList.h
# End Source File
# Begin Source File

SOURCE=.\mvLogColorTable.h
# End Source File
# Begin Source File

SOURCE=.\mvManager.h
# End Source File
# Begin Source File

SOURCE=.\mvMeshLines.h
# End Source File
# Begin Source File

SOURCE=.\mvModelFeatures.h
# End Source File
# Begin Source File

SOURCE=.\mvModelList.h
# End Source File
# Begin Source File

SOURCE=.\mvOverlay.h
# End Source File
# Begin Source File

SOURCE=.\mvParticles.h
# End Source File
# Begin Source File

SOURCE=.\mvPathlines.h
# End Source File
# Begin Source File

SOURCE=.\mvSutraFace.h
# End Source File
# Begin Source File

SOURCE=.\mvUtil.h
# End Source File
# Begin Source File

SOURCE=.\mvVRMLExporter.h
# End Source File
# Begin Source File

SOURCE=.\PhastDataSource.h
# End Source File
# Begin Source File

SOURCE=.\shapefil.h
# End Source File
# Begin Source File

SOURCE=.\SutraDataSource.h
# End Source File
# Begin Source File

SOURCE=.\UcodeDataSource.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
