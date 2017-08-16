# Microsoft Developer Studio Project File - Name="Modflow2000Reader" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Modflow2000Reader - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Modflow2000Reader.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Modflow2000Reader.mak" CFG="Modflow2000Reader - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Modflow2000Reader - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Modflow2000Reader - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "Modflow2000Reader"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Modflow2000Reader - Win32 Release"

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
# ADD F90 /assume:underscore /compile_only /iface:nomixed_str_len_arg /include:"Release/" /dll /names:lowercase /nologo /warn:nofileopt
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MODFLOW2000READER_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /out:"Release/mf2k_r.dll"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy .\Release\mf2k_r.dll ..\ModelViewer\Release\mf2k_r.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Modflow2000Reader - Win32 Debug"

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
# ADD F90 /assume:underscore /check:bounds /compile_only /debug:full /iface:nomixed_str_len_arg /include:"Debug/" /dll /names:lowercase /nologo /traceback /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /browser
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MODFLOW2000READER_EXPORTS" /Fr /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /profile /debug /machine:I386 /out:"Debug/mf2k_r.dll"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy .\Debug\mf2k_r.dll ..\ModelViewer\Debug\mf2k_r.dll
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "Modflow2000Reader - Win32 Release"
# Name "Modflow2000Reader - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\daf1.f
DEP_F90_DAF1_=\
	".\ground.com"\
	".\params.inc"\
	".\Release\mf2kmodule.mod"\
	".\startdaf.com"\
	
# End Source File
# Begin Source File

SOURCE=.\FHB1.FOR
# End Source File
# Begin Source File

SOURCE=.\glo1bas6.f
# End Source File
# Begin Source File

SOURCE=.\gutsdaf.f
DEP_F90_GUTSD=\
	".\params.inc"\
	".\startdaf.com"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1bas6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1bcf6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1chd6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1drn6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1drt1.f
DEP_F90_GWF1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1ghb6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1huf2.f
DEP_F90_GWF1H=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1ibs6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1lpf1.f
DEP_F90_GWF1L=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1mnw1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1riv6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1sfr1.f
DEP_F90_GWF1S=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\GWF1STR6.F
# End Source File
# Begin Source File

SOURCE=.\gwf1sub1.f

!IF  "$(CFG)" == "Modflow2000Reader - Win32 Release"

!ELSEIF  "$(CFG)" == "Modflow2000Reader - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"SUBARRAYS"

# End Source File
# Begin Source File

SOURCE=.\gwf1wel6.f
# End Source File
# Begin Source File

SOURCE=.\gwtCustomRead.f
# End Source File
# Begin Source File

SOURCE=.\hufutl2.f
DEP_F90_HUFUT=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\lak.f
# End Source File
# Begin Source File

SOURCE=..\Modflow96Reader\MfBasic.f

!IF  "$(CFG)" == "Modflow2000Reader - Win32 Release"

!ELSEIF  "$(CFG)" == "Modflow2000Reader - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"mfcommonmodule" \
	"simtime" \
	"timepointlist"

# End Source File
# Begin Source File

SOURCE=.\Modflow2000Common.f
DEP_F90_MODFL=\
	".\ground.com"\
	".\param.inc"\
	".\params.inc"\
	".\Release\mf2kmodule.mod"\
	".\Release\mfcommonmodule.mod"\
	".\Release\simtime.mod"\
	".\startdaf.com"\
	
# End Source File
# Begin Source File

SOURCE=.\Modflow2000Module.f

!IF  "$(CFG)" == "Modflow2000Reader - Win32 Release"

!ELSEIF  "$(CFG)" == "Modflow2000Reader - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"mf2kmodule"

# End Source File
# Begin Source File

SOURCE=.\Modflow2000Reader.f
DEP_F90_MODFLO=\
	".\Release\mf2kmodule.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\parutl1.f
DEP_F90_PARUT=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\RES1.FOR
# End Source File
# Begin Source File

SOURCE=.\sen.f
DEP_F90_SEN_F=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\utl6.f
DEP_F90_UTL6_=\
	".\actdata.inc"\
	".\param.inc"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
