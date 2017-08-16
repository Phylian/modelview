# Microsoft Developer Studio Project File - Name="Modflow96Reader" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Modflow96Reader - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Modflow96Reader.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Modflow96Reader.mak" CFG="Modflow96Reader - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Modflow96Reader - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Modflow96Reader - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "Modflow96Reader"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Modflow96Reader - Win32 Release"

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
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MODFLOW96READER_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MODFLOW96READER_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:"Release/mf96_r.dll"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy .\Release\mf96_r.dll ..\ModelViewer\Release\mf96_r.dll
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Modflow96Reader - Win32 Debug"

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
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MODFLOW96READER_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MODFLOW96READER_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /profile /debug /machine:I386 /out:"Debug/mf96_r.dll"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy .\Debug\mf96_r.dll ..\ModelViewer\Debug\mf96_r.dll
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "Modflow96Reader - Win32 Release"
# Name "Modflow96Reader - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\BAS5.FOR
# End Source File
# Begin Source File

SOURCE=.\BCF5.FOR
# End Source File
# Begin Source File

SOURCE=.\CHD1.FOR
# End Source File
# Begin Source File

SOURCE=.\DRN5.FOR
# End Source File
# Begin Source File

SOURCE=.\FHB1.FOR
# End Source File
# Begin Source File

SOURCE=.\GHB5.FOR
# End Source File
# Begin Source File

SOURCE=.\IBS1.FOR
# End Source File
# Begin Source File

SOURCE=.\mc3CustomRead.f
# End Source File
# Begin Source File

SOURCE=.\Mf96common.f
DEP_F90_MF96C=\
	".\Debug\mf96module.mod"\
	".\Debug\simtime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Mf96Module.f

!IF  "$(CFG)" == "Modflow96Reader - Win32 Release"

!ELSEIF  "$(CFG)" == "Modflow96Reader - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"mf96module"

# End Source File
# Begin Source File

SOURCE=.\MfBasic.f

!IF  "$(CFG)" == "Modflow96Reader - Win32 Release"

!ELSEIF  "$(CFG)" == "Modflow96Reader - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"simtime" \
	"timepointlist"

# End Source File
# Begin Source File

SOURCE=.\Modflow96Reader.f
DEP_F90_MODFL=\
	".\Debug\mf96module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\RES1.FOR
# End Source File
# Begin Source File

SOURCE=.\RIV5.FOR
# End Source File
# Begin Source File

SOURCE=.\STR1.FOR
# End Source File
# Begin Source File

SOURCE=.\UTL5.FOR
# End Source File
# Begin Source File

SOURCE=.\WEL5.FOR
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
