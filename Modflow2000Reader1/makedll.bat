del mf2k_r1.dll
del mf2k_r1.lib
LF90 -win -dll -ml msvc -i ..\Modflow2000Reader -out mf2k_r1.dll @SFiles.txt > lfresult
copy mf2k_r1.dll ..\ModelViewer\Debug\mf2k_r1.dll
copy mf2k_r1.dll ..\ModelViewer\Release\mf2k_r1.dll
