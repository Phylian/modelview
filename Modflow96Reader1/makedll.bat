del mf96_r1.dll
del mf96_r1.lib
lf90 -win -dll -ml msvc -out mf96_r1.dll @SFiles.txt > lfresult
copy mf96_r1.dll ..\ModelViewer\Debug\mf96_r1.dll
copy mf96_r1.dll ..\ModelViewer\Release\mf96_r1.dll
