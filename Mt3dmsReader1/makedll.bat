del mt_r1.dll
del mt_r1.lib
lf90 -win -dll -ml msvc -out mt_r1.dll ..\Mt3dmsReader\Mt3dmsCommon.f Mt3dmsReader1.f
copy mt_r1.dll ..\ModelViewer\Debug\mt_r1.dll
copy mt_r1.dll ..\ModelViewer\Release\mt_r1.dll
