#ifndef __Mt3dmsReader1_h
#define __Mt3dmsReader1_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL mtdims1(int *ierror, char *cnfFile, char *ucnFile, char *lmtFile, int *unstruct, int *nc, int *nr, int *nl, int len1, int len2, int len3);

extern void STDCALL mtgrid1(int *ierror, float *delr, float *delc, float *z, float *cinact);

extern void STDCALL mtcountscalars1(int *ierror, int *ndatasets, char *dataType, int len);

extern void STDCALL mtcountvectorsandfeatures1(int *ierror, int *ibousz, int *nfeat);

extern void STDCALL mtgettimepoints1(float *timepoints, int *periods, int *steps, int *moves, int *numtimepoints);

extern void STDCALL mtgetscalars1(int *ierror, float *array, int *istep);

extern void STDCALL mtgetvectorsandfeatures1(int *update, float *array, int *istep, int *ibnode);

extern void STDCALL mtcleanup1();

#ifdef __cplusplus
}
#endif
#endif
