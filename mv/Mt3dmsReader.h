#ifndef __Mt3dmsReader_h
#define __Mt3dmsReader_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL mtdims0_(int *ierror, char *cnfFile, char *ucnFile, char *lmtFile, int *unstruct, int *nc, int *nr, int *nl, int len1, int len2, int len3);

extern void STDCALL mtgrid0_(int *ierror, float *delr, float *delc, float *z, float *cinact);

extern void STDCALL mtcountscalars0_(int *ierror, int *ndatasets, char *dataType, int len);

extern void STDCALL mtcountvectorsandfeatures0_(int *ierror, int *ibousz, int *nfeat);

extern void STDCALL mtgettimepoints0_(float *timepoints, int *periods, int *steps, int *moves, int *numtimepoints);

extern void STDCALL mtgetscalars0_(int *ierror, float *array, int *istep);

extern void STDCALL mtgetvectorsandfeatures0_(int *update, float *array, int *istep, int *ibnode);

extern void STDCALL mtcleanup0_();

#ifdef __cplusplus
}
#endif
#endif
