#ifndef __Modflow96Reader1_h
#define __Modflow96Reader1_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL mf96rewindparticlefile1(int *ierror);

extern void STDCALL mf96readparticlecount1(int *NP, int *ierror, int *istep);

extern void STDCALL mf96readparticles1(int *NP,int *ierror, float *coord, float *scalars, float *delr, float *delc, float *elev);

extern void STDCALL mf96dims1(int *ierror, int *ncol, int *nrow, int *nlay, int *moc, int *unstruct, int *timeunit, char *namefile, char *elevfile, int len1, int len2);

extern void STDCALL mf96grid1(int *ierror, float *delr, float *delc, float *elev, int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer);

extern void STDCALL mf96countscalars1(int *ierror, int *iunit, int *ndatasets, char *dataType, int len);

extern void STDCALL mf96countvectors1(int *ierror, int *iunit);

extern void STDCALL mf96countfeatures1(int *ierror, int *ibousz, int *nfeat);

extern void STDCALL mf96gettimepoints1(float *timepoints, int *periods, int *steps, int *moves, int *numtimepoints);

extern void STDCALL mf96getscalars1(int *ierror, int *iunit, float *array, int *istep, int *kper);

extern void STDCALL mf96getvectors1(int *ierror, int *iunit, float *array, int *istep);

extern void STDCALL mf96getfeatures1(int *ierror, int *ibnode);

extern void STDCALL mf96cleanup1();


#ifdef __cplusplus
}
#endif
#endif
