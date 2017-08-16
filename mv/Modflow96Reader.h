#ifndef __Modflow96Reader_h
#define __Modflow96Reader_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL mf96rewindparticlefile0_(int *ierror);

extern void STDCALL mf96readparticlecount0_(int *NP,int *ierror, int *istep);

extern void STDCALL mf96readparticles0_(int *NP, int *ierror, float *coord, float *scalars, float *delr, float *delc, float *elev);

extern void STDCALL mf96dims0_(int *ierror, int *ncol, int *nrow, int *nlay, int *moc, int *unstruct, int *timeunit, char *namefile, char *elevfile, int len1, int len2);

extern void STDCALL mf96grid0_(int *ierror, float *delr, float *delc, float *elev, int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer);

extern void STDCALL mf96countscalars0_(int *ierror, int *iunit, int *ndatasets, char *dataType, int len);

extern void STDCALL mf96countvectors0_(int *ierror, int *iunit);

extern void STDCALL mf96countfeatures0_(int *ierror, int *ibousz, int *nfeat);

extern void STDCALL mf96gettimepoints0_(float *timepoints, int *periods, int *steps, int *moves, int *numtimepoints);

extern void STDCALL mf96getscalars0_(int *ierror, int *iunit, float *array, int *istep, int *kper);

extern void STDCALL mf96getvectors0_(int *ierror, int *iunit, float *array, int *istep);

extern void STDCALL mf96getfeatures0_(int *ierror, int *ibnode);

extern void STDCALL mf96cleanup0_();

#ifdef __cplusplus
}
#endif
#endif
