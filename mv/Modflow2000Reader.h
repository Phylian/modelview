#ifndef __Modflow2000Reader_h
#define __Modflow2000Reader_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL mf2krewindparticlefile0_(int *ierror);

extern void STDCALL mf2kreadparticlecount0_(int *NP, int *ierror, int *istep);

extern void STDCALL mf2kreadparticles0_(int *NP, int *ierror, float *coord, float *scalars, float *delr, float *delc, float *elev);

extern void STDCALL mf2kdims0_(int *ierror, int *ncol, int *nrow, int *nlay, int *gwt, int *unstruct, int *timeunit, char *namefile, int len1);

extern void STDCALL mf2kgrid0_(int *ierror, float *delr, float *delc, float *elev, int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer);

extern void STDCALL mf2kcountscalars0_(int *ierror, int *iunit, int *ndatasets, char *dataType, int len);

extern void STDCALL mf2kcountvectors0_(int *ierror, int *iunit);

extern void STDCALL mf2kcountfeatures0_(int *ierror, int *ibousz, int *nfeat, int *ibound);

extern void STDCALL mf2kgettimepoints0_(float *timepoints, int *periods, int *steps, int *moves, int *numtimepoints);

extern void STDCALL mf2kgetscalars0_(int *ierror, int *iunit, float *array, int *istep, int *kper);

extern void STDCALL mf2kgetvectors0_(int *ierror, int *iunit, float *array, int *istep);

extern void STDCALL mf2kgetfeatures0_(int *ierror, int *ibnode);

extern void STDCALL mf2kcleanup0_();

#ifdef __cplusplus
}
#endif
#endif
