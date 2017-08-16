#ifndef __Modflow2000Reader2_h
#define __Modflow2000Reader2_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL mf2krewindparticlefile2_(int *ierror);

extern void STDCALL mf2kreadparticlecount2_(int *NP, int *ierror, int *istep);

extern void STDCALL mf2kreadparticles2_(int *NP, int *ierror, float *coord, float *scalars, float *delr, float *delc, float *elev);

extern void STDCALL mf2kdims2_(int *ierror, int *ncol, int *nrow, int *nlay, int *gwt, int *unstruct, int *timeunit, char *namefile, int len1);

extern void STDCALL mf2kgrid2_(int *ierror, float *delr, float *delc, float *elev, int *ibound, float *conductivity, float *hnoflo, float *hdry, int *sunit, int *vunit, float *xoffset, float *yoffset, int *isMfLayer);

extern void STDCALL mf2kcountscalars2_(int *ierror, int *iunit, int *ndatasets, char *dataType, int len);

extern void STDCALL mf2kcountvectors2_(int *ierror, int *iunit);

extern void STDCALL mf2kcountfeatures2_(int *ierror, int *ibousz, int *nfeat, int *ibound);

extern void STDCALL mf2kgettimepoints2_(float *timepoints, int *periods, int *steps, int *moves, int *numtimepoints);

extern void STDCALL mf2kgetscalars2_(int *ierror, int *iunit, float *array, int *istep, int *kper);

extern void STDCALL mf2kgetvectors2_(int *ierror, int *iunit, float *array, int *istep);

extern void STDCALL mf2kgetfeatures2_(int *ierror, int *ibnode);

extern void STDCALL mf2kcleanup2_();

#ifdef __cplusplus
}
#endif
#endif
