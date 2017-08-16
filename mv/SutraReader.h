#ifndef __SutraReader_h
#define __SutraReader_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL sutra_2d3d_(int *ierror, int *istart, int *ibousz, int *ibnode, 
  int *nfeatures, int *ISTEADYFLOW, int *ISTEADYTRANSPORT, char *inpfile, int len);

extern void STDCALL sutra_labels_(char *flabels, int len);

#ifdef __cplusplus
}
#endif
#endif
