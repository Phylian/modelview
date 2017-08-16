#ifndef __SutraReader2_h
#define __SutraReader2_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL sutra_2d3d2_(int *ierror, int *istart, int *ibousz, int *ibnode, 
  int *nfeatures, int *ISTEADYFLOW, int *ISTEADYTRANSPORT, 
  float *ElementValues, int *IElementValueCount, int *INCIDENCE, 
  float *NodeValues, int *INodeValueCount, char *inpfile, int len);

extern void STDCALL sutra_labels2_(char *flabels, int len);

#ifdef __cplusplus
}
#endif
#endif
