#ifndef __SutraReader21_h
#define __SutraReader21_h
#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

extern void STDCALL sutra_21_(int *ierror, int *istart, int *ibousz, int *ibnode, 
  int *nfeatures, int *ISTEADYFLOW, int *ISTEADYTRANSPORT, 
  float *ElementValues, int *IElementValueCount, int *INCIDENCE, 
  float *NodeValues, int *INodeValueCount, int *MeshInfo, char *inpfile, int len);

extern void STDCALL sutra_labels21_(char *flabels, int len);

#ifdef __cplusplus
}
#endif
#endif
