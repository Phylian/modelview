#ifndef __DataFilesDialog_h
#define __DataFilesDialog_h

class DataFilesDialog
{
public:
	static char *GetDataFileList(char *model);

private:
	static char *GetModflow2000DataFiles();
	static char *GetMf2kgwtDataFiles();
	static char *GetModflow96DataFiles();
	static char *GetMoc3dDataFiles();
	static char *GetMt3dmsDataFiles();
	static char *GetSutraDataFiles();
	static int SutraConvert(const char *nod, const char *ele, const char *inp, const char *bin, int &TimeUnits);
	static char *GetPhastDataFiles();
	static char *GetUcodeSosFiles();
};

#endif
