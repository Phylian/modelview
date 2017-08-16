// comment out any of the following definitions to
// exclude supporting the corresponding model
#define MV_MODFLOW2000
#define MV_MODFLOW2000_GWT
#define MV_MODFLOW96
#define MV_MOC3D
#define MV_MT3DMS
#define MV_SUTRA
#define MV_PHAST
#define MV_UCODE_SOS

#include "mvModelList.h"

#ifdef MV_MODFLOW2000
	#include "Modflow2000DataSource.h"
#endif

#ifdef MV_MODFLOW2000_GWT
	#include "Mf2kgwtDataSource.h"
#endif

#ifdef MV_MODFLOW96
	#include "Modflow96DataSource.h"
#endif

#ifdef MV_MOC3D
	#include "Moc3dDataSource.h"
#endif

#ifdef MV_MT3DMS
	#include "Mt3dmsDataSource.h"
#endif

#ifdef MV_SUTRA
	#include "SutraDataSource.h"
#endif

#ifdef MV_PHAST
	#include "PhastDataSource.h"
#endif

#ifdef MV_UCODE_SOS
	#include "UcodeDataSource.h"
#endif

#include <string.h>

int mvModelList::GetNumberOfModels()
{
	int n = 0;
	#ifdef MV_MODFLOW2000
		n++;
	#endif

	#ifdef MV_MODFLOW2000_GWT
		n++;
	#endif

	#ifdef MV_MODFLOW96
		n++;
	#endif

	#ifdef MV_MOC3D
		n++;
	#endif

	#ifdef MV_MT3DMS
		n++;
	#endif

	#ifdef MV_SUTRA
		n++;
	#endif
		
	#ifdef MV_PHAST
		n++;
	#endif

	#ifdef MV_UCODE_SOS
		n++;
	#endif

	return n;
}

void mvModelList::GetModelNames(char **buffer)
{
	int n = 0;

	#ifdef MV_MODFLOW2000
		strcpy(buffer[n], Modflow2000DataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_MODFLOW2000_GWT
		strcpy(buffer[n], Mf2kgwtDataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_MODFLOW96
		strcpy(buffer[n], Modflow96DataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_MOC3D
		strcpy(buffer[n], Moc3dDataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_MT3DMS
		strcpy(buffer[n], Mt3dmsDataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_SUTRA
		strcpy(buffer[n], SutraDataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_PHAST
		strcpy(buffer[n], PhastDataSource::GetNameStatic());
		n++;
	#endif

	#ifdef MV_UCODE_SOS
		strcpy(buffer[n], UcodeDataSource::GetNameStatic());
		n++;
	#endif
}

mvDataSource *mvModelList::CreateDataSource(char *model, float version)
{
	#ifdef MV_MODFLOW2000
		if ((stricmp(model, "Modflow 2000") == 0)||
			  (stricmp(model, Modflow2000DataSource::GetNameStatic()) == 0))
		{
			Modflow2000DataSource *mf2000 = new Modflow2000DataSource;
			if (version >= 1.6)
			{
				mf2000->m_UseExternalFile = 1;
			}
			return mf2000;
		}
	#endif

	#ifdef MV_MODFLOW2000_GWT
		if (stricmp(model, Mf2kgwtDataSource::GetNameStatic()) == 0)
		{
			return new Mf2kgwtDataSource;
		}
	#endif

	#ifdef MV_MODFLOW96
		if (stricmp(model, Modflow96DataSource::GetNameStatic()) == 0)
		{
			return new Modflow96DataSource;
		}
	#endif

	#ifdef MV_MOC3D
		if (stricmp(model, Moc3dDataSource::GetNameStatic()) == 0)
		{
			return new Moc3dDataSource;
		}
		// for backward compatibility
		if (stricmp(model, "Moc3d") == 0)
		{
			return new Moc3dDataSource;
		}
	#endif

	#ifdef MV_MT3DMS
		if (stricmp(model, Mt3dmsDataSource::GetNameStatic()) == 0)
		{
			return new Mt3dmsDataSource;
		}
	#endif

	#ifdef MV_SUTRA
		if (stricmp(model, SutraDataSource::GetNameStatic()) == 0)
		{
			return new SutraDataSource;
		}
		// for backward compatibility with version 1.0
		if (stricmp(model, "Sutra (Version 2D3D.1)") == 0)
		{
			return new SutraDataSource;
		}
		// for backward compatibility with version earlier than 1.0
		if (stricmp(model, "Sutra 2D/3D") == 0)
		{
			return new SutraDataSource;
		}
	#endif

	#ifdef MV_PHAST
		if (stricmp(model, PhastDataSource::GetNameStatic()) == 0)
		{
			return new PhastDataSource;
		}
	#endif

	#ifdef MV_UCODE_SOS
		if (stricmp(model, UcodeDataSource::GetNameStatic()) == 0)
		{
			return new UcodeDataSource;
		}
	#endif

	return 0;
}
