#ifndef __mvVRMLExporter_h
#define __mvVRMLExporter_h

#include "mvHeader.h"
#include "vtkVRMLExporter.h"

class MV_EXPORT mvVRMLExporter : public vtkVRMLExporter  
{
public:
	mvVRMLExporter();
	~mvVRMLExporter();
	static mvVRMLExporter *New() {return new mvVRMLExporter;};
	const char *GetClassName() {return "mvVRMLExporter";};
	void WriteData();
	void WriteAnActor(vtkActor *anActor, FILE *fp);
	void WritePointData(vtkPoints *points, vtkDataArray *normals, 
                      vtkDataArray *tcoords, vtkUnsignedCharArray *colors, 
                      FILE *fp);
};

#endif
