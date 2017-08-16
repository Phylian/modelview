#include "mvCustomAppendPolyData.h"
#include "vtkFloatArray.h"

mvCustomAppendPolyData::mvCustomAppendPolyData()
{

}

mvCustomAppendPolyData::~mvCustomAppendPolyData()
{

}

void mvCustomAppendPolyData::PrintSelf(ostream& os, vtkIndent indent)
{
	vtkAppendPolyData::PrintSelf(os,indent);
}

void mvCustomAppendPolyData::Execute()
{
	// Check each input data set. If it is empty (contains no points)
	// add empty scalar data (for points and cells) so that vtkClipPolyData
	// will pass through scalar data.

	int idx;
	vtkPolyData *ds;

	for (idx = 0; idx < this->NumberOfInputs; ++idx)
	{
		ds = (vtkPolyData *)(this->Inputs[idx]);
		if (ds != 0 && ds->GetNumberOfPoints() == 0)
		{
			vtkFloatArray *scalars = vtkFloatArray::New();
			scalars->SetNumberOfComponents(1);
			ds->GetPointData()->SetScalars(scalars);
			scalars->Delete();
			scalars = vtkFloatArray::New();
			ds->GetCellData()->SetScalars(scalars);
			scalars->Delete();
		}
	}

	// Call the method in the base class
	vtkAppendPolyData::Execute();
}
