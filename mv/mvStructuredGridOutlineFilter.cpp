#include "mvStructuredGridOutlineFilter.h"

/*
 * Overrides the superclass to handle 2D cases
 */
void mvStructuredGridOutlineFilter::Execute()
{
  vtkStructuredGrid *input=this->GetInput();
  int *ext, xInc, yInc, zInc;
  vtkPoints *inPts;
  vtkPoints *newPts;
  vtkCellArray *newLines;
  vtkPolyData *output=this->GetOutput();
  int numPts, idx, offset, ids[2];
  // for marching through the points along an edge.
  int start, num, inc, id;

  for ( int i = 0; i < 12; i++ )
    {
    this->ComputeDivisionExtents( output, i, 12 );

    if ( i == 0 )
      {
      this->StreamExecuteStart();
      }

    if (this->ExecutePiece >= 12)
      {
      // thing should not have gotten this far.
      return;
      }

    // If all StructuredPointsSources were forced to give you exactly
    // the update extent, this execute method would be trivial.
    // However, Imaging does not have this requirement, and I have not
    // made the readers "StreamingReady" ...
  
    // Find the start of this edge, the length of this edge, and increment.
    ext = input->GetExtent();
    xInc = 1;
    yInc = ext[1]-ext[0]+1;
    zInc = yInc * (ext[3]-ext[2]+1);
    switch (this->ExecutePiece)
      {
      case 0:
	// start (0, 0, 0) increment z axis.
	num = ext[5]-ext[4]+1;
	start = (0-ext[0])*xInc + (0-ext[2])*yInc + (0-ext[4])*zInc;
	inc = zInc;
	break;
      case 1:
	// start (xMax, 0, 0) increment z axis.
	num = ext[5]-ext[4]+1;
	start = (ext[1]-ext[0])*xInc + (0-ext[2])*yInc + (0-ext[4])*zInc;
	inc = zInc;
	break;
      case 2:
	// start (0, yMax, 0) increment z axis.
	num = ext[5]-ext[4]+1;
	start = (0-ext[0])*xInc + (ext[3]-ext[2])*yInc + (0-ext[4])*zInc;
	inc = zInc;
	break;
      case 3:
	// start (xMax, yMax, 0) increment z axis.
	num = ext[5]-ext[4]+1;
	start = (ext[1]-ext[0])*xInc + (ext[3]-ext[2])*yInc + (0-ext[4])*zInc;
	inc = zInc;
	break;
      case 4:
	// start (0, 0, 0) increment y axis.
	num = ext[3]-ext[2]+1;
	start = (0-ext[0])*xInc + (0-ext[2])*yInc + (0-ext[4])*zInc;
	inc = yInc;
	break;
      case 5:
	// start (xMax, 0, 0) increment y axis.
	num = ext[3]-ext[2]+1;
	start = (ext[1]-ext[0])*xInc + (0-ext[2])*yInc + (0-ext[4])*zInc;
	inc = yInc;
	break;
      case 6:
	// start (0, 0, zMax) increment y axis.
	num = ext[3]-ext[2]+1;
	start = (0-ext[0])*xInc + (0-ext[2])*yInc + (ext[5]-ext[4])*zInc;
	inc = yInc;
	break;
      case 7:
	// start (xMax, 0, zMax) increment y axis.
	num = ext[3]-ext[2]+1;
	start = (ext[1]-ext[0])*xInc + (0-ext[2])*yInc + (ext[5]-ext[4])*zInc;
	inc = yInc;
	break;
      case 8:
	// start (0, 0, 0) increment x axis.
	num = ext[1]-ext[0]+1;
	start = (0-ext[0])*xInc + (0-ext[2])*yInc + (0-ext[4])*zInc;
	inc = xInc;
	break;
      case 9:
	// start (0, yMax, 0) increment x axis.
	num = ext[1]-ext[0]+1;
	start = (0-ext[0])*xInc + (ext[3]-ext[2])*yInc + (0-ext[4])*zInc;
	inc = xInc;
	break;
      case 10:
	// start (0, 0, zMax) increment x axis.
	num = ext[1]-ext[0]+1;
	start = (0-ext[0])*xInc + (0-ext[2])*yInc + (ext[5]-ext[4])*zInc;
	inc = xInc;
	break;
      case 11:
	// start (0, yMax, zMax) increment x axis.
	num = ext[1]-ext[0]+1;
	start = (0-ext[0])*xInc + (ext[3]-ext[2])*yInc + (ext[5]-ext[4])*zInc;
	inc = xInc;
	break;
      }
    
	// Modification: the rest of the method is moved inside the if block
    if (num > 1)
      {

		// these already created in StreamExecuteStart
		newPts = output->GetPoints();
		newLines = output->GetLines();
		offset = newPts->GetNumberOfPoints();
		inPts = input->GetPoints();
		numPts = inPts->GetNumberOfPoints();

		// add points
		for (idx = 0; idx < num; ++idx)
		  {
		  id = start + idx * inc;
		  // sanity check
		  if (id < 0 || id >= numPts)
		{
		vtkErrorMacro("Error stepping through points.");
		return;
		}
		  newPts->InsertNextPoint(inPts->GetPoint(id));
		  }

		// add lines
		for (idx = 1; idx < num; ++idx)
		  {
		  ids[0] = idx+offset-1;
		  ids[1] = idx+offset;
		  newLines->InsertNextCell(2, ids);
		  }
		}
	}
}

