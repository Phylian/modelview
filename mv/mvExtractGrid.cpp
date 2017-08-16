#include "mvExtractGrid.h"
#include "vtkCellData.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkStructuredGrid.h"


mvExtractGrid::mvExtractGrid()
{

}

mvExtractGrid::~mvExtractGrid()
{

}


void mvExtractGrid::Execute()
{
  vtkStructuredGrid *input= this->GetInput();
  vtkPointData *pd=input->GetPointData();
  vtkCellData *cd=input->GetCellData();
  vtkStructuredGrid *output= this->GetOutput();
  vtkPointData *outPD=output->GetPointData();
  vtkCellData *outCD=output->GetCellData();
  int i, j, k, uExt[6], voi[6];
  int *inExt;
  int *inWholeExt;
  int iIn, jIn, kIn;
  int outSize, jOffset, kOffset, rate[3];
  vtkIdType idx, newIdx, newCellId;
  vtkPoints *newPts, *inPts;
  int inInc1, inInc2;
  // Function to convert output index to input index f(i) = Rate*I + shift
  int shift[3];

  vtkDebugMacro(<< "Extracting Grid");

  inPts = input->GetPoints();

  output->GetUpdateExtent(uExt);
  inExt = input->GetExtent();
  inInc1 = (inExt[1]-inExt[0]+1);
  inInc2 = inInc1*(inExt[3]-inExt[2]+1);

  for (i = 0; i < 3; ++i)
    {
    if ( (rate[i] = this->SampleRate[i]) < 1 )
      {
      rate[i] = 1;
      }
    }

  // Clip the VOI by the input whole extent
  inWholeExt = input->GetWholeExtent();
  for (i = 0; i < 3; ++i)
    {
    voi[i*2] = this->VOI[2*i];
    if (voi[2*i] < inWholeExt[2*i])
      {
      voi[2*i] = inWholeExt[2*i];
      }
    voi[i*2+1] = this->VOI[2*i+1];
    if (voi[2*i+1] > inWholeExt[2*i+1])
      {
      voi[2*i+1] = inWholeExt[2*i+1];
      }
    }

  // Compute the shift.
  // The shift is necessary because the starting VOI may not be on stride boundary.
  // We need to duplicate the computation done in 
  // ExecuteInformtation for the output whole extent.
  // Use shift as temporary variable (output mins).
  shift[0] = (int)(floor( (float)(voi[0])/(float)(rate[0]) ));
  shift[1] = (int)(floor( (float)(voi[2])/(float)(rate[1]) ));
  shift[2] = (int)(floor( (float)(voi[4])/(float)(rate[2]) ));
  // Take the different between the output and input mins (in input coordinates).
  shift[0] = voi[0] - (shift[0]*rate[0]);
  shift[1] = voi[2] - (shift[1]*rate[1]);
  shift[2] = voi[4] - (shift[2]*rate[2]);


  output->SetExtent(uExt);

  // If output same as input, just pass data through
  if ( uExt[0] <= inExt[0] && uExt[1] >= inExt[1] &&
       uExt[2] <= inExt[2] && uExt[3] >= inExt[3] &&
       uExt[4] <= inExt[4] && uExt[5] >= inExt[5] &&
       rate[0] == 1 && rate[1] == 1 && rate[2] == 1)
    { 
    output->SetPoints(inPts);
    output->GetPointData()->PassData(input->GetPointData());
    output->GetCellData()->PassData(input->GetCellData());
    vtkDebugMacro(<<"Passed data through bacause input and output are the same");
    return;
    }

  // Allocate necessary objects
  //
  outSize = (uExt[1]-uExt[0]+1)*(uExt[3]-uExt[2]+1)*(uExt[5]-uExt[4]+1);
  newPts = (vtkPoints *) inPts->MakeObject(); 
  newPts->SetNumberOfPoints(outSize);
  outPD->CopyAllocate(pd,outSize,outSize);
  outCD->CopyAllocate(cd,outSize,outSize);

  // Traverse input data and copy point attributes to output
  // iIn,jIn,kIn are in input grid coordinates.
  newIdx = 0;
  for ( k=uExt[4]; k <= uExt[5]; ++k)
    { // Convert out coords to in coords.
    kIn = shift[2] + (k*rate[2]);
    if (kIn > voi[5])
      { // This handles the IncludeBoundaryOn condition.
      kIn = voi[5];
      }
    kOffset = (kIn-inExt[4]) * inInc2;
    for ( j=uExt[2]; j <= uExt[3]; ++j)
      { // Convert out coords to in coords.
      jIn = shift[1] + (j*rate[1]);
      if (jIn > voi[3])
        { // This handles the IncludeBoundaryOn condition.
        jIn = voi[3];
        }
      jOffset = (jIn-inExt[2]) * inInc1;
      for ( i=uExt[0]; i <= uExt[1]; ++i)
        { // Convert out coords to in coords.
        iIn = shift[0] + (i*rate[0]);
        if (iIn > voi[1])
          { // This handles the IncludeBoundaryOn condition.
          iIn = voi[1];
          }
        idx = (iIn-inExt[0]) + jOffset + kOffset;
        newPts->SetPoint(newIdx,inPts->GetPoint(idx));
        outPD->CopyData(pd, idx, newIdx++);

        }
      }
    }

  // Traverse input data and copy cell attributes to output
  //
  newCellId = 0;
  inInc1 = (inExt[1]-inExt[0]);
  inInc2 = inInc1*(inExt[3]-inExt[2]);
  // This will take care of 2D and 1D cells.
  // Each loop has to excute at least once.
  if (uExt[4] == uExt[5])
    {
    uExt[5] = uExt[5] + 1;
    }
  if (uExt[2] == uExt[3])
    {
    uExt[3] = uExt[3] + 1;
    }
  if (uExt[0] == uExt[1])
    {
    uExt[1] = uExt[1] + 1;
    }
  // No need to consider IncludeBoundary for cell data.
  for ( k=uExt[4]; k < uExt[5]; ++k )
    { // Convert out coords to in coords.
    kIn = shift[2] + (k*rate[2]);
    kOffset = (kIn-inExt[4]) * inInc2;
    for ( j=uExt[2]; j < uExt[3]; ++j )
      { // Convert out coords to in coords.
      jIn = shift[1] + (j*rate[1]);
      jOffset = (jIn-inExt[2]) * inInc1;
      for ( i=uExt[0]; i < uExt[1]; ++i )
        {
        iIn = shift[0] + (i*rate[0]);
        idx = (iIn-inExt[0]) + jOffset + kOffset;
        outCD->CopyData(cd, idx, newCellId++);
        }
      }
    }

  output->SetPoints(newPts);
  newPts->Delete();
}
