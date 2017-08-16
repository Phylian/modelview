#include "mvInteractorStyleTrackball.h"

void mvInteractorStyleTrackball::OnLeftButtonUp(int ctrl, int shift, int X, int Y) 
{
  // The next statement is moved to after release/end method is invoked
 //this->UpdateInternalState(ctrl, shift, X, Y);
  //
  if (this->LeftButtonReleaseMethod) 
    {
    (*this->LeftButtonReleaseMethod)(this->LeftButtonReleaseMethodArg);
    }
  else 
    {
    if (this->ShiftKey) 
      {
      if (this->CtrlKey) 
        {
        this->EndDolly();
        }
      else        
        {
        this->EndPan();
        }
      } 
    else 
      {
      if (this->CtrlKey) 
        {
        this->EndSpin();
        }
      else
        {
        this->EndRotate();
        }
      }
    }

  this->UpdateInternalState(ctrl, shift, X, Y);

  this->OldX = 0.0;
  this->OldY = 0.0;
  if (this->ActorMode && this->ActorPicked)
    {
    this->HighlightActor(this->InteractionActor);
    }
  else if (this->ActorMode)
    {
    this->HighlightActor(NULL);
    }
}

void mvInteractorStyleTrackball::OnRightButtonUp(int ctrl, int shift, 
                                                  int X, int Y) 
{
  // The next statement is moved to after release/end method is invoked
 //this->UpdateInternalState(ctrl, shift, X, Y);
  //
  if (this->RightButtonReleaseMethod) 
    {
    (*this->RightButtonReleaseMethod)(this->RightButtonReleaseMethodArg);
    }
  else 
    {
    if (this->ActorMode)
      {
      this->EndUniformScale();
      }
    else
      {
      this->EndZoom();
      }
    }
 this->UpdateInternalState(ctrl, shift, X, Y);
  this->OldX = 0.0;
  this->OldY = 0.0;
  if (this->ActorMode && this->ActorPicked)
    {
    this->HighlightActor(this->InteractionActor);
    }
  else if (this->ActorMode)
    {
    this->HighlightActor(NULL);
    }
}
