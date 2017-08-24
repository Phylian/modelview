/**
    Alegría 2D Game Engine Copyright (C) 2010 J.G. Camarasa <pepius@gmail.com>
    
    This file is part of Alegría.
    
    Alegría is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef APY_MouseCompProxy_hpp
#define APY_MouseCompProxy_hpp

#include "Python.h"
#include "APY_Utils.hpp"
#include "..\components\AC_Mouse.hpp"


struct APY_MouseCompProxy{
    PyObject_HEAD
    AC_Mouse *ref;
};


static PyObject* _apy_IsLButtonDown(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_IsMButtonDown(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_IsRButtonDown(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetMousePos(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetMouseWindowPos(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetMouseWindowPos(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetMouseWindowIncrement(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetMouseIncrement(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_ShowMouse(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_LimitMouse(PyObject *self, PyObject *args, PyObject *kwds);

_APY_METH_START
    _APY_METHOD("IsLButtonDown", _apy_IsLButtonDown, "Returns if the left button is pressed")
    _APY_METHOD("IsMButtonDown", _apy_IsMButtonDown, "Returns if the middle button is pressed")
    _APY_METHOD("IsRButtonDown", _apy_IsRButtonDown, "Returns if the right button is pressed")
    _APY_METHOD("GetMousePos", _apy_GetMousePos, "Returns the mouse position in space coordinates")
    _APY_METHOD("GetMouseWindowPos", _apy_GetMouseWindowPos, "Returns the mouse position in window coordinates")
    _APY_METHOD("SetMouseWindowPos", _apy_SetMouseWindowPos, "Sets the mouse position in window coordinates")
    _APY_METHOD("GetMouseWindowIncrement", _apy_GetMouseWindowIncrement, "Returns the mouse increment in window coordinates")
    _APY_METHOD("GetMouseIncrement", _apy_GetMouseIncrement, "Returns the mouse increment in space coordinates")
    _APY_METHOD("ShowMouse", _apy_ShowMouse, "Sets the visibility of the cursor")
    _APY_METHOD("LimitMouse", _apy_LimitMouse, "Sets the visibility of the cursor")
_APY_METH_END

_APY_TYPE(APY_MouseCompProxy, "AC_Mouse")

PyObject* _apy_IsLButtonDown(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("i", ((APY_MouseCompProxy*)self)->ref->IsLButtonDown());
}

PyObject* _apy_IsMButtonDown(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("i", ((APY_MouseCompProxy*)self)->ref->IsMButtonDown());
}

PyObject* _apy_IsRButtonDown(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("i", ((APY_MouseCompProxy*)self)->ref->IsRButtonDown());
}

PyObject* _apy_GetMousePos(PyObject *self, PyObject *args, PyObject *kwds){
    Float2d pos = ((APY_MouseCompProxy*)self)->ref->GetMousePosS();

    return Py_BuildValue("(f,f)", pos.x, pos.y );
}

PyObject* _apy_GetMouseWindowPos(PyObject *self, PyObject *args, PyObject *kwds){
    Int2d pos = ((APY_MouseCompProxy*)self)->ref->GetMousePos();

    return Py_BuildValue("(i,i)", pos.x, pos.y );
}

PyObject* _apy_SetMouseWindowPos(PyObject *self, PyObject *args, PyObject *kwds){
    int x,y;
    PyArg_ParseTuple(args,"ii",&x,&y);

    ((APY_MouseCompProxy*)self)->ref->SetMousePos(x,y);

    Py_RETURN_NONE;
}

PyObject* _apy_GetMouseIncrement(PyObject *self, PyObject *args, PyObject *kwds){
    Float2d pos = ((APY_MouseCompProxy*)self)->ref->GetMouseIncrementS();

    return Py_BuildValue("(f,f)", pos.x, pos.y );
}

PyObject* _apy_GetMouseWindowIncrement(PyObject *self, PyObject *args, PyObject *kwds){
    Int2d pos = ((APY_MouseCompProxy*)self)->ref->GetMouseIncrement();

    return Py_BuildValue("(i,i)", pos.x, pos.y );
}

PyObject* _apy_ShowMouse(PyObject *self, PyObject *args, PyObject *kwds){
    int show;
    PyArg_ParseTuple(args,"i",&show);

    ((APY_MouseCompProxy*)self)->ref->ShowMouse(show);

    Py_RETURN_NONE;
}

PyObject* _apy_LimitMouse(PyObject *self, PyObject *args, PyObject *kwds){
    int flag;
    PyArg_ParseTuple(args,"i",&flag);

    ((APY_MouseCompProxy*)self)->ref->LimitMouse(flag);

    Py_RETURN_NONE;
}

#endif 