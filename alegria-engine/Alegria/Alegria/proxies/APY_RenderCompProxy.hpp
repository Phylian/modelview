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

#ifndef APY_RenderCompProxy_hpp
#define APY_RenderCompProxy_hpp

#include "Python.h"

#include "APY_Utils.hpp"
#include "..\components\AC_Render.hpp"


struct APY_RenderCompProxy{
    PyObject_HEAD
    AC_Render *ref;
};

static PyObject* _apy_SetVisibility(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_IsVisible(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_SetRenderLayer(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetRenderLayer(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_SetCurrentFrame(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetCurrentFrame(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_GetCameraPosition(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetCameraPosition(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_GetZoomDistance(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetZoomDistance(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_GetAlpha(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetAlpha(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_SetBackgroundColor(PyObject *self, PyObject *args, PyObject *kwds);

_APY_METH_START
    _APY_METHOD("SetVisibility", _apy_SetVisibility, "Sets the visibility of the entity")
    _APY_METHOD("IsVisible", _apy_IsVisible, "Returns if the entity is visible")
    _APY_METHOD("SetRenderLayer", _apy_SetRenderLayer, "Sets the render layer the entity is on")
    _APY_METHOD("GetRenderLayer", _apy_GetRenderLayer, "Returns which render layer the entity is on")
    _APY_METHOD("SetCurrentFrame", _apy_SetCurrentFrame, "Sets the current frame of the animation")
    _APY_METHOD("GetCurrentFrame", _apy_GetCurrentFrame, "Returns the current frame of the animation")
    _APY_METHOD("GetCameraPosition", _apy_GetCameraPosition, "Sets the camera position")
    _APY_METHOD("SetCameraPosition", _apy_SetCameraPosition, "Gets the camera position")
    _APY_METHOD("GetZoomDistance", _apy_GetZoomDistance, "Sets the camera position")
    _APY_METHOD("SetZoomDistance", _apy_SetZoomDistance, "Gets the camera position")
    _APY_METHOD("GetAlpha", _apy_GetAlpha, "Sets the camera position")
    _APY_METHOD("SetAlpha", _apy_SetAlpha, "Gets the camera position")
	_APY_METHOD("SetBackgroundColor", _apy_SetBackgroundColor, "Sets the background color")
_APY_METH_END

_APY_TYPE(APY_RenderCompProxy,"AC_Render")

PyObject* _apy_SetVisibility(PyObject *self, PyObject *args, PyObject *kwds){
    int aux;
    PyArg_ParseTuple(args,"i",&aux);
    
    ((APY_RenderCompProxy*)self)->ref->SetVisibility(aux);

    Py_RETURN_NONE;
}

PyObject* _apy_IsVisible(PyObject *self, PyObject *args, PyObject *kwds){

    return Py_BuildValue("i", ((APY_RenderCompProxy*)self)->ref->IsVisible());
}

PyObject* _apy_SetRenderLayer(PyObject *self, PyObject *args, PyObject *kwds){
    int layer;
    PyArg_ParseTuple(args,"i",&layer);

    ((APY_RenderCompProxy*)self)->ref->SetRenderLayer(layer);

    Py_RETURN_NONE;

}

PyObject* _apy_GetRenderLayer(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("i",((APY_RenderCompProxy*)self)->ref->GetRenderLayer());
}

PyObject* _apy_SetCurrentFrame(PyObject *self, PyObject *args, PyObject *kwds){
    int layer;
    PyArg_ParseTuple(args,"i",&layer);

    ((APY_RenderCompProxy*)self)->ref->SetCurrentFrame(layer);

    Py_RETURN_NONE;

}

PyObject* _apy_GetCurrentFrame(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("i",((APY_RenderCompProxy*)self)->ref->GetCurrentFrame());
}

PyObject* _apy_GetCameraPosition(PyObject *self, PyObject *args, PyObject *kwds){   
    Float2d pos = ((APY_RenderCompProxy*)self)->ref->GetCameraPosition();

    return Py_BuildValue("(f,f)", pos.x, pos.y ); 
}

PyObject* _apy_SetCameraPosition(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;

    PyArg_ParseTuple(args,"ff",&x,&y);
    
    ((APY_RenderCompProxy*)self)->ref->SetCameraPosition(x,y);

    Py_RETURN_NONE;
}

PyObject* _apy_SetZoomDistance(PyObject *self, PyObject *args, PyObject *kwds){
    float dist;
    PyArg_ParseTuple(args,"f",&dist);

    ((APY_RenderCompProxy*)self)->ref->SetZoomDistance(dist);

    Py_RETURN_NONE;

}

PyObject* _apy_GetZoomDistance(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("f",((APY_RenderCompProxy*)self)->ref->GetZoomDistance());
}

PyObject* _apy_SetAlpha(PyObject *self, PyObject *args, PyObject *kwds){
    float dist;
    PyArg_ParseTuple(args,"f",&dist);

    ((APY_RenderCompProxy*)self)->ref->SetAlpha(dist);

    Py_RETURN_NONE;

}

PyObject* _apy_GetAlpha(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("f",((APY_RenderCompProxy*)self)->ref->GetAlpha());
}

PyObject* _apy_SetBackgroundColor(PyObject *self, PyObject *args, PyObject *kwds){
    float r,g,b;
    PyArg_ParseTuple(args,"fff",&r,&g,&b);

    ((APY_RenderCompProxy*)self)->ref->SetBackgroundColor(r,g,b);

    Py_RETURN_NONE;

}

#endif 