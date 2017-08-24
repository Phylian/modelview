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

#ifndef APY_KeyboardCompProxy_hpp
#define APY_KeyboardCompProxy_hpp

#include "Python.h"
#include "APY_Utils.hpp"
#include "..\components\AC_Keyboard.hpp"


struct APY_KeyboardCompProxy{
    PyObject_HEAD
    AC_Keyboard *ref;
};


static PyObject* _apy_IsPressed(PyObject *self, PyObject *args, PyObject *kwds);

_APY_METH_START
    //_APY_COMPONENT_METHODS
    _APY_METHOD("IsPressed", _apy_IsPressed, "Returns if the key is pressed")
_APY_METH_END

_APY_TYPE(APY_KeyboardCompProxy, "AC_Keyboard")

PyObject* _apy_IsPressed(PyObject *self, PyObject *args, PyObject *kwds){
    int aux;
    PyArg_ParseTuple(args,"i",&aux);
    bool a = ((APY_KeyboardCompProxy*)self)->ref->IsPressed(char(aux));
    return Py_BuildValue("i", ((APY_KeyboardCompProxy*)self)->ref->IsPressed(aux));
}


#endif 