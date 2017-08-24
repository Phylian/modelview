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

#ifndef APY_SoundCompProxy_hpp
#define APY_SoundCompProxy_hpp

#include "Python.h"
#include "APY_Utils.hpp"
#include "..\components\AC_Sound.hpp"


struct APY_SoundCompProxy{
    PyObject_HEAD
    AC_Sound *ref;
};


static PyObject* _apy_PlaySound(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_PlaySoundLoop(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_StopSound(PyObject *self, PyObject *args, PyObject *kwds);

_APY_METH_START
    _APY_METHOD("PlaySound", _apy_PlaySound, "Plays a sound")
	_APY_METHOD("PlaySoundLoop", _apy_PlaySoundLoop, "Plays a sound on loop")
	_APY_METHOD("StopSound", _apy_StopSound, "Stops a sound")
_APY_METH_END

_APY_TYPE(APY_SoundCompProxy, "AC_Sound")


PyObject* _apy_PlaySound(PyObject *self, PyObject *args, PyObject *kwds){
    char *arg;
    PyArg_ParseTuple(args, "s", &arg);
    
    string name(arg);

    ((APY_SoundCompProxy*)self)->ref->PlaySoundK(name);

    Py_RETURN_NONE;
}

PyObject* _apy_StopSound(PyObject *self, PyObject *args, PyObject *kwds){
    char *arg;
    PyArg_ParseTuple(args, "s", &arg);
    
    string name(arg);

    ((APY_SoundCompProxy*)self)->ref->StopSound(name);

    Py_RETURN_NONE;
}

PyObject* _apy_PlaySoundLoop(PyObject *self, PyObject *args, PyObject *kwds){
    char *arg;
    PyArg_ParseTuple(args, "s", &arg);
    
    string name(arg);

    ((APY_SoundCompProxy*)self)->ref->PlaySoundLoop(name);

    Py_RETURN_NONE;
}

#endif 