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

#ifndef APY_MessageCompProxy_hpp
#define APY_MessageCompProxy_hpp

#include "Python.h"

#include "APY_Utils.hpp"
#include "..\components\AC_Message.hpp"


struct APY_MessageCompProxy{
    PyObject_HEAD
    AC_Message *ref;
};

static PyObject* _apy_SendMessage(PyObject *self, PyObject *args, PyObject *kwds);




_APY_METH_START
    _APY_METHOD("SendMessage", _apy_SendMessage, "Sends a message")
_APY_METH_END

_APY_TYPE(APY_MessageCompProxy,"AC_Message")

PyObject* _apy_SendMessage(PyObject *self, PyObject *args, PyObject *kwds){
    char* csendto;
	char* cmsg;
    PyArg_ParseTuple(args,"ss",&csendto,&cmsg);
    string sendto(csendto);
	string msg(cmsg);
    ((APY_MessageCompProxy*)self)->ref->SendMessageK(sendto,msg);

    Py_RETURN_NONE;
}

#endif