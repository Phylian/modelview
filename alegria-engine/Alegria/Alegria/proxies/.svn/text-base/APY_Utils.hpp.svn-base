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



#ifndef APY_Utils_hpp
#define APY_Utils_hpp

#define _APY_TYPE(Class,ClassStr) \
    static PyTypeObject _type = {\
        PyObject_HEAD_INIT(&PyType_Type)\
        0,				/*ob_size*/\
        ClassStr,			/*tp_name*/\
        sizeof(Class),			/*tp_basicsize*/\
        0,				/*tp_itemsize*/\
        /* methods */\
        apy_dealloc,	  		/*tp_dealloc*/ \
        0,			 	/*tp_print*/ \
        0, 			/*tp_Getattr*/ \
        0, 			/*tp_Setattr*/ \
        0,			        /*tp_compare*/ \
        0,		        	/*tp_repr*/ \
        0,0,0,0,0,0,\
        PyObject_GenericGetAttr,\
        PyObject_GenericSetAttr,\
        0,\
        Py_TPFLAGS_DEFAULT| Py_TPFLAGS_BASETYPE,\
        0,0,0,0,0,0,0,\
        _methods\
    };

static void apy_dealloc(PyObject* self){
    PyObject_Del(self);
}

#define _APY_METH_START \
    static PyMethodDef _methods[] = {

#define _APY_METH_END \
    {NULL,NULL}};

#define _APY_METHOD(Name,Func,Desc) \
    {Name,     (PyCFunction) Func,    METH_VARARGS, Desc}, 

#endif 
