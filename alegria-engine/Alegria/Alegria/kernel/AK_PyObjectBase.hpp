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

/**
 * Classe de la que han d'heretar tots els objectes que tinguen 
 * que ser accedits desde Python.
*/

#ifndef AK_PyObjectBase_hpp
#define AK_PyObjectBase_hpp

#include <string>
#include "Python.h"

using namespace std;

// Macros per a utilitzar en les classes derivades.
// -----

#define _APY_OBJECT_HEADER \
    public: \
        static PyTypeObject   m_apy_type; \
        static PyMethodDef    m_apy_methods[]; 

#define _APY_OBJECT_TYPE(Class,ClassStr) \
    PyTypeObject Class::m_apy_type = {\
        PyObject_HEAD_INIT(&PyType_Type)\
        0,				/*ob_size*/\
        ClassStr,			/*tp_name*/\
        sizeof(Class),			/*tp_basicsize*/\
        0,				/*tp_itemsize*/\
        /* methods */\
        0,	  		/*tp_dealloc*/ \
        0,			 	/*tp_print*/ \
        0, 			/*tp_Getattr*/ \
        0, 			/*tp_Setattr*/ \
        0,			        /*tp_compare*/ \
        _apy_repr,		        	/*tp_repr*/ \
        0,0,0,0,0,0,\
        PyObject_GenericGetAttr,\
        PyObject_GenericSetAttr,\
        0,\
        Py_TPFLAGS_DEFAULT| Py_TPFLAGS_BASETYPE,\
        0,0,0,0,0,0,0,\
        m_apy_methods\
    };

#define _APY_OBJECT_METH_START(Class) \
    PyMethodDef Class::m_apy_methods[] = {

#define _APY_OBJECT_METH_END \
    {NULL,NULL}};

#define _APY_OBJECT_METHOD(Name,Func,Desc) \
    {Name,     (PyCFunction) Func,    METH_VARARGS, Desc}, 

// -----

class AK_PyObjectBase:public PyObject{
    //Py_Header;							// Always start with Py_Header

    protected:
         static PyObject* _apy_repr(PyObject *self);
         virtual string _apy_GetRepr();
    public:
        AK_PyObjectBase(PyTypeObject *type);
        virtual ~AK_PyObjectBase();
        PyObject* GetPyObject();
};

#endif //AK_PyObjectBase_hpp
