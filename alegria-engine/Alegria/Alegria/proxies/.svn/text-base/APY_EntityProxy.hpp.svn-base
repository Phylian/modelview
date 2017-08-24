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

#ifndef APY_EntityProxy_hpp
#define APY_EntityProxy_hpp

#include "Python.h"
#include "..\kernel\AK_Entity.hpp"
#include "..\kernel\AK_Component.hpp"
#include "../attributes/AA_Int.hpp"
#include "../attributes/AA_Float.hpp"
#include "../attributes/AA_Bool.hpp"
#include "../attributes/AA_String.hpp"

#include "APY_Utils.hpp"

class AK_Entity;

struct APY_EntityProxy{
    PyObject_HEAD
    AK_Entity *ref;
};


static PyObject* _apy_GetComponent(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetParent(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetChildren(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetChildrenCount(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetName(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_Kill(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_KillNextFrame(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_GetPosition(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetRotation(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetSize(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_SetPosition(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetRotation(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetSize(PyObject *self, PyObject *args, PyObject *kwds);

static PyObject* _apy_GetAttribute(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetAttribute(PyObject *self, PyObject *args, PyObject *kwds);

_APY_METH_START
    _APY_METHOD("GetComponent", _apy_GetComponent, "Returns a component given its name")
    _APY_METHOD("GetParent", _apy_GetParent, "Returns the parent of the entity (if any)")
    _APY_METHOD("GetChildren", _apy_GetChildren, "Returns the children of the entity (if any)")
    _APY_METHOD("GetChildrenCount", _apy_GetChildrenCount, "Returns the number of children of the entity")
    _APY_METHOD("GetName", _apy_GetName, "Returns the name of the entity")
    _APY_METHOD("Kill", _apy_Kill, "Kills the entity")
	_APY_METHOD("KillNextFrame", _apy_KillNextFrame, "Kills the entity the next frame")
    _APY_METHOD("GetPosition", _apy_GetPosition, "Gets the position of the entity")
    _APY_METHOD("GetRotation", _apy_GetRotation, "Gets the rotation of the entity")
    _APY_METHOD("GetSize", _apy_GetSize, "Gets the size of the entity")
    _APY_METHOD("SetPosition", _apy_SetPosition, "Sets the position of the entity")
    _APY_METHOD("SetRotation", _apy_SetRotation, "Sets the rotation of the entity")
    _APY_METHOD("SetSize", _apy_SetSize, "Sets the size of the entity")
    _APY_METHOD("GetAttribute", _apy_GetAttribute, "Gets the value of the given attribute")
    _APY_METHOD("SetAttribute", _apy_SetAttribute, "Sets the value of the given attribute")
_APY_METH_END

_APY_TYPE(APY_EntityProxy,"AK_Entity")

PyObject* _apy_GetComponent(PyObject *self, PyObject *args, PyObject *kwds){
    char *arg;
    PyArg_ParseTuple(args, "s", &arg);
    
    string name(arg);
    
    AK_Component* comp = ((APY_EntityProxy*)self)->ref->GetComponent(name);
    if(!comp)
        Py_RETURN_NONE;
    
    return comp->CreateProxy();
}

PyObject* _apy_GetParent(PyObject *self, PyObject *args, PyObject *kwds){
    AK_Entity* parent = ((APY_EntityProxy*)self)->ref->GetParent();
    
    if(!parent)
        Py_RETURN_NONE;
    
    return parent->CreateProxy();
}

PyObject* _apy_GetChildren(PyObject *self, PyObject *args, PyObject *kwds){
    AK_Entity** children = ((APY_EntityProxy*)self)->ref->GetChildren();
    int children_count = ((APY_EntityProxy*)self)->ref->GetChildrenCount();
    PyObject* children_list = PyList_New(children_count);
    
    for(int i=0;i<children_count;i++){
        //Py_INCREF(children[i]);
        PyList_SetItem(children_list, i, children[i]->CreateProxy());
    }
    
    delete[] children;
    
    return children_list;
}

PyObject* _apy_GetChildrenCount(PyObject *self, PyObject *args, PyObject *kwds){
    int n = ((APY_EntityProxy*)self)->ref->GetChildrenCount();
    
    return PyInt_FromLong(n);
}

PyObject* _apy_GetName(PyObject *self, PyObject *args, PyObject *kwds){
    string name = ((APY_EntityProxy*)self)->ref->GetName();
    return Py_BuildValue("s", name.c_str());  
}    

PyObject* _apy_Kill(PyObject *self, PyObject *args, PyObject *kwds){
    ((APY_EntityProxy*)self)->ref->Kill();
    Py_RETURN_NONE;
}

PyObject* _apy_KillNextFrame(PyObject *self, PyObject *args, PyObject *kwds){
	((APY_EntityProxy*)self)->ref->KillNextFrame();
    Py_RETURN_NONE;
}


PyObject* _apy_GetPosition(PyObject *self, PyObject *args, PyObject *kwds){
    float x=((APY_EntityProxy*)self)->ref->GetPosition().x;
    float y=((APY_EntityProxy*)self)->ref->GetPosition().y;
    return Py_BuildValue("(f,f)", x,y );  
}   

PyObject* _apy_GetRotation(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("f", ((APY_EntityProxy*)self)->ref->GetRotation() );  
}   

PyObject* _apy_GetSize(PyObject *self, PyObject *args, PyObject *kwds){
    return Py_BuildValue("f", ((APY_EntityProxy*)self)->ref->GetSize() );  
}   

PyObject* _apy_SetPosition(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;
    PyArg_ParseTuple(args, "f|f", &x,&y);
    ((APY_EntityProxy*)self)->ref->SetPosition(x,y);
    Py_RETURN_NONE;
}

PyObject* _apy_SetRotation(PyObject *self, PyObject *args, PyObject *kwds){
    float x;
    PyArg_ParseTuple(args, "f", &x);
    ((APY_EntityProxy*)self)->ref->SetRotation(x);
    Py_RETURN_NONE;
}

PyObject* _apy_SetSize(PyObject *self, PyObject *args, PyObject *kwds){
    float x;
    PyArg_ParseTuple(args, "f", &x);
    ((APY_EntityProxy*)self)->ref->SetSize(x);
    Py_RETURN_NONE;
}

static PyObject* _apy_GetAttribute(PyObject *self, PyObject *args, PyObject *kwds){
    char *arg;
    PyArg_ParseTuple(args, "s", &arg);
    
    string name(arg);

    AK_Attribute* attr = ((APY_EntityProxy*)self)->ref->GetAttribute(name);

    if(!attr)
        Py_RETURN_NONE;

    AA_Int *aint;
    AA_Float *afloat;
    AA_Bool *abool;
    AA_String *astring;

    switch(attr->GetType()){
        case AK_ATTR_INT:
            aint = static_cast<AA_Int*>(attr);

            return Py_BuildValue("i", aint->GetValue());

        case AK_ATTR_FLOAT:
            afloat = static_cast<AA_Float*>(attr);

            return Py_BuildValue("f", afloat->GetValue());

        case AK_ATTR_BOOL:
            abool = static_cast<AA_Bool*>(attr);

            return Py_BuildValue("i", (int)abool->GetValue());
        case AK_ATTR_STR:
            astring = static_cast<AA_String*>(attr);

            string str = astring->GetValue();
            return Py_BuildValue("s", str.c_str());

    }

    Py_RETURN_NONE;
    

}
static PyObject* _apy_SetAttribute(PyObject *self, PyObject *args, PyObject *kwds){
    char *arg;
    PyObject *obj;
    PyArg_ParseTuple(args, "sO", &arg, &obj);
    
    string name(arg);

    AK_Attribute* attr = ((APY_EntityProxy*)self)->ref->GetAttribute(name);

    AA_Int *aint;
    AA_Float *afloat;
    AA_Bool *abool;
    AA_String *astring;

    if(!attr)
        Py_RETURN_NONE;

    switch(attr->GetType()){
        case AK_ATTR_INT:
            aint = static_cast<AA_Int*>(attr);
            if(PyInt_Check(obj))
                aint->SetValue(PyInt_AsLong(obj));

            break;

        case AK_ATTR_FLOAT:
            afloat = static_cast<AA_Float*>(attr);
            if(PyFloat_Check(obj))
                afloat->SetValue(PyFloat_AsDouble(obj));
            break;

        case AK_ATTR_BOOL:
            abool = static_cast<AA_Bool*>(attr);
            
            if(PyInt_Check(obj))
                abool->SetValue((bool)PyInt_AsLong(obj));

            break;

        case AK_ATTR_STR:
            astring = static_cast<AA_String*>(attr);
            if(PyString_Check(obj))
                astring->SetValue(PyString_AsString(obj));

            break;

    }

    Py_RETURN_NONE;
    
}

#endif 