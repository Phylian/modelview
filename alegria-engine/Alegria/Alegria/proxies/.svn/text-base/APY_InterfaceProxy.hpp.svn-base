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

#ifndef APY_InterfaceProxy_hpp
#define APY_InterfaceProxy_hpp

#include "Python.h"

#include "..\components\AC_Physics.hpp"
#include "..\kernel\AK_Entity.hpp"

#include "..\subsystems\AS_Physics.hpp"
#include "..\subsystems\AS_Render.hpp"
#include "..\kernel\AK_EntityManager.hpp"

#include "..\Alegria.hpp"


void __ConnectSubsystems(AK_EntityManager *entmanager, Alegria* alegria);

AK_EntityManager* __GetEntityManager();
Alegria* __GetAlegria();

static PyObject* _apy_CreateEntity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetEntity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetEntitiesByAttribute(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_Quit(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_LoadScene(PyObject *self, PyObject *args, PyObject *kwds);

static PyMethodDef EmbMethods[] = {
    {"CreateEntity", (PyCFunction)_apy_CreateEntity, METH_VARARGS,
     "Creates a new entity based on an archtype name"},
     {"GetEntity", (PyCFunction)_apy_GetEntity, METH_VARARGS,
     "Gets an Entity given its name"},
	 {"GetEntitiesByAttribute", (PyCFunction)_apy_GetEntitiesByAttribute, METH_VARARGS,
     "Gets a list of entities given an attribute"},
     {"LoadScene", (PyCFunction)_apy_LoadScene, METH_VARARGS,
     "Loads a new scene, deallocating the old one"},
     {"Quit", (PyCFunction)_apy_Quit, METH_VARARGS,
     "Ends the game."},
    {NULL, NULL, 0, NULL}
};


PyObject* _apy_CreateEntity(PyObject *self, PyObject *args, PyObject *kwds){
    
    char *arg;
    float posx,posy,rot,size;

    PyArg_ParseTuple(args,"sffff",&arg,&posx,&posy,&rot,&size);
    
    string name(arg);

    AK_Entity* ent = __GetEntityManager()->CreateEntity(name,posx,posy,rot,size);
    


    return ent->CreateProxy();
}

PyObject* _apy_Quit(PyObject *self, PyObject *args, PyObject *kwds){
    
    char *arg;


  

    __GetAlegria()->Quit();
    


    Py_RETURN_NONE;
}

PyObject* _apy_LoadScene(PyObject *self, PyObject *args, PyObject *kwds){
    
    char *arg;


    PyArg_ParseTuple(args,"s",&arg);
    
    string name(arg);

    __GetAlegria()->ChangeScene(arg);
    
    


    Py_RETURN_NONE;
}

PyObject* _apy_GetEntity(PyObject *self, PyObject *args, PyObject *kwds){
    
    char *arg;


    PyArg_ParseTuple(args,"s",&arg);
    
    string name(arg);

    AK_Entity* ent = __GetEntityManager()->GetEntity(name);
    
    if(!ent)
        Py_RETURN_NONE;

    return ent->CreateProxy();
}

PyObject* _apy_GetEntitiesByAttribute(PyObject *self, PyObject *args, PyObject *kwds){
    
    char *arg;


    PyArg_ParseTuple(args,"s",&arg);
    
    string attr(arg);

	list<AK_Entity*> entities = __GetEntityManager()->GetEntitiesByAttribute(attr);
    
    PyObject* entlist = PyList_New(entities.size());

	list<AK_Entity*>::iterator it;
	int i=0;
    for(it=entities.begin();it!=entities.end();++it){
		PyList_SetItem(entlist, i, (*it)->CreateProxy());
		i++;
	}

	return entlist;
}



#endif 