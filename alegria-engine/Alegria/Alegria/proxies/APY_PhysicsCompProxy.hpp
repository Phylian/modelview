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

#ifndef APY_PhysicsCompProxy_hpp
#define APY_PhysicsCompProxy_hpp

#include "Python.h"

#include "APY_Utils.hpp"
#include "..\components\AC_Physics.hpp"
#include "..\kernel\AK_Entity.hpp"


struct APY_PhysicsCompProxy{
    PyObject_HEAD
    AC_Physics *ref;
};

static PyObject* _apy_SetPosition(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetRotation(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_ApplyForce(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_ApplyTorque(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_ApplyLinearImpulse(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_ApplyAngularImpulse(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetLinearVelocity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetLinearVelocity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetAngularVelocity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetAngularVelocity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetColEntities(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetColPoints(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_GetGravity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetGravity(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetSensor(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_SetType(PyObject *self, PyObject *args, PyObject *kwds);
static PyObject* _apy_TestPoint(PyObject *self, PyObject *args, PyObject *kwds);
_APY_METH_START
    //APY_COMPONENT_METHODS
    _APY_METHOD("SetPosition", _apy_SetPosition, "Sets the position of the body")
    _APY_METHOD("SetRotation", _apy_SetRotation, "Sets the rotation of the body")
    _APY_METHOD("ApplyForce", _apy_ApplyForce, "Applies a force to the body")
    _APY_METHOD("ApplyTorque", _apy_ApplyTorque, "Applies a torque to the body")
    _APY_METHOD("ApplyLinearImpulse", _apy_ApplyLinearImpulse, "Applies a linear impulse to the body")
    _APY_METHOD("ApplyAngularImpulse", _apy_ApplyAngularImpulse, "Applies an angular impulse to the body")
    _APY_METHOD("GetLinearVelocity", _apy_GetLinearVelocity, "Gets the linear velocity of the body")
    _APY_METHOD("SetLinearVelocity", _apy_SetLinearVelocity, "Sets the linear velocity of the body")
    _APY_METHOD("GetAngularVelocity", _apy_GetAngularVelocity, "Gets the angular velocity of the body")
    _APY_METHOD("SetAngularVelocity", _apy_SetAngularVelocity, "Sets the angular velocity of the body")
    _APY_METHOD("GetColEntities", _apy_GetColEntities, "Gets a list containing which entities the owner is colliding to")
    _APY_METHOD("GetColPoints", _apy_GetColPoints, "Gets a list containing which entities the owner is colliding to")
    _APY_METHOD("GetGravity", _apy_GetGravity, "Gets the world gravity")
    _APY_METHOD("SetGravity", _apy_SetGravity, "Sets the world gravity")
	_APY_METHOD("SetSensor", _apy_SetSensor, "Sets if it's a sensor")
	_APY_METHOD("SetType", _apy_SetType, "Sets its type")
    _APY_METHOD("TestPoint", _apy_TestPoint, "Test if a point is inside the body")
_APY_METH_END

_APY_TYPE(APY_PhysicsCompProxy,"AC_Physics")

PyObject* _apy_TestPoint(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;

    PyArg_ParseTuple(args,"ff",&x,&y);
    
    int inside = ((APY_PhysicsCompProxy*)self)->ref->TestPoint(x,y);

    PyObject *ret = Py_BuildValue("i",inside);

    return ret;
}

PyObject* _apy_SetPosition(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;

    PyArg_ParseTuple(args,"ff",&x,&y);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetPosition(x,y);

    Py_RETURN_NONE;
}

PyObject* _apy_SetRotation(PyObject *self, PyObject *args, PyObject *kwds){
    float angle;

    PyArg_ParseTuple(args,"f",&angle);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetRotation(angle);

    Py_RETURN_NONE;
}

PyObject* _apy_ApplyForce(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;
    int l;
    PyArg_ParseTuple(args,"ffi",&x,&y,&l);
    
    ((APY_PhysicsCompProxy*)self)->ref->ApplyForce(x,y,l);

    Py_RETURN_NONE;
}

PyObject* _apy_ApplyTorque(PyObject *self, PyObject *args, PyObject *kwds){
    float torque;
    PyArg_ParseTuple(args,"f",&torque);
    
    ((APY_PhysicsCompProxy*)self)->ref->ApplyTorque(torque);

    Py_RETURN_NONE;
}

PyObject* _apy_ApplyLinearImpulse(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;
    int l;
    PyArg_ParseTuple(args,"ffi",&x,&y,&l);
    
    ((APY_PhysicsCompProxy*)self)->ref->ApplyLinearImpulse(x,y,l);

    Py_RETURN_NONE;
}

PyObject* _apy_ApplyAngularImpulse(PyObject *self, PyObject *args, PyObject *kwds){
    float impulse;
    PyArg_ParseTuple(args,"f",&impulse);
    
    ((APY_PhysicsCompProxy*)self)->ref->ApplyAngularImpulse(impulse);

    Py_RETURN_NONE;
}

PyObject* _apy_GetGravity(PyObject *self, PyObject *args, PyObject *kwds){   
    Float2d vec = ((APY_PhysicsCompProxy*)self)->ref->GetGravity();

    return Py_BuildValue("(f,f)", vec.x, vec.y ); 
}

PyObject* _apy_SetGravity(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;

    PyArg_ParseTuple(args,"ff",&x,&y);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetGravity(x,y);

    Py_RETURN_NONE;
}

PyObject* _apy_SetSensor(PyObject *self, PyObject *args, PyObject *kwds){
    int s;

    PyArg_ParseTuple(args,"i",&s);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetSensor(s);

    Py_RETURN_NONE;
}

PyObject* _apy_SetType(PyObject *self, PyObject *args, PyObject *kwds){
    int s;

    PyArg_ParseTuple(args,"i",&s);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetBodyType(s);

    Py_RETURN_NONE;
}

PyObject* _apy_GetLinearVelocity(PyObject *self, PyObject *args, PyObject *kwds){   
    Float2d vec = ((APY_PhysicsCompProxy*)self)->ref->GetLinearVelocity();

    return Py_BuildValue("(f,f)", vec.x, vec.y ); 
}

PyObject* _apy_SetLinearVelocity(PyObject *self, PyObject *args, PyObject *kwds){
    float x,y;

    PyArg_ParseTuple(args,"ff",&x,&y);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetLinearVelocity(x,y);

    Py_RETURN_NONE;
}

PyObject* _apy_GetAngularVelocity(PyObject *self, PyObject *args, PyObject *kwds){   
    float w = ((APY_PhysicsCompProxy*)self)->ref->GetAngularVelocity();

    return Py_BuildValue("f", w ); 
}

PyObject* _apy_SetAngularVelocity(PyObject *self, PyObject *args, PyObject *kwds){
    float w;

    PyArg_ParseTuple(args,"f",&w);
    
    ((APY_PhysicsCompProxy*)self)->ref->SetAngularVelocity(w);

    Py_RETURN_NONE;
}

PyObject* _apy_GetColEntities(PyObject *self, PyObject *args, PyObject *kwds){
    vector<AK_Entity*>* entlist =  ((APY_PhysicsCompProxy*)self)->ref->GetColEntities();
    
    PyObject* collist = PyList_New(entlist->size());

    
    for(int i=0;i<entlist->size();i++){

        PyList_SetItem(collist, i, (*entlist)[i]->CreateProxy());
    }

    return collist;
}

PyObject* _apy_GetColPoints(PyObject *self, PyObject *args, PyObject *kwds){
    vector<b2Vec2>* pointlist =  ((APY_PhysicsCompProxy*)self)->ref->GetColPoints();
    
    PyObject* collist = PyList_New(pointlist->size());

    
    for(int i=0;i<pointlist->size();i++){
        
        PyObject *point= Py_BuildValue("(f,f)",(*pointlist)[i].x,(*pointlist)[i].y);
        PyList_SetItem(collist, i, point);
    }

    return collist;
}
#endif 