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

#include <string>
#include "Python.h"

#include "AK_PyObjectBase.hpp"

using namespace std;



// Constructor.
// Paràmetres:
//      - type : Tipus identificador per a Python, ha de ser provist
//               per les classes derivades.

AK_PyObjectBase::AK_PyObjectBase(PyTypeObject *type){
    this->ob_type = type;
    //_Py_NewReference(this);
    this->ob_refcnt=0;
    
    
    //this->ob_type = type;
    //Pot faltar algo al respecte de les referències en Python? =S
}

AK_PyObjectBase::~AK_PyObjectBase(){
    //this->ob_refcnt=0;  
   
   
}

// Funció que serà cridada per Python quan es requerisca una representació
// de l'objecte.
// Paràmetres:
//      - self : El propi objecte, seguint el perfil imposat per Python.
// Retorna:
//      - Un PyObject que conté la representació de l'objecte.
//
// Aquesta funció és tansols un wrapper, cada classe derivada ha de 
// proporcionar una implementació de _apy_GetRepr() que es qui realment 
// defineix la representació.

PyObject* AK_PyObjectBase::_apy_repr(PyObject *self){
    string msg = ((AK_PyObjectBase*)self)->_apy_GetRepr(); //Crida a la funció virtual que rep la representació.

    return Py_BuildValue("s",msg.c_str());
}



// Funció per a obtindre la representació de un objecte en Python
// Paràmetres:
//      (cap)
// Retorna:
//      - Una cadena de caràcters que representa l'objecte.
//
// Funció virtual que retorna la representació de l'objecte. Aquesta representació
// es mostrarà, per exemple, quan desde Python fem un print sobre l'objecte.
// Ha de ser redefinida per les classes que l'hereten.

string AK_PyObjectBase::_apy_GetRepr(){
    string msg = "Default AK_PyObject.\n(Representation not overriden by object)";
    return msg;
}



PyObject* AK_PyObjectBase::GetPyObject(){
    return Py_BuildValue("O!",this->ob_type,this);
}

