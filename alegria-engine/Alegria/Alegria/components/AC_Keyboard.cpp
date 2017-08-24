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
#include <iostream>
#include <stdlib.h>
#include "AC_Keyboard.hpp"
#include "../kernel/AK_Condition.hpp"
#include "../components/AC_KeyboardCondition.hpp"
#include "../proxies/APY_KeyboardCompProxy.hpp"
#include <iostream>

using namespace std;

AC_Keyboard::AC_Keyboard():m_keys(0){
    SetType("Keyboard");
}

AC_Keyboard::AC_Keyboard(const AC_Keyboard &der){
    SetType("Keyboard");

    m_keys = der.m_keys;

    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }

}
bool* AC_Keyboard::GetKeys(){
    return m_keys;
}
 
void AC_Keyboard::SetKeys(bool *keys){
    m_keys = keys;
}

bool AC_Keyboard::IsPressed(char key){
    char key2 = toupper(key);
    return m_keys[key2];
}

bool AC_Keyboard::IsAnyPressed(){
    for(int i=0;i<256;i++){
        if(m_keys[i])
            return true;
    }

    return false;
}

AK_Condition* AC_Keyboard::CreateCondition(){
    AC_KeyboardCondition *ncond = new AC_KeyboardCondition();
    m_conds.push_back(ncond);

    return ncond;
}

AK_Condition* AC_Keyboard::CreateCondition(const AK_Condition &cond){
    const AC_KeyboardCondition *old = static_cast<const AC_KeyboardCondition*>(&cond);
    AC_KeyboardCondition *ncond = new AC_KeyboardCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;
}

void AC_Keyboard::UpdateConditions(int ticks){
    list<AK_Condition*>::iterator it;
    
    for(it=m_conds.begin();it!=m_conds.end();++it){
        AC_KeyboardCondition *cond = static_cast<AC_KeyboardCondition*>(*it);
        if(cond->AllKeys()){
            if(IsAnyPressed())
                cond->SetPositive(true,ticks);
            else
                cond->SetPositive(false,ticks);
        }else{
            if(IsPressed(cond->GetKey()))
                cond->SetPositive(true,ticks);
            else
                cond->SetPositive(false,ticks);
        }
    }
}

int AC_Keyboard::Parse(xml_node<> *comp_node){
	for (xml_node<> * cond_node = comp_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling()){
		AK_Condition* cond = CreateCondition();
		cond->Parse(cond_node);
	}

	return 1;
}

int AC_Keyboard::Parse(const AIO_XMLToken &token){
    return 0;
}


PyObject* AC_Keyboard::CreateProxy(){
    APY_KeyboardCompProxy* aux = PyObject_New(APY_KeyboardCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;
}

/*
// Mètodes per a l'integració amb Python

_APY_OBJECT_TYPE(AC_Keyboard, "AC_Keyboard")
    
_APY_OBJECT_METH_START(AC_Keyboard)
    _APY_COMPONENT_METHODS
    _APY_OBJECT_METHOD("IsPressed", _apy_IsPressed, "Returns if the key is pressed")
_APY_OBJECT_METH_END



PyObject* AC_Keyboard::_apy_IsPressed(PyObject *self, PyObject *args, PyObject *kwds){
    char aux;
    PyArg_ParseTuple(args,"c",&aux);
    return Py_BuildValue("i", ((AC_Keyboard*)self)->IsPressed(aux));
}



string AC_Keyboard::_apy_GetRepr(){
    string msg = "Keyboard Component.";
    return msg;
}


*/

