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
#include "AC_Always.hpp"
#include "../kernel/AK_Condition.hpp"
#include "../components/AC_AlwaysCondition.hpp"
//#include "../proxies/APY_AlwaysCompProxy.hpp"
#include <iostream>

using namespace std;

AC_Always::AC_Always(){
    SetType("Always");
}

AC_Always::AC_Always(const AC_Always &der){
    SetType("Always");


    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }

}



AK_Condition* AC_Always::CreateCondition(){
    AC_AlwaysCondition *ncond = new AC_AlwaysCondition();
    m_conds.push_back(ncond);

    return ncond;
  
}

AK_Condition* AC_Always::CreateCondition(const AK_Condition &cond){
    const AC_AlwaysCondition *old = static_cast<const AC_AlwaysCondition*>(&cond);
    AC_AlwaysCondition *ncond = new AC_AlwaysCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;

}

void AC_Always::UpdateConditions(int ticks){
    list<AK_Condition*>::iterator it;

    for(it=m_conds.begin();it!=m_conds.end();++it){
        AC_AlwaysCondition *cond = static_cast<AC_AlwaysCondition*>(*it);
        
        cond->SetPositive(true,ticks);
    }
}

int AC_Always::Parse(const AIO_XMLToken &token){
    return 0;
}

int AC_Always::Parse(xml_node<> *comp_node){
	for (xml_node<> * cond_node = comp_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling()){
		AK_Condition* cond = CreateCondition();
		cond->Parse(cond_node);
	}

	return 1;
}



PyObject* AC_Always::CreateProxy(){
    /*APY_KeyboardCompProxy* aux = PyObject_New(APY_KeyboardCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;*/
    
    return 0;
}


