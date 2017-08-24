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
#include "AC_Message.hpp"
#include "../kernel/AK_Condition.hpp"
#include "../components/AC_MessageCondition.hpp"
#include "../subsystems/AS_Message.hpp"
#include "../proxies/APY_MessageCompProxy.hpp"
#include <iostream>

using namespace std;

AC_Message::AC_Message(AS_Message *asmsg):m_asmsg(asmsg){
    SetType("Message");
}

AC_Message::AC_Message(const AC_Message &der){
    SetType("Message");

    m_asmsg=der.m_asmsg;

    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }

}


void AC_Message::SendMessageK(const string &sendto, const string &msg){
    AK_Message message(sendto,msg,this->GetOwner());
    m_asmsg->PutMessage(message);
}

void AC_Message::ReceiveMessage(const AK_Message &msg){
    m_messages.push_back(msg);
}

AK_Condition* AC_Message::CreateCondition(){
    AC_MessageCondition *ncond = new AC_MessageCondition();
    m_conds.push_back(ncond);

    return ncond;


  
}

AK_Condition* AC_Message::CreateCondition(const AK_Condition &cond){
    const AC_MessageCondition *old = static_cast<const AC_MessageCondition*>(&cond);
    AC_MessageCondition *ncond = new AC_MessageCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;

    return 0;
}

void AC_Message::UpdateConditions(int ticks){
    list<AK_Condition*>::iterator it;

    for(it=m_conds.begin();it!=m_conds.end();++it){
        AC_MessageCondition *cond = static_cast<AC_MessageCondition*>(*it);
        
        list<AK_Message>::iterator it2;
        bool setpositive=false;
        for(it2=m_messages.begin();it2!=m_messages.end();++it2){
            if(cond->GetMessageK()==(*it2).GetMsg()){
                setpositive=true;
                break;
            }
        }
        
        cond->SetPositive(setpositive,ticks);
    }

    m_messages.clear();
}

int AC_Message::Parse(xml_node<> *comp_node){
	for (xml_node<> * cond_node = comp_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling()){
		AK_Condition* cond = CreateCondition();
		cond->Parse(cond_node);
	}

	return 1;
}

int AC_Message::Parse(const AIO_XMLToken &token){
    return 0;
}


PyObject* AC_Message::CreateProxy(){
    APY_MessageCompProxy* aux = PyObject_New(APY_MessageCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;

}

