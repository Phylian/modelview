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

#include <Windows.h>
#include <gl/GL.h>

#include "../kernel/AK_Component.hpp"
#include "../components/AC_Message.hpp"
#include "../kernel/AK_Entity.hpp"

#include "AS_Message.hpp"

#include <iostream>

using namespace std;

AS_Message::AS_Message(){
    SetName("Message");
}

AS_Message::~AS_Message(){
    m_components.clear();
}

void AS_Message::PutMessage(const AK_Message &msg){
    m_messages.push_back(msg);
}

void AS_Message::Update(int ticks, double ms){
  
    list<AC_Message*>::iterator it;

    it=m_components.begin();
    
    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            AC_Message *comp = (*it);
            AK_Entity *owner = comp->GetOwner();
            list<AK_Message>::iterator it2;
            for(it2=m_messages.begin();it2!=m_messages.end();++it2){
                if(owner->GetAttribute((*it2).GetReceiver())){
                    comp->ReceiveMessage((*it2));
                }
            }

            comp->UpdateConditions(ticks);
            ++it;
        }
    }

    m_messages.clear();
}

int AS_Message::Register(AK_Component *comp){ // Registra el component al subsistema
    if(comp->GetType()!="Message")
        return 0;
    
    AC_Message *mcomp = static_cast<AC_Message*>(comp);
    
    m_components.push_back(mcomp);
    
    return 1;
}

void AS_Message::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Message");
    
    if(aux)
        Register(aux);
}

void AS_Message::Clean(){
    list<AC_Message*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();
}
