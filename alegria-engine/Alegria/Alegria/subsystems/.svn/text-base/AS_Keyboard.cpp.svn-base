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
#include "../kernel/AK_Entity.hpp"
#include "../components/AC_Keyboard.hpp"
#include "AS_Keyboard.hpp"

AS_Keyboard::AS_Keyboard():m_keys(0){
    SetName("Keyboard");
}

void AS_Keyboard::Update(int ticks, double ms){
    list<AK_Component*>::iterator it;

    it=m_components.begin();

    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            /*AC_Keyboard *comp = static_cast<AC_Keyboard*>(*it);
            if(comp->GetKeys()==0)
                comp->SetKeys(m_keys);*/
            (*it)->UpdateConditions(ticks);
            ++it;
        }
    }
}
int AS_Keyboard::Register(AK_Component *comp){
    if(comp->GetType()!="Keyboard")
        return 0;
    
    AC_Keyboard *kcomp = static_cast<AC_Keyboard*>(comp);
    kcomp->SetKeys(m_keys);

    m_components.push_back(comp);

    return 1;
}

void AS_Keyboard::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Keyboard");
    
    if(aux)
        Register(aux);
}

void AS_Keyboard::ConnectKeys(bool *keys){
    m_keys = keys;
}

void AS_Keyboard::Clean(){
    list<AK_Component*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();
}
