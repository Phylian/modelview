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
#include "../components/AC_Mouse.hpp"
#include "../kernel/AK_SubsystemManager.hpp"

#include "AS_Mouse.hpp"

AS_Mouse::AS_Mouse(AS_Render *render):m_minfo(0),m_render(render){
    SetName("Mouse");
}

void AS_Mouse::Update(int ticks, double ms){
    list<AK_Component*>::iterator it;

    it=m_components.begin();

    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            AC_Mouse *comp = static_cast<AC_Mouse*>(*it);
            //cout << comp->GetMousePosS().x << " " << comp->GetMousePosS().y << endl;
            comp->UpdateConditions(ticks);
            ++it;
        }
    }
}

int AS_Mouse::Register(AK_Component *comp){
    if(comp->GetType()!="Mouse")
        return 0;
    
    AC_Mouse *mcomp = static_cast<AC_Mouse*>(comp);

    mcomp->SetMouseInfo(m_minfo);
    mcomp->ConnectRenderSubsystem(m_render);
    m_components.push_back(comp);

    return 1;
}

void AS_Mouse::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Mouse");
    
    if(aux)
        Register(aux);
}

void AS_Mouse::ConnectMouseInfo(__MouseInfo *minfo){
    m_minfo = minfo;
}

void AS_Mouse::Clean(){
    list<AK_Component*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();
}
