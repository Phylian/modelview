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
#include "../components/AC_Always.hpp"
#include "../kernel/AK_Entity.hpp"

#include "AS_Always.hpp"

#include <iostream>

using namespace std;

AS_Always::AS_Always(){
    SetName("Always");
}

AS_Always::~AS_Always(){
    m_components.clear();
}

void AS_Always::Update(int ticks, double ms){
  
    list<AC_Always*>::iterator it;

    it=m_components.begin();
    
 
    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            AC_Always *comp = (*it);
            
            comp->UpdateConditions(ticks);
            ++it;
        }
    }
}

int AS_Always::Register(AK_Component *comp){ // Registra el component al subsistema
    if(comp->GetType()!="Always")
        return 0;
    
    AC_Always *Alwayscomp = static_cast<AC_Always*>(comp);
    m_components.push_back(Alwayscomp);
    
    return 1;
}

void AS_Always::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Always");
    
    if(aux)
        Register(aux);
}


void AS_Always::Clean(){
    list<AC_Always*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();
}