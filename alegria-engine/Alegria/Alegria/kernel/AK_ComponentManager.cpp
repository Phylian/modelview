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



#include "AK_ComponentManager.hpp"
#include "AK_ComponentFactoryBase.hpp"
#include "AK_Component.hpp"


    

AK_ComponentManager::~AK_ComponentManager(){
    map<string, AK_ComponentFactoryBase*>::iterator it;
    while(!m_factories.empty()){
        it = m_factories.begin();
        delete it->second;
        m_factories.erase(it);
    }
}



int AK_ComponentManager::RegisterComponentType(AK_ComponentFactoryBase *factory){
    if(factory==NULL)
        return 0;
        
    m_factories[factory->GetType()] = factory;
    return 1;
}





int AK_ComponentManager::RemoveComponentType(const string &type){
    map<string, AK_ComponentFactoryBase*>::iterator it = m_factories.find(type);
    
    if(it!=m_factories.end()){
        delete it->second;
        m_factories.erase(it);
        return 1;
    }
    return 0;    
}




AK_Component* AK_ComponentManager::CreateComponent(const string &type){
    map<string, AK_ComponentFactoryBase*>::iterator it = m_factories.find(type);
    
    if(it==m_factories.end())
        return NULL;
    return it->second->CreateComponent();
}





AK_Component* AK_ComponentManager::CreateComponent(AK_Component* comp){
    map<string, AK_ComponentFactoryBase*>::iterator it = m_factories.find(comp->GetType());
    
    if(it==m_factories.end())
        return NULL;
    return it->second->CreateComponent(comp);
}
