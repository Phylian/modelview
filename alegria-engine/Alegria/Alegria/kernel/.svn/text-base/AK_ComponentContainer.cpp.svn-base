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
#include "AK_ComponentContainer.hpp"
#include "AK_Component.hpp"

using namespace std;


// Constructor

AK_ComponentContainer::AK_ComponentContainer(){}



// Destructor virtual

AK_ComponentContainer::~AK_ComponentContainer(){
    ComponentMap::iterator it;


    m_component_map.clear();
}





// Afegix un component al contenidor. Si no es pot afegir perque ja 
// existeix un component d'eixe tipus retorna 0. Si tot va bé retorna 1.

int AK_ComponentContainer::AddComponent(AK_Component *comp){
    
    if(comp==NULL)
        return 0;
        
    string type = comp->GetType();
    ComponentMap::iterator it = m_component_map.find(type);
    
    if(it == m_component_map.end()){
        m_component_map.insert(pair<string,AK_Component*>(type,comp));
        return 1;
    }else{
        return 0;
    }
}



// Obté un component a partir del seu nom. Retorna NULL si no el troba.

AK_Component* AK_ComponentContainer::GetComponent(const string &type){   
    ComponentMap::iterator it = m_component_map.find(type);
    
    if(it!=m_component_map.end())
        return it->second;
    else
        return NULL;
}




const map<string, AK_Component*>* AK_ComponentContainer::GetComponents(){
    return &m_component_map;
}

