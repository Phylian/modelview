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

#include "AK_Entity.hpp"
#include "AK_Component.hpp"
#include "AK_Condition.hpp"


AK_Component::AK_Component():m_type("NO TYPE"),m_owner(NULL),m_erase(false){}
   
// Deallocates all the conditions.
AK_Component::~AK_Component(){
    list<AK_Condition*>::iterator it;

    for(it=m_conds.begin();it!=m_conds.end();++it){
        delete (*it);
    }

    m_conds.clear();
}
  


int AK_Component::SetType(const string &type){
    if(m_type == "NO TYPE"){
        m_type = type;
        return 1;
    }else{
        return 0;
    }
}




string AK_Component::GetType(){
    return m_type;
}


void AK_Component::Erase(){
    m_erase = 1;
}


bool AK_Component::IsErased(){
    return m_erase;
}

int AK_Component::SetOwner(AK_Entity *ent){
    if(ent==NULL)
        return 0;
        
    m_owner = ent;
    return 1;
}

AK_Entity* AK_Component::GetOwner(){
    return m_owner;
}


AK_Condition* AK_Component::GetCondition(const string &name){
    list<AK_Condition*>::iterator it;

    for(it=m_conds.begin();it!=m_conds.end();++it){
        if((*it)->GetName()==name)
            return (*it);
    }

    return 0;
}










