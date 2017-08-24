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

#include <list>
#include "AK_EntityManager.hpp"
#include "AK_Entity.hpp"
#include "AK_ComponentManager.hpp"
#include "AK_Component.hpp"
#include "AK_ArchtypeManager.hpp"
#include "AK_Archtype.hpp"
#include "AK_ScriptQueue.hpp"
#include "AK_Trigger.hpp"
#include "AK_Attribute.hpp"
#include "AK_SubsystemManager.hpp"


using namespace std;

// Constructor.
// Paràmetres:
//      - archmngr : Punter al manager d'arquetips.
//      - cmpmngr  : Punter al manager de components.
//

AK_EntityManager::AK_EntityManager(AK_ArchtypeManager* archmngr,AK_ComponentManager* cmpmngr, AK_ScriptQueue* scriptqueue, AK_SubsystemManager *ssmanager):
                                    m_archtypemanager(archmngr), m_componentmanager(cmpmngr), m_scriptqueue(scriptqueue),m_ssmanager(ssmanager){}



// Destructor. Borra totes les entitats.

AK_EntityManager::~AK_EntityManager(){
 
    list<AK_Entity*>::iterator it;
    it = m_entities.begin();

    while(it!=m_entities.end()){
        AK_Entity *ent = (*it); 
        delete (ent);
        it=m_entities.erase(it);
    }
}


AK_Entity* AK_EntityManager::CreateEntity(const string &name, float posx, float posy, float rot, float size){
    AK_Archtype *arch = m_archtypemanager->GetArchtype(name);
    if(!arch)
        return 0;

    const map<string, AK_Component*>* comps = arch->GetComponents();
 
    AK_Entity* ent = new AK_Entity(arch->GetNewID(), m_scriptqueue);
    
    map<string, AK_Component*>::const_iterator it;
    
    for(it=comps->begin(); it!=comps->end();++it){
        AK_Component *comp = it->second;
        comp = m_componentmanager->CreateComponent(comp);
        if(comp)
            ent->AddComponentK(comp);

    }
    
    const list<AK_Trigger*> *triggers = arch->GetTriggers();

    list<AK_Trigger*>::const_iterator it2;

    for(it2=triggers->begin(); it2!=triggers->end();++it2){
        AK_Trigger* trigg = new AK_Trigger(*(*it2),ent);
        ent->AddTrigger(trigg);
        
    }

    const map<string,AK_Attribute*> *attributes = arch->GetAttributes();

    map<string,AK_Attribute*>::const_iterator it3;

    for(it3=attributes->begin(); it3!=attributes->end();++it3){
        ent->AddAttribute(it3->first,it3->second);           
    }


    ent->SetPosition(posx, posy);

    ent->SetRotation(rot);

    ent->SetSize(size);

    m_ssmanager->AddEntity(ent);

    m_entities.push_back(ent);

    return ent;
}

AK_Entity* AK_EntityManager::GetEntity(const string &name){
    list<AK_Entity*>::iterator it;

    for(it=m_entities.begin();it!=m_entities.end();++it){
        if((*it)->GetName()==name)
            return (*it);
    } 

    return 0;
   
}

list<AK_Entity*> AK_EntityManager::GetEntitiesByAttribute(const string &attr){
    list<AK_Entity*>::iterator it;
	list<AK_Entity*> entity_list;

    for(it=m_entities.begin();it!=m_entities.end();++it){
		if((*it)->GetAttribute(attr))
			entity_list.push_back(*it);
    } 

    return entity_list;
   
}

int AK_EntityManager::EraseEntity(AK_Entity *ent){
    if(!ent)
        return 0;
        

    list<AK_Entity*>::iterator it = m_entities.begin();

    while(it!=m_entities.end()){
        if((*it)==ent){
            delete ent;
            m_entities.erase(it);
            return 1;
        }
        ++it;
    }
    

    return 0;
}

int AK_EntityManager::EraseAllEntities(){
    

    list<AK_Entity*>::iterator it;
    it = m_entities.begin();

    while(it!=m_entities.end()){
        AK_Entity *ent = (*it); 
        delete (ent);
        it=m_entities.erase(it);
    }

    return 1;
}


void AK_EntityManager::CheckTriggers(){
    
    list<AK_Entity*>::iterator it = m_entities.begin();

    while(it!=m_entities.end()){
        (*it)->UpdateTriggers();
        ++it;
    }
}

void AK_EntityManager::UpdateEntities(){
	list<AK_Entity*>::iterator it = m_entities.begin();

    while(it!=m_entities.end()){
		int kill_counter = (*it)->IsKilledNextFrame();
		if(kill_counter==2)
			(*it)->SetKillNextFrame(1);
		else if(kill_counter==1){
			(*it)->SetKillNextFrame(0);
			(*it)->Kill();
		}
		++it;
    }
}


void AK_EntityManager::CleanEntities(){
    

    list<AK_Entity*>::iterator it;
    it = m_entities.begin();

    while(it!=m_entities.end()){
        if((*it)->IsDead()){
            delete (*it);
            it=m_entities.erase(it);
        }else{
            it++;
        }

    }
}

void AK_EntityManager::Clean(){
    list<AK_Entity*>::iterator it;
    for(it = m_entities.begin();it!=m_entities.end();++it){
        delete (*it);
    }
    m_entities.clear();
}