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
#include "AK_Entity.hpp"
#include "AK_Component.hpp"
#include "AK_ScriptQueue.hpp"
#include "../proxies/APY_EntityProxy.hpp"
#include "../components/AC_Physics.hpp"

using namespace std;



// Constructor genèric, inicialitza les variables.
// Paràmetres:
//      - name : Nom que se li assignarà a l'entitat.
//      - scriptqueue : Cola d'scripts.

AK_Entity::AK_Entity(const string &name, AK_ScriptQueue* scriptqueue):AK_TriggerContainer(scriptqueue),
                                         m_name(name),
                                         m_parent(0),
                                         m_dead(false),m_kill_next_frame(0),
                                         m_rotation(0), m_size(1){}



// Destructor, neteja les relacions pares/fills

AK_Entity::~AK_Entity(){
    list<AK_Entity*>::iterator it; 
    
    RemoveParent();
    
 

    if(!m_children.empty()){
        it = m_children.begin();

        while(it!=m_children.end()){
            (*it)->RemoveParent();
            it = m_children.begin();
        }
    }
}



// Obté el nom de l'entitat
// Paràmetres: 
//      (cap)
// Retorna:
//      - El nom de l'entitat.

string AK_Entity::GetName(){
    return m_name;
}



// Retorna si l'entitat deu ser eliminada
// Paràmetres: 
//      (cap)
// Retorna:
//      - True si deu ser eliminada, False en cas contrari.

bool AK_Entity::IsDead(){
    return m_dead;
}



// Marca l'entitat per a eliminar
// Paràmetres: 
//      (cap)
// Retorna:
//      (cap)

void AK_Entity::Kill(){
    m_dead = true;
    
    map<string, AK_Component*>::iterator it;

    for(it = m_component_map.begin();it!=m_component_map.end();++it){
        it->second->Erase();
    }
}

void AK_Entity::KillNextFrame(){
	m_kill_next_frame = 2;
}

void AK_Entity::SetKillNextFrame(int k){
	m_kill_next_frame = k;
}

int AK_Entity::IsKilledNextFrame(){
	return m_kill_next_frame;
}


int AK_Entity::AddComponentK(AK_Component *comp){
    comp->SetOwner(this);

    return AddComponent(comp);
}



// Retorna el pare de l'entitat. 
// Paràmetres: 
//      (cap)
// Retorna:
//      - El pare, NULL si no en te.

AK_Entity* AK_Entity::GetParent(){
    return m_parent;
}



// Assigna un pare a l'entitat. 
// Paràmetres: 
//      - ent : Punter a el que serà el pare.
// Retorna:
//      - 1 si te èxit.
//      - 0 si el paràmetre és NULL o si l'entitat assignada ja és el pare.
//
// Al mateix temps, registra l'entitat com a fill en el pare. 

int AK_Entity::SetParent(AK_Entity *ent){
    if(ent == NULL)
        return 0;
    if(m_parent != ent){
        if(m_parent)
            RemoveParent();
        m_parent = ent;
        ent->AddChild(this);
        return 1;
    }else{
        return 0;
    }
}



// Desactiva el parentesc. Torna 1 si te éxit, 0 en cas contrari (no
// te pare).

int AK_Entity::RemoveParent(){
    if(m_parent){
        m_parent->RemoveChild(this);
        m_parent = NULL;
        return 1;
    }else{
        return 0;
    }
}



// Afegix un fill (sols pot ser cridat desde SetParent() de altra entitat)

int AK_Entity::AddChild(AK_Entity *ent){
    m_children.push_back(ent);
    
    return 1;
}



// Elimina un fill (sols pot ser cridat desde RemoveParent() de altra entitat)
// Torna 1 si s'ha trobat, 0 si no.

int AK_Entity::RemoveChild(AK_Entity *ent){
    if(ent==NULL)
        return 0;
    
    list<AK_Entity*>::iterator it = m_children.begin();
    bool found=false;
    
    while(it!=m_children.end() && !found){
        if((*it)==ent){
            m_children.erase(it);
            found = true;
        }
        ++it;
    }
    
    return found;
}



// Obté el nombre de fills.

int AK_Entity::GetChildrenCount(){
    return m_children.size();
}



// Obté els fills. Copia els punters de la llista (m_children) a un vector 
// de punters, que caldrà lliberar
// (pero no les entitats que conté!) més tard.

AK_Entity** AK_Entity::GetChildren(){
    int count = GetChildrenCount();
    AK_Entity **child_vec = new AK_Entity*[count];
	list<AK_Entity*>::iterator it = m_children.begin();
    
    for(int i=0; i<count; i++, ++it)
        child_vec[i]=(*it);
    
    return child_vec;
}


Float2d AK_Entity::GetPosition(){
    AK_Component *comp = GetComponent("Physics");

    if(!comp)
        return m_position;
    else{
        AC_Physics *p_comp = static_cast<AC_Physics*>(comp);

        return p_comp->GetPosition();
    }

}

Float2d AK_Entity::GetPositionRaw(){
    return m_position;
}

float AK_Entity::GetRotation(){
    AK_Component *comp = GetComponent("Physics");

    if(!comp)
        return m_rotation;
    else{
        AC_Physics *p_comp = static_cast<AC_Physics*>(comp);

        return p_comp->GetRotation();
    }
}


float AK_Entity::GetRotationRaw(){
    return m_rotation;
}

float AK_Entity::GetSize(){
    return m_size;
}

void AK_Entity::SetPosition(float x, float y){
    m_position.x = x;
    m_position.y = y;
}

void AK_Entity::SetPosition(Float2d pos){
    m_position = pos;
}

void AK_Entity::SetRotation(float degrees){
    m_rotation = degrees;
}

void AK_Entity::SetSize(float size){
    m_size = size;
}


PyObject* AK_Entity::CreateProxy(){
    APY_EntityProxy* aux = PyObject_New(APY_EntityProxy,&_type);
    aux->ref=this;
    PyObject *p = (PyObject*)aux;
    return p;
}
