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

#include <list>
#include "AK_Trigger.hpp"
#include "AK_Entity.hpp"
#include "AK_Condition.hpp"
#include "AK_Component.hpp"

using namespace std;


AK_Trigger::Condition::Condition(const string &t, const string &n, AK_Condition* c):type(t),name(n),cond(c){}

AK_Trigger::Condition::Condition(const Condition &dcond){
    name = dcond.name;
    type = dcond.type;
    cond = dcond.cond;
}



int AK_Trigger::Condition::Parse(const AIO_XMLToken &token){
    if(token.Name()=="name"){
        name = token.Value();
    }else if(token.Name()=="type"){
        type = token.Value();
    }else{
        return 0;
    }

    return 1;
}



AK_Trigger::AK_Trigger():m_owner(0){}


// En el constructor de copia agafem les condicions equivalents de l'altre trigger en la nostra entitat mitjançant el tipus i el nom

AK_Trigger::AK_Trigger(const AK_Trigger &trigger, AK_Entity* owner):m_owner(owner){
    list<Condition>::const_iterator it;
  
    for(it=trigger.m_conditions.begin();it!=trigger.m_conditions.end();++it){

        AK_Component *comp = m_owner->GetComponent((*it).type); 
        AK_Condition *cond = comp->GetCondition((*it).name);
        cond->SetOwner(comp);
        AddCondition((*it).type, (*it).name, cond);
    }
    
    m_scriptname = trigger.m_scriptname;
}

AK_Trigger::~AK_Trigger(){
    m_conditions.clear();
}
void AK_Trigger::SetOwner(AK_Entity *owner){
    m_owner=owner;
}

AK_Entity* AK_Trigger::GetOwner(){
    return m_owner;
}

string AK_Trigger::GetScript(){
    return m_scriptname;
}

int AK_Trigger::Parse(xml_node<> *trig_node){
	m_scriptname = trig_node->first_attribute("script")->value();

	for (xml_node<> * cond_node = trig_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling()){
		string type = cond_node->first_attribute("type")->value();
		string name = cond_node->first_attribute("name")->value();
		AddCondition(type,name,0);
	}

	return 1;
}

int AK_Trigger::Parse(const AIO_XMLToken &token){
    if(token.Name()=="script"){
        m_scriptname = token.Value();
    }else{
        return 0;
    }

    return 1;
}

int AK_Trigger::AddCondition(const string &type, const string &name, AK_Condition* cond){
    m_conditions.push_back(Condition(type,name,cond));

    return 1;
}

bool AK_Trigger::IsPositive(){
    list<Condition>::const_iterator it;
  
    for(it=m_conditions.begin();it!=m_conditions.end();++it){
        if(!(*it).cond->IsPositive())
            return false;
    }

    return true;
}
