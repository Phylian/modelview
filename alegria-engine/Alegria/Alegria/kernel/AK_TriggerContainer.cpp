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
#include "AK_TriggerContainer.hpp"
#include "AK_Trigger.hpp"
#include "AK_ScriptQueue.hpp"
using namespace std;


// Constructor

AK_TriggerContainer::AK_TriggerContainer(AK_ScriptQueue* scriptqueue):m_scriptqueue(scriptqueue){}



// Destructor virtual

AK_TriggerContainer::~AK_TriggerContainer(){

    list<AK_Trigger*>::iterator it;

    for(it = m_triggers.begin(); it != m_triggers.end(); ++it){
        delete (*it);
    }
}

int AK_TriggerContainer::AddTrigger(AK_Trigger* trigger){
    if(!trigger)
        return 0;

    m_triggers.push_back(trigger);

    return 1;
}


const list<AK_Trigger*>* AK_TriggerContainer::GetTriggers(){
    return &m_triggers;
}

void AK_TriggerContainer::UpdateTriggers(){
    list<AK_Trigger*>::iterator it;

    for(it = m_triggers.begin(); it != m_triggers.end(); ++it){
        if((*it)->IsPositive()){
            m_scriptqueue->Push((*it)->GetScript(), (*it)->GetOwner());
        }
    }
}