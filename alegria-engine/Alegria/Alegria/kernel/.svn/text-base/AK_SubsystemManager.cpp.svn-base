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

#include "AK_SubsystemManager.hpp"
#include "AK_Subsystem.hpp"
#include <iostream>
using namespace std;



AK_SubsystemManager::AK_SubsystemManager():m_ms(0),m_omega(1){}


AK_SubsystemManager::~AK_SubsystemManager(){
    list<AK_Subsystem*>::iterator it;

    for(it=m_subsystems.begin();it!=m_subsystems.end();++it)
        delete(*it);
}



AK_Subsystem* AK_SubsystemManager::GetSubsystem(const string &name){
    list<AK_Subsystem*>::iterator it;
    
    for(it=m_subsystems.begin();it!=m_subsystems.end();++it){
        if((*it)->GetName()==name){
            return (*it);
        }
    }

    return 0;
}

int AK_SubsystemManager::RegisterSubsystem(AK_Subsystem* subsystem){
    if(!subsystem)
        return 0;
        
    list<AK_Subsystem*>::iterator it;
    
    for(it=m_subsystems.begin();it!=m_subsystems.end();++it)
        if((*it)==subsystem)
            return 0;
    
    subsystem->_init_(this); // Inicialitza el subsistema.
    m_subsystems.push_back(subsystem);
    return 1;
}

int AK_SubsystemManager::RemoveSubsystem(const string &name){
    list<AK_Subsystem*>::iterator it;
    
    for(it=m_subsystems.begin();it!=m_subsystems.end();++it){
        if((*it)->GetName()==name){
            delete (*it);           // IMPORTANT: Primer cridem al destructor del subsistema!
            m_subsystems.erase(it);
            return 1;
        }
    }
    
    return 0;
}



int AK_SubsystemManager::GetOmega(){
    return m_omega;
}

void AK_SubsystemManager::Update(double ms){
    list<AK_Subsystem*>::iterator it;
    
    

    int ticks = (ms+m_ms)/m_omega;
    m_ms = (ms+m_ms)-ticks*m_omega;
    
	
    for(it=m_subsystems.begin();it!=m_subsystems.end();++it)
		if((*it)->ShouldUpdate())
			(*it)->Update(ticks,ms);
}
 
int AK_SubsystemManager::AddEntity(AK_Entity* ent){
    list<AK_Subsystem*>::iterator it;

    for(it=m_subsystems.begin();it!=m_subsystems.end();++it)
        (*it)->RegisterEntity(ent);


    return 1;
}

void AK_SubsystemManager::Clean(){
    list<AK_Subsystem*>::iterator it;

    for(it=m_subsystems.begin();it!=m_subsystems.end();++it)
        (*it)->Clean();
}

