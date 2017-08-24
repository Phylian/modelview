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


#include "AK_Subsystem.hpp"
#include "AK_SubsystemManager.hpp"


AK_Subsystem::AK_Subsystem():m_name("UNDEFINED"),
							m_should_update(true),
                            m_ssmanager(NULL){}


int AK_Subsystem::_init_(AK_SubsystemManager *ssmngr){
    if(!ssmngr)
        return 0;
        
    m_ssmanager = ssmngr;
    
    return 1;
}


AK_Subsystem::~AK_Subsystem(){

}


void AK_Subsystem::SetName(const string &name){
    m_name = name;
}


string AK_Subsystem::GetName(){
    return m_name;
}

