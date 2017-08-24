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

#include "AK_Archtype.hpp"



AK_Archtype::AK_Archtype():AK_TriggerContainer(0),
                          m_count(0){}

string AK_Archtype::GetName(){
    return m_name;
}

void AK_Archtype::SetName(const string &name){
    m_name = name;
}



// Increases m_count so must be called each time (and only once!) 
// an entity is created from this archtype.
string AK_Archtype::GetNewID(){
    string name;
    name = m_name;
    name+= ".";
    char aux[50];
    sprintf(aux,"%d",m_count);
    name+=aux;
    m_count++;
    return name;
}

unsigned int AK_Archtype::GetCount(){
    return m_count;
}
