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

#include "AK_ArchtypeManager.hpp"
#include "AK_Archtype.hpp"


AK_ArchtypeManager::~AK_ArchtypeManager(){
    EraseAllArchtypes();
}


int AK_ArchtypeManager::AddArchtype(AK_Archtype *arch){
    map<string, AK_Archtype*>::iterator it = m_archtypes.find(arch->GetName());

    if(it!=m_archtypes.end())
        return 0;
    
    m_archtypes[arch->GetName()] = arch;
    return 1;
}

AK_Archtype* AK_ArchtypeManager::GetArchtype(const string &name){
    map<string, AK_Archtype*>::iterator it = m_archtypes.find(name);

    if(it == m_archtypes.end())
        return 0;
    return (it->second);
}

AK_Archtype* AK_ArchtypeManager::GetArchtype(int pos){
     map<string, AK_Archtype*>::iterator it;
     int count=0;
     for(it=m_archtypes.begin();it!=m_archtypes.end();++it,++count){
        if(count==pos)
            return (*it).second;
     }
}
int AK_ArchtypeManager::EraseArchtype(const string &name){

    map<string, AK_Archtype*>::iterator it = m_archtypes.find(name);
    
    delete it->second;
    m_archtypes.erase(it);

    return 1;
}

int AK_ArchtypeManager::EraseAllArchtypes(){
    map<string, AK_Archtype*>::iterator it;

    for( it = m_archtypes.begin();it!=m_archtypes.end();++it )
        delete it->second;

    m_archtypes.clear();

    return 1;
}