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
#include "AK_ScriptQueue.hpp"
#include "AK_ScriptCall.hpp"


int AK_ScriptQueue::Push(const string &name, AK_Entity* ent){
    if(!ent)    
        return 0;
    m_queue.push(AK_ScriptCall(name,ent));
    return 1;
}

AK_ScriptCall AK_ScriptQueue::Pop(){
    AK_ScriptCall aux = m_queue.front();
    m_queue.pop();
    return aux;
}

int AK_ScriptQueue::Count(){
    return m_queue.size();
}

int AK_ScriptQueue::IsEmpty(){
    return m_queue.empty();
}

void AK_ScriptQueue::Clean(){
    while(!m_queue.empty())
        m_queue.pop();
}