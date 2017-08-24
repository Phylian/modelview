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
#include "AK_ScriptCall.hpp"
#include "AK_Entity.hpp"

using namespace std;


// Constructor.
// Paràmetres:
//      - scriptname      : Nom de l'script a cridar.
//      - comp            : Component que crida l'script.
//

AK_ScriptCall::AK_ScriptCall(const string &scriptname, AK_Entity *caller):
                                                m_scriptname(scriptname),
                                                m_caller(caller){
}



// Obté el nom de l'script a cridar.
// Paràmetres:
//      - (cap)
// Retorna:
//      - El nom de l'script.

string AK_ScriptCall::GetScriptName() const{
    return m_scriptname;
}



// Obté el component que ha cridat l'script.
// Paràmetres:
//      - (cap)
// Retorna:
//      - El component que ha cridat l'script.

AK_Entity* AK_ScriptCall::GetCaller() const{
    return m_caller;
}
