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


#ifndef AK_ScriptCall_hpp
#define AK_ScriptCall_hpp


#include <string>

using namespace std;


class AK_Entity;

/// Represents a call to a script.
/// Instances of this class will be stored
/// in the Script Queue.
class AK_ScriptCall{
    private:
        string        m_scriptname;   // Nom de l'script a cridar
        AK_Entity    *m_caller;   // Component que crida l'script.
        
    public:
        AK_ScriptCall(const string &scriptname, AK_Entity *caller);
        
        /// Returns the name of the script that must be called.
        string GetScriptName() const;

        /// Returns the entity that sent this script call.
        AK_Entity* GetCaller() const;
};

#endif //AK_ScripCall_hpp
