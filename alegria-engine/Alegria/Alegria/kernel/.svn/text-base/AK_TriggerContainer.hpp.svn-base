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

#ifndef AK_TriggerContainer_hpp
#define AK_TriggerContainer_hpp

#include <string>
#include <map>
#include <list>
using namespace std;

class AK_Trigger;
class AK_ScriptQueue;

/// Manages the triggers of an archtype/entity.
/// Both archtypes and entities derive from this class.
class AK_TriggerContainer{
    protected:       
        list<AK_Trigger*>   m_triggers;
        AK_ScriptQueue      *m_scriptqueue;
    public:
        AK_TriggerContainer(AK_ScriptQueue* scriptqueue);
        virtual ~AK_TriggerContainer();

        /// Add a trigger to the container
        int AddTrigger(AK_Trigger* trigger);

        /// Returns a pointer to the list containing the triggers.
        /// Used by the Entity Manager to iterate through the list
        /// in order to copy all the triggers from an archtype
        /// to a newly created entity.
        const list<AK_Trigger*>* GetTriggers();

        /// Checks the triggers and if they are positive 
        /// queue up a new script call.
        void UpdateTriggers();
};

#endif 
