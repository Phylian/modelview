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



#ifndef AK_Trigger_hpp
#define AK_Trigger_hpp

#include <string>
#include <list>
#include <rapidxml/rapidxml.hpp>
#include "../io/AIO_XMLToken.hpp"

using namespace std;
using namespace rapidxml;

class AK_Entity;
class AK_Component;
class AK_Condition;
class AK_ScriptQueue;


/// A container of conditions which, if fulfilled (are positive)
/// will generate a script call.
class AK_Trigger{
    public:
        struct Condition{
            string        type;    // Component en el que està la condició
            string        name;    // Posició dins del component
            AK_Condition* cond;    // Punter a la condició

            Condition(const string &t, const string &n, AK_Condition* c);
            Condition(const Condition &dcond);
            int Parse(const AIO_XMLToken &token);
        };
        
    private:
        AK_Entity           *m_owner;
        list<Condition>     m_conditions;
        string              m_scriptname;
        

    public:
        
        AK_Trigger();

        /// This constructor is very important as it will
        /// initialize correctly the conditions of the trigger.
        AK_Trigger(const AK_Trigger &trigger, AK_Entity* owner);

        virtual ~AK_Trigger();


        /// Assigns an owner to the trigger.
        void SetOwner(AK_Entity *owner);

        /// Returns the entity the trigger belongs to.
        AK_Entity* GetOwner();

        /// Returns the name of the script that must me called
        /// if the trigger is activated.
        string GetScript();

        /// The trigger analyzes a token and modifies 
        /// it's own parameters if needed.
        int Parse(const AIO_XMLToken &token);

		/// The trigger parses the information from
		/// an XML node.
		int Parse(xml_node<> *trig_node);

        /// Adds a new condition to the trigger.
        int AddCondition(const string &type, const string &name, AK_Condition* cond);

        /// Adds a new condition to the trigger based on another condition.
        int AddCondition(const Condition &elem);

        /// Returns if the trigger is positive 
        /// (i.e. all the conditions are positive)
        bool IsPositive();

};
#endif  