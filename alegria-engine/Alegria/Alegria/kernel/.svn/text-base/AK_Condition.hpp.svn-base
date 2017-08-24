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



#ifndef AK_Condition_hpp
#define AK_Condition_hpp

#include <string>
#include <list>
#include <rapidxml/rapidxml.hpp>
#include "../io/AIO_XMLToken.hpp"

using namespace std;
using namespace rapidxml;

class AK_Trigger;
class AK_Component;

/// Base class. Component-specific conditions must
/// be derived from this class.
class AK_Condition{
    private:
        bool            m_positive;
        AK_Component*   m_owner;
        
       
        int             m_past_ticks;
        
        bool            m_falsed;
    
    protected:
        string          m_name;
        int             m_omega;
        bool            m_inv;

    public:

        AK_Condition();

        /// The condition analyzes a token and modifies 
        /// it's own parameters if needed. (deprecated)
        virtual int Parse(const AIO_XMLToken &token);

		/// The component parses the information from
		/// an XML node.
		virtual int Parse(xml_node<> *cond_node);

        /// Returns whether the condition is positive 
        /// (i.e. it's fulfilled) or not.
        /// This will determine if a trigger is activated.
        bool IsPositive();

        /// Specifies if the condition is fulfilled.
        /// Must be used only by the associated subsystem
        /// which will pass through the ticks elapsed
        /// from the last call. Ticks are used to
        /// apply the omega parameter.
        void SetPositive(bool flag, int ticks);

        /// Returns the component the condition belongs to.
        AK_Component* GetOwner();

        /// Specifies the component the condition belongs to.
        void SetOwner(AK_Component* owner);

        /// Returns the name of the condition.
        /// A condition is identified both by its name and the type
        /// of the component it belongs to.
        string GetName();

        /// Specifies the name of the condition.
        void SetName(const string &name);

        /// Returns if the condition is inverted.
        bool GetInv();
};
#endif 
