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


#ifndef AK_Archtype_hpp
#define AK_Archtype_hpp

#include <string>
#include "AK_ComponentContainer.hpp"
#include "AK_TriggerContainer.hpp"
#include "AK_AttributeContainer.hpp"
using namespace std;

/// An archtype used as model to create real-time entities. 
//
/// An Archtype represents the definition of an object.
/// It consists of a group of Components, Triggers and Attributes.
/// Every new real-time object (from now on: Entity) is created from an Archtype.
/// Archtypes are defined in the scene file and are identified by 
/// a name.
class AK_Archtype:public AK_ComponentContainer, public AK_TriggerContainer, public AK_AttributeContainer{
    private:
        string            m_name;       
        unsigned int      m_count;      
    
    public:
        
        /// Default constructor.
        AK_Archtype();
        
        /// Returns the name of the archtype.
        //
        /// This name works as identifier.
        string GetName();

        /// Returns a new ID for an entity created from this archtype.
        //
        /// An ID has the following format: "Name.Count", where Name is 
        /// the name of the archtype and Count is the number of entities
        /// created from this archtype.
        /// @warning Increases the Count variable (m_count).
        string GetNewID();

        /// Assigns a name to the archtype. 
        //
        ///This name works as an identifier.
        void SetName(const string &name);

        /// Returns the number of entities created from this archtype (initially 0)
        unsigned int GetCount();
};

#endif //AK_Archtype_hpp
