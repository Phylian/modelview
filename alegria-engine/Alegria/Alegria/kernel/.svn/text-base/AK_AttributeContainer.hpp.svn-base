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



#ifndef AK_AttributeContainer_hpp
#define AK_AttributeContainer_hpp

#include <string>
#include <map>

using namespace std;

class AK_Attribute;

/// Manages the attributes of an archtype/entity.
//
/// Both archtypes and entities derive from this class
/// and you must use it to allocate new attributes on them.
class AK_AttributeContainer{
    protected:
        map<string, AK_Attribute*>      m_attributes; 

    public:
        virtual ~AK_AttributeContainer();
        
        /// Creates a new attribute on the container.
        //
        /// Must specify the name which will identify the attribute
        /// and the type (v.g AK_ATTR_INT)
        /// Returns a pointer to the created attribute.
        AK_Attribute* AddAttribute(const string &name, int type);

        /// Creates a new attribute on the container from another attribute.
        AK_Attribute* AddAttribute(const string &name, AK_Attribute *attr);

        /// Removes an attribute from the container and deallocates it.
        int RemoveAttribute(const string &name);

        /// Returns a pointer to the attribute with the given name.
        AK_Attribute* GetAttribute(const string &name);

        /// Returns a pointer to the map containing the attributes.
        //
        /// Used by the Entity Manager to iterate through the map
        /// in order to copy all the attributes from an archtype
        /// to a newly created entity.
        const map<string,AK_Attribute*>* GetAttributes();
};

#endif
