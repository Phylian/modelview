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


#ifndef AK_ComponentContainer_hpp
#define AK_ComponentContainer_hpp

#include <string>
#include <map>
#include <list>
using namespace std;

class AK_Component;

/// Manages the components of an archtype/entity.
/// Both archtypes and entities derive from this class.
class AK_ComponentContainer{
    protected:
        typedef map<string, AK_Component*> ComponentMap;
        
        ComponentMap      m_component_map; // Taula hash de components indexats pel nom.




    public:
        AK_ComponentContainer();
        virtual ~AK_ComponentContainer();

        /// Add a new component to the container. The component must be 
        /// created beforehand through a component factory.
        /// The container will take care of deallocating the memory.
        /// A container can have only a component of each type.
        int AddComponent(AK_Component *comp);

        /// Returns a pointer to the component of the given type.
        AK_Component* GetComponent(const string &type);

        /// Returns a pointer to the map containing the components.
        /// Used by the Entity Manager to iterate through the map
        /// in order to copy all the components from an archtype
        /// to a newly created entity.
        const map<string, AK_Component*>* GetComponents();
};

#endif //AK_ComponentContainer_hpp
