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


#ifndef AK_ComponentFactoryBase_hpp
#define AK_ComponentFactoryBase_hpp

#include <string>

using namespace std;

class AK_Component;

/// Base class for the component factories which will allocate
/// new instances of a specific component type.
class AK_ComponentFactoryBase{
    public:

        /// Creates a new instance of a component of the
        /// factory's type.
        virtual AK_Component* CreateComponent()=0;

        /// Creates a new component from another given component
        /// (works as some kind of copy constructor).
        virtual AK_Component* CreateComponent(AK_Component *comp)=0;  

        /// Returns the type of the components the factory will instantiate.
        /// (v.g "Render" or "Message")
        virtual string GetType()=0;
};

#endif
