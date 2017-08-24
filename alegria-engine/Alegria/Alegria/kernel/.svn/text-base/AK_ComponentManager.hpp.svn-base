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


#ifndef AK_ComponentManager_hpp
#define AK_ComponentManager_hpp

#include <map>
#include <string>
using namespace std;

class AK_ComponentFactoryBase;
class AK_Component;


/// Registers new component types through factories
/// and creates instances of them.
class AK_ComponentManager{
    private:
        map<string, AK_ComponentFactoryBase*> m_factories;
    
    public:
        ~AK_ComponentManager();

        /// Registers a new component type given its factory.
        /// The factory must be allocated with new beforehand
        /// and the manager will take care of deallocating it 
        /// when appropiate.
        int RegisterComponentType(AK_ComponentFactoryBase *factory);

        /// Removes a specific type through deallocating the
        /// associated factory.
        int RemoveComponentType(const string &type);

        /// Creates a new instance of a specific component type.
        AK_Component* CreateComponent(const string &type);

        /// Creates a copy of a given component.
        AK_Component* CreateComponent(AK_Component* comp);
};

#endif 
