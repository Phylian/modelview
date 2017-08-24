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

/**
 * Message Factory
*/

#ifndef AF_MessageFactory_hpp
#define AF_MessageFactory_hpp

#include "../kernel/AK_ComponentFactoryBase.hpp"
#include "../kernel/AK_Component.hpp"
#include "../subsystems/AS_Message.hpp"

class AF_MessageFactory:public AK_ComponentFactoryBase{
    private:
        AS_Message *m_asmsg;
    public:
        AF_MessageFactory(AS_Message *asmsg);

        AK_Component* CreateComponent();
        AK_Component* CreateComponent(AK_Component *comp);  
        string GetType();
};


#endif
