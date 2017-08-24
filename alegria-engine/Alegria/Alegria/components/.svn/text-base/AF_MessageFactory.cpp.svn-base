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

#include "AF_MessageFactory.hpp"
#include "AC_Message.hpp"

AF_MessageFactory::AF_MessageFactory(AS_Message *asmsg):m_asmsg(asmsg){}

AK_Component* AF_MessageFactory::CreateComponent(){
    AK_Component *comp = new AC_Message(m_asmsg);

    return comp;
}

AK_Component* AF_MessageFactory::CreateComponent(AK_Component *comp){
    AC_Message *aux = static_cast<AC_Message*>(comp);
    
    AK_Component *newcomp = new AC_Message(*aux);

    return newcomp;
}

string AF_MessageFactory::GetType(){
     return string("Message");
}
