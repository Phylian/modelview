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

#include "AF_RenderFactory.hpp"
#include "AC_Render.hpp"

AF_RenderFactory::AF_RenderFactory(AIO_ImageManager* imgmngr):m_imgmngr(imgmngr){}

AK_Component* AF_RenderFactory::CreateComponent(){
    AK_Component *comp = new AC_Render(m_imgmngr);

    return comp;
}

AK_Component* AF_RenderFactory::CreateComponent(AK_Component *comp){
    AC_Render *aux = static_cast<AC_Render*>(comp);
    
    AK_Component *newcomp = new AC_Render(*aux);

    return newcomp;
}

string AF_RenderFactory::GetType(){
     return string("Render");
}
