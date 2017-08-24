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

#ifndef AS_Mouse_hpp
#define AS_Mouse_hpp

#include <string>
#include <list>

#include "../kernel/AK_Subsystem.hpp"
#include "../kernel/AK_Window.hpp"
#include "../subsystems/AS_Render.hpp"
#include <Windows.h>

using namespace std;

class AK_Component;

class AS_Mouse:public AK_Subsystem{
    private:
        list<AK_Component*> m_components;
        
        __MouseInfo *m_minfo;
        AS_Render *m_render;

    public:
        AS_Mouse(AS_Render *render);

        void Update(int ticks, double ms);
        int Register(AK_Component *comp); // Registra el component al subsistema
        void RegisterEntity(AK_Entity *ent);

        void ConnectMouseInfo(__MouseInfo *minfo);

        void Clean();
};

#endif 
