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

#ifndef AS_Particle_hpp
#define AS_Particle_hpp

#include <string>
#include <list>

#include "../kernel/AK_Subsystem.hpp"
#include <Windows.h>

using namespace std;

class AC_Particle;

class AS_Particle:public AK_Subsystem{
    private:
        list<AC_Particle*> m_components;

    public:
        AS_Particle();

        void Update(int ticks, double ms);
        int Register(AK_Component *comp); // Registra el component al subsistema
        void RegisterEntity(AK_Entity *ent);
        void Clean();
};

#endif 
