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



#ifndef AS_Physics_hpp
#define AS_Physics_hpp

#include <string>
#include <list>

#include "../kernel/AK_Subsystem.hpp"
#include "../Box2d/Box2D.h"


using namespace std;

class AK_Component;
class AC_Physics;


class AS_Physics:public AK_Subsystem, public b2ContactListener{
    private:
        list<AK_Component*> m_components;

        b2World* m_world;

        float m_dt;

        bool m_init;
    public:
        AS_Physics();
        ~AS_Physics();

        virtual void BeginContact(b2Contact* contact);
        virtual void EndContact(b2Contact* contact);
        virtual void PreSolve(b2Contact* contact, const b2Manifold* oldManifold);
        virtual void PostSolve(b2Contact* contact, const b2ContactImpulse* impulse);

        void Update(int ticks, double ms);
        int Register(AK_Component *comp); // Registra el component al subsistema
        void RegisterEntity(AK_Entity *ent);
        
        void Init();

        Float2d GetGravity();
        void    SetGravity(float x, float y);

        
        void Clean();
};

#endif 
