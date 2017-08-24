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

/*
  Physics Box2d Component

  En el component és va guardant la Body Definition. El subsistema de físiques s'encarregará de cridar a Box2d
  per a que cree i inicialitze el Body.

  */


#ifndef AC_Physics_hpp
#define AC_Physics_hpp

#include "../Box2d/Box2D.h"
#include "../kernel/AK_Component.hpp"
#include "../utils/AK_Utils.hpp"
#include "../kernel/AK_Entity.hpp"
#include <vector>

class AK_Condition;
class AS_Physics;

class AC_Physics:public AK_Component{
    private:
        b2BodyDef       m_bdef;
        b2FixtureDef    m_fixdef;
        b2Body          *m_body;
		b2Joint			*m_joint;
        int             m_shape;
        float           m_shape_size_x;
		float           m_shape_size_y;
        vector<b2Vec2>  m_points;

        // Info sobre colisions
        vector<AK_Entity*> m_col_entities;
        vector<b2Vec2>     m_col_points;

		

        AS_Physics         *m_asp;
        
    public:

		b2Vec2			m_previous_position;
		float			m_previous_rotation;

		AC_Physics();  
        AC_Physics(const AC_Physics &der);

        ~AC_Physics();

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        b2Body*    GetBody();
        b2BodyDef* GetBodyDef();  

        b2FixtureDef* GetFixtureDef();  

        Float2d GetPosition();
        float GetRotation();

        
        int GetShape();
        float GetShapeSizeX();
		float GetShapeSizeY();
        b2Vec2* GetShapePoints();
        int GetNumPoints();

        void SetPosition(float x, float y);
        void SetRotation(float angle);
        void ApplyForce(float x, float y, bool local);
        void ApplyTorque(float torque);
        void ApplyLinearImpulse(float x, float y, bool local);
        void ApplyAngularImpulse(float impulse);
        Float2d GetLinearVelocity();
        void SetLinearVelocity(float x, float y);
        float GetAngularVelocity();
        void SetAngularVelocity(float w);
		void SetSensor(bool sensor);
		void SetBodyType(int type);

        void AddColEntity(AK_Entity* ent);
        void AddColPoint(b2Vec2 point);
        vector<AK_Entity*>* GetColEntities();
        vector<b2Vec2>*     GetColPoints();



        bool TestPoint(float x, float y);

        Float2d GetGravity();
        void    SetGravity(float x, float y);

		void CleanCollisions(AK_Entity *ent);

        void Init(b2Body *body);

        void ConnectSubsystem(AS_Physics *asp);

        PyObject* CreateProxy();
};


#endif  
