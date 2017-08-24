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

#include <Windows.h>
#include <gl/GL.h>

#include "../kernel/AK_Component.hpp"
#include "../components/AC_Physics.hpp"
#include "../kernel/AK_Entity.hpp"
#include "../kernel/AK_SubsystemManager.hpp"
#include "AS_Physics.hpp"

#include <iostream>

using namespace std;

AS_Physics::AS_Physics():m_dt(1/80.0){
    SetName("Physics");
    //m_dt = 0.016;
    m_init = false;
   
}

AS_Physics::~AS_Physics(){
    m_components.clear();
}

void AS_Physics::BeginContact(b2Contact* contact){
    
    b2Vec2 point;

    b2Fixture *fixtureA = contact->GetFixtureA();
    b2Fixture *fixtureB = contact->GetFixtureB();
    b2Body* bodyA = contact->GetFixtureA()->GetBody();
    b2Body* bodyB = contact->GetFixtureB()->GetBody();
	

    if(fixtureA->IsSensor() || fixtureB->IsSensor()){
        b2Shape* shapeA = fixtureA->GetShape();
        b2Shape* shapeB = fixtureB->GetShape();



        b2Manifold manifold;
        const b2Transform xfA = bodyA->GetTransform();
        const b2Transform xfB = bodyB->GetTransform();

        contact->Evaluate(&manifold, xfA, xfB);

        b2WorldManifold worldManifold;
        worldManifold.Initialize(&manifold, xfA, shapeA->m_radius, xfB, shapeB->m_radius);
      
        point = worldManifold.points[0]; 

        
            
    }else{
        b2WorldManifold wman;
        contact->GetWorldManifold(&wman);
        point = wman.points[0];
        
        
    }

    
    
    AC_Physics *compA = (AC_Physics*)bodyA->GetUserData();
    AC_Physics *compB = (AC_Physics*)bodyB->GetUserData();
    
    compA->AddColPoint(point);
    compA->AddColEntity(compB->GetOwner());

    compB->AddColPoint(point);
    compB->AddColEntity(compA->GetOwner());

    
}

void AS_Physics::EndContact(b2Contact* contact){
	b2Fixture *fixtureA = contact->GetFixtureA();
    b2Fixture *fixtureB = contact->GetFixtureB();
    b2Body* bodyA = contact->GetFixtureA()->GetBody();
    b2Body* bodyB = contact->GetFixtureB()->GetBody();

	AC_Physics *compA = (AC_Physics*)bodyA->GetUserData();
    AC_Physics *compB = (AC_Physics*)bodyB->GetUserData();

	compA->CleanCollisions(compB->GetOwner());
	compB->CleanCollisions(compA->GetOwner());
}
void AS_Physics::PreSolve(b2Contact* contact, const b2Manifold* oldManifold){}
void AS_Physics::PostSolve(b2Contact* contact, const b2ContactImpulse* impulse){}

void AS_Physics::Update(int ticks, double ms){

    list<AK_Component*>::iterator it;

    it=m_components.begin();

    while(it!=m_components.end()){
        AC_Physics *comp = static_cast<AC_Physics*>(*it);
        if((*it)->IsErased()){
            m_world->DestroyBody(comp->GetBody());
            delete (*it);
            it = m_components.erase(it);
        }else{ 
            //comp->CleanCollisions();   
			comp->m_previous_position = comp->GetBody()->GetPosition();
			comp->m_previous_rotation = comp->GetRotation();
            ++it;
        }
        
    }
	if(m_components.empty())
		return;

    //float ms=(float)(ticks*m_ssmanager->GetOmega())/1000; // Conversion from ticks to ms
    //cout << "Ms: " << ms << endl;
    double sec = ms/1000.0;
    //cout << ms << endl;
    
    /*while(sec > 0.0){
        double deltaTime = min(sec, m_dt);
        sec -= deltaTime;
        if(sec < m_dt){
            sec = sec;
        }
        if(sec < 1.0/600){
            deltaTime += sec;
            sec = 0;
        }
        m_world->Step(deltaTime, 13, 8);
        
    }*/
	m_world->Step(sec, 13, 8);
    it=m_components.begin();
    while(it!=m_components.end()){
        AC_Physics *comp = static_cast<AC_Physics*>(*it); 
        comp->UpdateConditions(ticks);
            
        ++it;
    }
}

int AS_Physics::Register(AK_Component *comp){ // Registra el component al subsistema
    if(comp->GetType()!="Physics")
        return 0;
    
    if(!m_init)
        Init();
    AC_Physics *physicscomp = static_cast<AC_Physics*>(comp);
    b2BodyDef *bdef = physicscomp->GetBodyDef();
    Float2d pos = comp->GetOwner()->GetPositionRaw();
    bdef->position.Set(pos.x,pos.y);
    bdef->angle = (comp->GetOwner()->GetRotationRaw())*b2_pi/180;

    b2Body *body = m_world->CreateBody(bdef);
    
    int shape = physicscomp->GetShape();
    
    b2FixtureDef *fdef = physicscomp->GetFixtureDef();
    b2PolygonShape pshape; 
    b2CircleShape cshape; cshape.m_radius=physicscomp->GetShapeSizeX()*comp->GetOwner()->GetSize()/2;
    if(shape==0){ //box
		pshape.SetAsBox(physicscomp->GetShapeSizeX()*comp->GetOwner()->GetSize()/2,physicscomp->GetShapeSizeY()*comp->GetOwner()->GetSize()/2);
        fdef->shape = &pshape;
    }else if(shape==2){ //polygon
        b2Vec2 *points = physicscomp->GetShapePoints();
        int npoints = physicscomp->GetNumPoints();
        pshape.Set(points, npoints);
        fdef->shape = &pshape;

        delete[] points;
    }else{ //circle    
        fdef->shape = &cshape;
    }

	
    
    body->CreateFixture(fdef);
    physicscomp->Init(body);
    physicscomp->ConnectSubsystem(this);
    m_components.push_back(physicscomp);

    return 1;
    
}

void AS_Physics::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Physics");
    
    if(aux)
        Register(aux);
}



void AS_Physics::Init(){
    m_world = new b2World(b2Vec2(0,0), false);
    m_world->SetContactListener(this);

    m_init = true;
}

Float2d AS_Physics::GetGravity(){
    b2Vec2 vec = m_world->GetGravity();

    Float2d grav; 
    grav.x = vec.x; grav.y = vec.y;

    return grav;
}

void AS_Physics::SetGravity(float x, float y){
    m_world->SetGravity(b2Vec2(x,y));
}


void AS_Physics::Clean(){
    list<AK_Component*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();

    delete m_world;

    m_init = false;

    Init();
}