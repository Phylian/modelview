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

#include <string>
#include <iostream>
#include <stdlib.h>
#include "AC_Physics.hpp"
#include "../kernel/AK_Condition.hpp"
#include "AC_PhysicsCondition.hpp"
#include "../proxies/APY_PhysicsCompProxy.hpp"
#include "../utils/AK_Utils.hpp"
#include "../subsystems/AS_Physics.hpp"
#include <iostream>

using namespace std;

AC_Physics::AC_Physics():m_body(0),m_shape(0),m_shape_size_x(1),m_shape_size_y(1){
    SetType("Physics");
    m_fixdef.density=1;
}

AC_Physics::AC_Physics(const AC_Physics &der){
    SetType("Physics");

    m_bdef = der.m_bdef;
    m_fixdef = der.m_fixdef;
    m_shape = der.m_shape;
    m_shape_size_x = der.m_shape_size_x;
	m_shape_size_y = der.m_shape_size_y;

    for(int i=0;i<der.m_points.size();i++){
        m_points.push_back(der.m_points[i]);
    }

    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }
}

AC_Physics::~AC_Physics(){
    m_points.clear();
    m_col_entities.clear();
    m_col_points.clear();
}

b2Body* AC_Physics::GetBody(){
    return m_body;
    
}

b2BodyDef* AC_Physics::GetBodyDef(){
    return &m_bdef;

}

b2FixtureDef* AC_Physics::GetFixtureDef(){
    return &m_fixdef;
    
}

Float2d AC_Physics::GetPosition(){
    Float2d pos;

    if(m_body){
        b2Vec2 vec = m_body->GetPosition();
        pos.x = vec.x;
        pos.y = vec.y;  
    }

    return pos;
}

float AC_Physics::GetRotation(){
    float rot=0;

    if(m_body){
        rot = m_body->GetAngle();
        rot*=180;
        rot/=b2_pi;
    }

    return rot;
}

float AC_Physics::GetShapeSizeX(){
    return m_shape_size_x;
}

float AC_Physics::GetShapeSizeY(){
    return m_shape_size_y;
}

int AC_Physics::GetShape(){
    return m_shape;
}

b2Vec2* AC_Physics::GetShapePoints(){
    b2Vec2 *vec;

    int npoints = m_points.size();

    vec = new b2Vec2[npoints];

    for(int i=0;i<npoints;i++){
        vec[i] = m_points[i];
		vec[i].x *=m_shape_size_x*m_owner->GetSize();
		vec[i].y *=m_shape_size_y*m_owner->GetSize();
    }

    return vec;
}

int AC_Physics::GetNumPoints(){
    return m_points.size();
}

void AC_Physics::SetPosition(float x, float y){
	m_body->SetAwake(true);
    b2Transform transform = m_body->GetTransform();
    
    m_body->SetTransform(b2Vec2(x,y),transform.GetAngle());
}

void AC_Physics::SetRotation(float angle){
	m_body->SetAwake(true);
    b2Transform transform = m_body->GetTransform();
    float rot = angle;
    rot/=180;
    rot*=b2_pi;
    m_body->SetTransform(transform.position,angle);
}

void AC_Physics::ApplyForce(float x, float y, bool local){
	m_body->SetAwake(true);
    b2Vec2 f(x,y),p;
    if(local)
        f = m_body->GetWorldVector(f);
    
    p = m_body->GetWorldCenter();
    m_body->ApplyForce(f,p);
}

void AC_Physics::ApplyTorque(float torque){
	m_body->SetAwake(true);
    m_body->ApplyTorque(torque);
}

void AC_Physics::ApplyLinearImpulse(float x, float y, bool local){
	m_body->SetAwake(true);
    b2Vec2 f(x,y),p;
    if(local)
        f = m_body->GetWorldVector(f);
    
    p = m_body->GetWorldCenter();
    m_body->ApplyLinearImpulse(f,p); 
}

Float2d AC_Physics::GetLinearVelocity(){
    b2Vec2 vec = m_body->GetLinearVelocity();
    Float2d vec2; 
    vec2.x = vec.x; vec2.y = vec.y;
    return vec2;
}

void AC_Physics::SetLinearVelocity(float x, float y){
	m_body->SetAwake(true);
    b2Vec2 v(x,y);
    m_body->SetLinearVelocity(v);
}

float AC_Physics::GetAngularVelocity(){
    return m_body->GetAngularVelocity();
}

void AC_Physics::SetAngularVelocity(float w){
	m_body->SetAwake(true);
    m_body->SetAngularVelocity(w);
}

void AC_Physics::ApplyAngularImpulse(float impulse){
	m_body->SetAwake(true);
    m_body->ApplyAngularImpulse(impulse);

}

vector<AK_Entity*>* AC_Physics::GetColEntities(){
    return &m_col_entities;
}
vector<b2Vec2>*     AC_Physics::GetColPoints(){
    return &m_col_points;
}

void AC_Physics::AddColEntity(AK_Entity* ent){
    m_col_entities.push_back(ent);
}

void AC_Physics::AddColPoint(b2Vec2 point){
    m_col_points.push_back(point);
}

void AC_Physics::CleanCollisions(AK_Entity *ent){
	//vector<AK_Entity*> m_col_entities;
    //    vector<b2Vec2>     m_col_points;
	vector<AK_Entity*>::iterator it;
	int i=0,j=0;
    for(it=m_col_entities.begin(); it!=m_col_entities.end(); ++it){
		if((*it)==ent){
			m_col_entities.erase(it);
			break;
		}else{
			i++;
		}
    }

	
	if(i>=m_col_entities.size()+1)
		return;

	vector<b2Vec2>::iterator it2; 
	
	for(it2=m_col_points.begin(); it2!=m_col_points.end(); ++it2){
		if(i==j){
			m_col_points.erase(it2);
			break;
		}else{
			j++;
		}
    }

	
    //m_col_entities.clear();
    //m_col_points.clear();
}

bool AC_Physics::TestPoint(float x, float y){
    return m_body->GetFixtureList()[0].TestPoint(b2Vec2(x,y));
}

Float2d AC_Physics::GetGravity(){
    return m_asp->GetGravity();
}

void AC_Physics::SetGravity(float x, float y){
	m_body->SetAwake(true);
    m_asp->SetGravity(x,y);
}

void AC_Physics::SetSensor(bool sensor){
	m_body->GetFixtureList()[0].SetSensor(sensor);
}

void AC_Physics::SetBodyType(int type){
	if(type)
		m_body->SetType(b2BodyType::b2_dynamicBody);
	else
		m_body->SetType(b2BodyType::b2_staticBody);
}


void AC_Physics::Init(b2Body *body){
    m_body = body;
    body->SetUserData(this);
	

}

AK_Condition* AC_Physics::CreateCondition(){
    AC_PhysicsCondition *ncond = new AC_PhysicsCondition();
    m_conds.push_back(ncond);

    return ncond;
}

AK_Condition* AC_Physics::CreateCondition(const AK_Condition &cond){
    const AC_PhysicsCondition *old = static_cast<const AC_PhysicsCondition*>(&cond);
    AC_PhysicsCondition *ncond = new AC_PhysicsCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;
}

void AC_Physics::UpdateConditions(int ticks){
    

    list<AK_Condition*>::iterator it;
    
    for(it=m_conds.begin();it!=m_conds.end();++it){

        AC_PhysicsCondition *cond = static_cast<AC_PhysicsCondition*>(*it);
        string attrname = cond->GetAttribute();

        int num = m_col_entities.size();

        if(num==0)
            cond->SetPositive(false,ticks);
        else{
            for(int i=0;i<num;i++){
                AK_Attribute *attr = m_col_entities[i]->GetAttribute(attrname);
                if(attr||attrname==""){
                    cond->SetPositive(true,ticks);
                    break;
                }else{
                    cond->SetPositive(false,ticks);
                }
            }
        }
    }
}

int AC_Physics::Parse(xml_node<> *comp_node){

	string a = (comp_node->first_attribute("friction")->value());
	m_fixdef.friction = atof(comp_node->first_attribute("friction")->value());
	m_fixdef.density = atof(comp_node->first_attribute("density")->value());
	m_fixdef.restitution = atof(comp_node->first_attribute("restitution")->value());
	m_fixdef.isSensor = atoi(comp_node->first_attribute("sensor")->value());
	string shape = comp_node->first_attribute("shape")->value();
	if(shape=="circle")
        m_shape = 1;
    else if(shape=="box")
        m_shape = 0;
    else if(shape=="polygon")
        m_shape = 2;
	if(comp_node->first_attribute("shape_size")){
		m_shape_size_x = atof(comp_node->first_attribute("shape_size")->value());
		m_shape_size_y = atof(comp_node->first_attribute("shape_size")->value());
	}else{
		m_shape_size_x = atof(comp_node->first_attribute("shape_size_x")->value());
		m_shape_size_y = atof(comp_node->first_attribute("shape_size_y")->value());
	}


	m_bdef.linearDamping = atof(comp_node->first_attribute("linear_damping")->value());
	m_bdef.angularDamping = atof(comp_node->first_attribute("angular_damping")->value());
	string type = comp_node->first_attribute("type")->value();
	if(type=="dynamic")
        m_bdef.type=b2_dynamicBody;
    else if(type=="static")
        m_bdef.type=b2_staticBody;
	m_bdef.fixedRotation = atoi(comp_node->first_attribute("fixed_rotation")->value());
	m_bdef.bullet = atoi(comp_node->first_attribute("bullet")->value());
	
	m_fixdef.density = atof(comp_node->first_attribute("density")->value());

	
	for (xml_node<> * point_node = comp_node->first_node("point");point_node; point_node = point_node->next_sibling("point")){
		float posx = atof(point_node->first_attribute("x")->value());
		float posy = atof(point_node->first_attribute("y")->value());
		m_points.push_back(b2Vec2(posx,posy));
	}

	for (xml_node<> * cond_node = comp_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling("condition")){
		AK_Condition* cond = CreateCondition();
		cond->Parse(cond_node);
	}

	return 1;
}

int AC_Physics::Parse(const AIO_XMLToken &token){
    if(token.Name()=="friction"){
        m_fixdef.friction = String2Float(token.Value());
    }else if(token.Name()=="density"){
        m_fixdef.density = String2Float(token.Value());
    }else if(token.Name()=="restitution"){
        m_fixdef.restitution = String2Float(token.Value());
    }else if(token.Name()=="sensor"){
        if(token.Value() == "true" || token.Value()=="1")
            m_fixdef.isSensor = true;
        else
            m_fixdef.isSensor = false;
    }else if(token.Name()=="shape"){
        if(token.Value()=="circle")
            m_shape = 1;
        else if(token.Value()=="box")
            m_shape = 0;
        else if(token.Value()=="polygon")
            m_shape = 2;
    }else if(token.Name()=="shape_size"){
        m_shape_size_x = String2Float(token.Value());
    }else if(token.Name()=="linear_damping"){
        m_bdef.linearDamping = String2Float(token.Value());
    }else if(token.Name()=="angular_damping"){
        m_bdef.angularDamping = String2Float(token.Value());
    }else if(token.Name()=="type"){
        if(token.Value()=="dynamic")
            m_bdef.type=b2_dynamicBody;
        else if(token.Value()=="static")
            m_bdef.type=b2_staticBody;
    }else if(token.Name()=="fixed_rotation"){
        if(token.Value() == "true" || token.Value()=="1")
            m_bdef.fixedRotation=true;
    }else if(token.Name()=="bullet"){
        if(token.Value() == "true" || token.Value()=="1")
            m_bdef.bullet=true;
    }else if(token.Name()=="point"){
        string sposx, sposy;
        float posx,posy;
        StringSplit(token.Value(),sposx,sposy);
        posx = String2Float(sposx);
        posy = String2Float(sposy);
        m_points.push_back(b2Vec2(posx,posy));
    }else{
        return 0;
    }

    return 1;
}


void AC_Physics::ConnectSubsystem(AS_Physics *asp){
    m_asp = asp;
}


PyObject* AC_Physics::CreateProxy(){
    
    APY_PhysicsCompProxy* aux = PyObject_New(APY_PhysicsCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;

    
}

