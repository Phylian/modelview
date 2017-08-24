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
#include "AC_Particle.hpp"
#include "../kernel/AK_Condition.hpp"
//#include "../components/AC_KeyboardCondition.hpp"
//#include "../proxies/APY_KeyboardCompProxy.hpp"
#include "../utils/AK_Utils.hpp"
#include "../kernel/AK_Entity.hpp"
#include "../Box2D/Box2D.h"
#include <cmath>
#include <iostream>

using namespace std;

AC_Particle::AC_Particle():m_lifetime(0),m_velocity(0),m_animate(0),m_fading(0),m_current_frame(0),m_scaling(0){
    SetType("Particle");
}

AC_Particle::AC_Particle(const AC_Particle &der){
    SetType("Particle");

    m_lifetime = der.m_lifetime;
    m_velocity = der.m_velocity;
    m_animate = der.m_animate;
    m_fading = der.m_fading;
    m_current_frame = der.m_current_frame;
    m_scaling = der.m_scaling;

    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }

}

int AC_Particle::GetLifetime(){
    return m_lifetime;
}

void AC_Particle::SetLifetime(int time){
    m_lifetime = time;
}

float AC_Particle::GetVelocity(){
    return m_velocity;
}

bool AC_Particle::GetFading(){
    return m_fading;
}

void AC_Particle::SetFading(bool fading){
    m_fading = fading;
}

void AC_Particle::SetVelocity(float vel){
    m_velocity = vel;
}

bool AC_Particle::IsAnimated(){
    return m_animate;
}

void AC_Particle::SetAnimated(bool animated){
    m_animate = animated;
}

void AC_Particle::SetScaling(float scaling){
    m_scaling = scaling;
}

float AC_Particle::GetScaling(){
    return m_scaling;
}

int AC_Particle::GetCurrentFrame(){
    return m_current_frame;
}

void AC_Particle::Step(){
	m_velx*=0.995;
	m_vely*=0.995;
    m_current_frame++;
}

void AC_Particle::UpdateVelComps(){
   
    float vel = m_velocity;
    m_rad_rot = m_owner->GetRotation()*b2_pi/180; //en radians
    m_velx = sin(m_rad_rot)*vel;
    m_vely = -cos(m_rad_rot)*vel;
}

AK_Condition* AC_Particle::CreateCondition(){
    /*AC_KeyboardCondition *ncond = new AC_KeyboardCondition();
    m_conds.push_back(ncond);

    return ncond;*/

    return 0;
}

AK_Condition* AC_Particle::CreateCondition(const AK_Condition &cond){
    /*const AC_KeyboardCondition *old = static_cast<const AC_KeyboardCondition*>(&cond);
    AC_KeyboardCondition *ncond = new AC_KeyboardCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;*/

    return 0;
}

void AC_Particle::UpdateConditions(int ticks){

    /*
    list<AK_Condition*>::iterator it;
    
    for(it=m_conds.begin();it!=m_conds.end();++it){
        AC_KeyboardCondition *cond = static_cast<AC_KeyboardCondition*>(*it);
        if(cond->AllKeys()){
            if(IsAnyPressed())
                cond->SetPositive(true,ticks);
            else
                cond->SetPositive(false,ticks);
        }else{
            if(IsPressed(cond->GetKey()))
                cond->SetPositive(true,ticks);
            else
                cond->SetPositive(false,ticks);
        }
    }*/
}

int AC_Particle::Parse(xml_node<> *comp_node){
	

	return 1;
}

int AC_Particle::Parse(const AIO_XMLToken &token){

    if(token.Name()=="lifetime"){
        m_lifetime = String2Int(token.Value());
    }else if(token.Name()=="velocity"){
        m_velocity = String2Float(token.Value());
    }else if(token.Name()=="animate"){
        if(token.Value()=="true" || token.Value()=="1")
            m_animate = true;
    }else if(token.Name()=="fade"){
        if(token.Value()=="true" || token.Value()=="1")
            m_fading = true;
    }else{
        return 0;
    }

    return 1;
}


PyObject* AC_Particle::CreateProxy(){
    /*
    APY_KeyboardCompProxy* aux = PyObject_New(APY_KeyboardCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;*/

    return 0;
}

