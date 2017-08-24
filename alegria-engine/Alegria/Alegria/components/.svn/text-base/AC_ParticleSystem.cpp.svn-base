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
#include "AC_ParticleSystem.hpp"
#include "../kernel/AK_Condition.hpp"
//#include "../components/AC_KeyboardCondition.hpp"
//#include "../proxies/APY_KeyboardCompProxy.hpp"
#include "../utils/AK_Utils.hpp"
#include <iostream>

using namespace std;

AC_ParticleSystem::AC_ParticleSystem():m_totalparticles(0),m_lifetime(0),m_particle_velocity(0),m_rotate_particles(1),
                                        m_particle_lifetime(0),m_particle_randomness(0),m_running(0),m_paused(0),
                                        m_current_frame(0),m_autokill(0),m_loop(0),m_particle_size(1),m_interval(1),m_particle_fading(0),m_particle_animation(0),m_particle_scaling(0),m_velocity_interpolation(0){
    SetType("ParticleSystem");
}

AC_ParticleSystem::AC_ParticleSystem(const AC_ParticleSystem &der){
    SetType("ParticleSystem");

    m_particle = der.m_particle;
    m_totalparticles = der.m_totalparticles;
    m_lifetime = der.m_lifetime;
    m_particle_lifetime = der.m_particle_lifetime;
    m_particle_velocity = der.m_particle_velocity;
    m_particle_size = der.m_particle_size;
    m_particle_randomness = der.m_particle_randomness;
    m_particle_fading = der.m_particle_fading;
    m_particle_scaling = der.m_particle_scaling;
    m_particle_animation = der.m_particle_animation;
    m_rotate_particles = der.m_rotate_particles;
    m_velocity_interpolation = der.m_velocity_interpolation;
    m_running = der.m_running;
    m_paused = der.m_paused;
    m_current_frame = der.m_current_frame;
    m_autokill = der.m_autokill;
    m_loop = der.m_loop;
    m_interval = der.m_interval;


    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }

}


string AC_ParticleSystem::GetParticle(){
    return m_particle;
}
void AC_ParticleSystem::SetParticle(const string &particle){
    m_particle = particle;
}

int AC_ParticleSystem::GetTotalParticles(){
    return m_totalparticles;
}

int AC_ParticleSystem::GetInterval(){
    return m_interval;
}

void AC_ParticleSystem::SetTotalParticles(int total){
    m_totalparticles = total;
}

int AC_ParticleSystem::GetLifetime(){
    return m_lifetime;
}

void AC_ParticleSystem::SetLifetime(int time){
    m_lifetime = time;
}

int AC_ParticleSystem::GetParticleLifetime(){
    return m_particle_lifetime;
}

void AC_ParticleSystem::SetParticleLifetime(int time){
    m_particle_lifetime = time;
}

float AC_ParticleSystem::GetParticleVelocity(){
    return m_particle_velocity;
}

void AC_ParticleSystem::SetParticleVelocity(float vel){
    m_particle_velocity = vel;
}

float AC_ParticleSystem::GetParticleSize(){
    return m_particle_size;
}

void AC_ParticleSystem::SetParticleSize(float size){
    m_particle_size = size;
}

int AC_ParticleSystem::GetParticleRandomness(){
    return m_particle_randomness;
}

void AC_ParticleSystem::SetParticleRandomness(int vel){
    m_particle_randomness = vel;
}

bool AC_ParticleSystem::GetParticleRotation(){
    return m_rotate_particles;
}

void AC_ParticleSystem::SetParticleRotation(bool flag){
    m_rotate_particles = flag;
}

bool AC_ParticleSystem::IsParticleAnimated(){
    return m_particle_animation;
}

bool AC_ParticleSystem::GetParticleFading(){
    return m_particle_fading;
}

float AC_ParticleSystem::GetParticleScaling(){
    return m_particle_scaling;
}

float AC_ParticleSystem::GetVelocityInterpolation(){
    return m_velocity_interpolation;
}

bool AC_ParticleSystem::IsRunning(){
    return m_running;
}

int AC_ParticleSystem::GetCurrentFrame(){
    return m_current_frame;
}

bool AC_ParticleSystem::GetAutokill(){
    return m_autokill;
}

bool AC_ParticleSystem::GetLoop(){
    return m_loop;
}

void AC_ParticleSystem::Start(){
    m_running = true;
    m_paused = false;
}

void AC_ParticleSystem::Step(){
    m_current_frame++;
}
void AC_ParticleSystem::Pause(){
    m_running = false;
    m_paused = true;
}
void AC_ParticleSystem::Stop(){
    m_running = false;
    m_paused = false;
    m_current_frame = 0;
}

AK_Condition* AC_ParticleSystem::CreateCondition(){
    /*AC_KeyboardCondition *ncond = new AC_KeyboardCondition();
    m_conds.push_back(ncond);

    return ncond;*/

    return 0;
}

AK_Condition* AC_ParticleSystem::CreateCondition(const AK_Condition &cond){
    /*const AC_KeyboardCondition *old = static_cast<const AC_KeyboardCondition*>(&cond);
    AC_KeyboardCondition *ncond = new AC_KeyboardCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;*/

    return 0;
}

void AC_ParticleSystem::UpdateConditions(int ticks){

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

int AC_ParticleSystem::Parse(xml_node<> *comp_node){
	m_particle = comp_node->first_attribute("particle")->value();
	m_totalparticles = atoi(comp_node->first_attribute("total_particles")->value());
	m_interval = atoi(comp_node->first_attribute("interval")->value());
	m_lifetime = atoi(comp_node->first_attribute("lifetime")->value());
	m_particle_lifetime = atoi(comp_node->first_attribute("particle_lifetime")->value());
	m_particle_velocity = atof(comp_node->first_attribute("particle_velocity")->value());
	m_particle_size = atof(comp_node->first_attribute("particle_size")->value());
	m_particle_randomness = atoi(comp_node->first_attribute("randomness")->value());
	m_rotate_particles = atoi(comp_node->first_attribute("rotate_particles")->value());
	m_running = atoi(comp_node->first_attribute("autostart")->value());
	m_autokill = atoi(comp_node->first_attribute("autokill")->value());
	m_loop = atoi(comp_node->first_attribute("loop")->value());
	m_particle_animation = atoi(comp_node->first_attribute("animate")->value());
	m_particle_fading = atoi(comp_node->first_attribute("fading")->value());
	m_particle_scaling = atof(comp_node->first_attribute("scaling")->value());
	m_velocity_interpolation = atof(comp_node->first_attribute("velocity_interpolation")->value());
	
	return 1;
}

int AC_ParticleSystem::Parse(const AIO_XMLToken &token){

    if(token.Name()=="particle"){
        m_particle = token.Value();
    }else if(token.Name()=="total_particles"){
        m_totalparticles = String2Int(token.Value());
    }else if(token.Name()=="interval"){
        m_interval = String2Int(token.Value());
    }else if(token.Name()=="lifetime"){
        m_lifetime = String2Int(token.Value());
    }else if(token.Name()=="particle_lifetime"){
        m_particle_lifetime = String2Int(token.Value());
    }else if(token.Name()=="particle_velocity"){
        m_particle_velocity = String2Float(token.Value());
    }else if(token.Name()=="particle_size"){
        m_particle_size = String2Float(token.Value());
    }else if(token.Name()=="randomness"){
        m_particle_randomness = String2Int(token.Value());
    }else if(token.Name()=="rotate_particles"){
        if(token.Value()=="true" || token.Value()=="1")
            m_rotate_particles = true;
        else
            m_rotate_particles = false;
    }else if(token.Name()=="autostart"){
        if(token.Value()=="true" || token.Value()=="1")
            m_running = true;
    }else if(token.Name()=="autokill"){
        if(token.Value()=="true" || token.Value()=="1")
            m_autokill = true;
    }else if(token.Name()=="loop"){
        if(token.Value()=="true" || token.Value()=="1")
            m_loop = true;
    }else if(token.Name()=="animate"){
        if(token.Value()=="true" || token.Value()=="1")
            m_particle_animation = true;
    }else if(token.Name()=="fading"){
        if(token.Value()=="true" || token.Value()=="1")
            m_particle_fading = true;
    }else if(token.Name()=="scaling"){
        m_particle_scaling = String2Float(token.Value());
    }else if(token.Name()=="velocity_interpolation"){
        m_velocity_interpolation = String2Float(token.Value());
    }else{
        return 0;
    }

    return 1;
}


PyObject* AC_ParticleSystem::CreateProxy(){
    /*
    APY_KeyboardCompProxy* aux = PyObject_New(APY_KeyboardCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;*/

    return 0;
}

