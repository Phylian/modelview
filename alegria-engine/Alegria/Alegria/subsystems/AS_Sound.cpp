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





#include "../kernel/AK_Component.hpp"
#include "../components/AC_Sound.hpp"
#include "../io/AIO_Sound.hpp"
#include "../kernel/AK_Entity.hpp"
#include "../io/AIO_SoundManager.hpp"

#include "AS_Sound.hpp"

#include <iostream>

using namespace std;

const ALfloat AS_Sound::_listener_pos[3]={0.0,0.0,0.0};
const ALfloat AS_Sound::_listener_vel[3]={0.0,0.0,0.0};
const ALfloat AS_Sound::_listener_or[6]={0.0,0.0,1.0,0.0,1.0,0.0};

const ALfloat AS_Sound::_source_pos[]={ 0.0, 0.0, 0.0};
const ALfloat AS_Sound::_source_vel[]={ 0.0, 0.0, 0.0};


AS_Sound::AS_Sound(AIO_SoundManager *manager):m_components(0),m_sound_manager(manager),init(false){
    SetShouldUpdate(true);
	SetName("Sound");

}

AS_Sound::~AS_Sound(){
    m_components.clear();
}

void AS_Sound::Update(int ticks, double ms){

    
    
    

    list<AC_Sound*>::iterator it;

    it=m_components.begin();

    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            ++it;
        }
    }
}

int AS_Sound::Register(AK_Component *comp){ // Registra el component al subsistema
    if(comp->GetType()!="Sound")
        return 0;
    else{
        AC_Sound *soundcomp = static_cast<AC_Sound*>(comp);
        soundcomp->ConnectSoundSubsystem(this);
        m_components.push_back(soundcomp);
    }
    return 1;
}

void AS_Sound::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Sound");
    
    if(aux)
        Register(aux);
}



int AS_Sound::Init(){
	
	alGetError();
	m_device = alcOpenDevice(NULL);


	if(m_device){
		m_context = alcCreateContext(m_device, NULL);

		alcMakeContextCurrent(m_context);
		
	}

	for(int i=0;i<MAX_SOURCES;i++){
		alGenSources(MAX_SOURCES,m_sources);
	}

	return 1;
}

void AS_Sound::Clean(){
    list<AC_Sound*>::iterator it;

	
    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }
    m_components.clear();
    
	
    
}

int AS_Sound::GetFreeSource(){
	for(int i=0;i<MAX_SOURCES;i++){
		ALint state;
		alGetSourcei(m_sources[i], AL_SOURCE_STATE, &state);
		if(state != AL_PLAYING)
			return i;
	}

	return -1;
}

void AS_Sound::PlaySoundK(const string &name){
	AIO_Sound *sound = m_sound_manager->GetSound(name);
	if(sound){
		int index = GetFreeSource();
		if(index!=-1){
			alSourcei(m_sources[index],AL_BUFFER,sound->GetBufferName());
			alSourcei(m_sources[index],AL_LOOPING,AL_FALSE);
			alSourcePlay(m_sources[index]);
		}
		
	}
}

void AS_Sound::PlaySoundLoop(const string &name){
	AIO_Sound *sound = m_sound_manager->GetSound(name);
	if(sound){
		alSourcei(sound->GetSourceName(),AL_LOOPING,AL_TRUE);
		alSourcePlay(sound->GetSourceName());
	}
}

void AS_Sound::StopSound(const string &name){
	AIO_Sound *sound = m_sound_manager->GetSound(name);
	if(sound){
		alSourceStop(sound->GetSourceName());
	}
}