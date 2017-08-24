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
#include "AIO_SoundManager.hpp"
#include "AIO_Sound.hpp"

AIO_SoundManager::AIO_SoundManager(){}



AIO_SoundManager::~AIO_SoundManager(){
    EraseAll();
}



AIO_Sound* AIO_SoundManager::LoadSound(AIO_Sound* sound){

    if(GetSound(sound->GetName()))
        return 0;
    
    

    m_sounds[sound->GetName()] = sound;

    return sound;
}

AIO_Sound* AIO_SoundManager::GetSound(const string &name){

    map<string,AIO_Sound*>::iterator it;

    it = m_sounds.find(name);

    if(it==m_sounds.end()){
        return 0;
    }

    return it->second;
}

unsigned int AIO_SoundManager::GetNumSounds(){
    return m_sounds.size();
}

void AIO_SoundManager::AttachSounds(ALuint source){
	map<string,AIO_Sound*>::iterator it;
	for(it = m_sounds.begin();it!=m_sounds.end();++it){
		alSourcei(source,AL_BUFFER,(*it).second->GetBufferName());	
	}
}


void AIO_SoundManager::EraseAll(){
    while(!m_sounds.empty()){
        map<string,AIO_Sound*>::iterator it = m_sounds.begin();
        delete it->second;
        m_sounds.erase(it);
    }   
}
