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
#include "AC_Sound.hpp"
#include "../kernel/AK_Condition.hpp"
#include "../proxies/APY_SoundCompProxy.hpp"
#include "../subsystems/AS_Sound.hpp"
#include <iostream>

using namespace std;

AC_Sound::AC_Sound(){
    SetType("Sound");
}

AC_Sound::AC_Sound(const AC_Sound &der){
    SetType("Sound");

}

void AC_Sound::PlaySoundK(const string &name){
	m_sound_subsystem->PlaySoundK(name);
}

void AC_Sound::PlaySoundLoop(const string &name){
	m_sound_subsystem->PlaySoundLoop(name);
}

void AC_Sound::StopSound(const string &name){
	m_sound_subsystem->StopSound(name);
}


AK_Condition* AC_Sound::CreateCondition(){
    return 0;
}

AK_Condition* AC_Sound::CreateCondition(const AK_Condition &cond){
	return 0;
}

void AC_Sound::UpdateConditions(int ticks){
    return;
}



int AC_Sound::Parse(xml_node<> *comp_node){
	for (xml_node<> * cond_node = comp_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling()){
		AK_Condition* cond = CreateCondition();
		cond->Parse(cond_node);
	}

	return 1;
}

int AC_Sound::Parse(const AIO_XMLToken &token){
    return 0;
}

void AC_Sound::ConnectSoundSubsystem(AS_Sound *sound){
	m_sound_subsystem = sound;
}


PyObject* AC_Sound::CreateProxy(){
    APY_SoundCompProxy* aux = PyObject_New(APY_SoundCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;



}


