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

/**
 * Classe base per als subsystemes. És abstracta.
*/

#ifndef AS_Sound_hpp
#define AS_Sound_hpp


#include <string>
#include <list>
#include <vector>

#include "../kernel/AK_Subsystem.hpp"
#include <AL/al.h>
#include <AL/alc.h>

using namespace std;

class AK_Component;
class AC_Sound;
class AK_Window;
class AIO_SoundManager;

const int MAX_SOURCES = 32;


class AS_Sound:public AK_Subsystem{
    private:
        list<AC_Sound*>			m_components;
		AIO_SoundManager		*m_sound_manager;
		ALuint					m_sources[MAX_SOURCES];
		ALCdevice               *m_device;
		ALCcontext				*m_context;

		static const ALfloat _listener_pos[3];
		static const ALfloat _listener_vel[3];
		static const ALfloat _listener_or[6];

		static const ALfloat _source_pos[];
		static const ALfloat _source_vel[];
        


        bool init;

    public:
        AS_Sound(AIO_SoundManager *manager);
        ~AS_Sound();

        void Update(int ticks, double ms);
        int Register(AK_Component *comp); // Registra el component al subsistema
        void RegisterEntity(AK_Entity *ent);

        int  Init();
        
        void Clean();

		int GetFreeSource();

		void PlaySoundK(const string &name);
		void PlaySoundLoop(const string &name);
		void StopSound(const string &name);
};

#endif 
