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

#ifndef AIO_SoundManager_hpp
#define AIO_SoundManager_hpp

#include <map>
#include <AL/al.h>

using namespace std;

class AIO_Sound;

class AIO_SoundManager{
    private:
        map<string,AIO_Sound*>          m_sounds;
        
    public:
        AIO_SoundManager();
        ~AIO_SoundManager();

        //AIO_Image* LoadImageK(const string &filename);
        AIO_Sound* LoadSound(AIO_Sound* sound);

        AIO_Sound* GetSound(const string &name);
		void	   AttachSounds(ALuint source);

        unsigned int        GetNumSounds();

        void                EraseAll();

};

#endif