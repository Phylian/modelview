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
 * Mouse Component
*/

#ifndef AC_Sound_hpp
#define AC_Sound_hpp


#include "../kernel/AK_Component.hpp"

class AS_Sound;

class AC_Sound:public AK_Component{
    private:
        
        AS_Sound *m_sound_subsystem;

    public:
        AC_Sound();
        AC_Sound(const AC_Sound &der);
        

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

		void ConnectSoundSubsystem(AS_Sound* sound);


        void PlaySoundK(const string &name);
		void PlaySoundLoop(const string &name);
		void StopSound(const string &name);
        PyObject* CreateProxy();
       
};


#endif  
