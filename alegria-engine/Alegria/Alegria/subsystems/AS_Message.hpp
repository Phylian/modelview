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



#ifndef AS_Message_hpp
#define AS_Message_hpp

#include <string>
#include <list>


#include "../kernel/AK_Subsystem.hpp"
#include "../kernel/AK_Message.hpp"
#include "../utils/AK_Utils.hpp"
#include <Windows.h>

using namespace std;

class AK_Component;
class AC_Message;

class AS_Message:public AK_Subsystem{
    private:
        list<AC_Message*> m_components;
        list<AK_Message> m_messages;

    public:
        AS_Message();
        ~AS_Message();

        void PutMessage(const AK_Message &msg); 

        

        void Update(int ticks, double ms);
        int Register(AK_Component *comp); 
        void RegisterEntity(AK_Entity *ent);
        void Clean();

};

#endif 
