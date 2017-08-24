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
 * Keyboard Condition
*/

#ifndef AC_KeyboardCondition_hpp
#define AC_KeyboardCondition_hpp


#include "../kernel/AK_Condition.hpp"

class AC_KeyboardCondition:public AK_Condition{
    private:
        char m_key;
        bool m_allkeys;
        
    public:
        
        AC_KeyboardCondition();
        AC_KeyboardCondition(const AC_KeyboardCondition &der);

		int Parse(xml_node<> *comp_node);
        int Parse(const AIO_XMLToken &token);

        void SetKey(char key);
        char GetKey();
        void SetAllKeys(bool flag);
        bool AllKeys();
};

#endif  
