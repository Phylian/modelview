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
 * Physics Condition
*/

#ifndef AC_PhysicsCondition_hpp
#define AC_PhysicsCondition_hpp

#include <string>
#include "../kernel/AK_Condition.hpp"

using namespace std;

class AC_PhysicsCondition:public AK_Condition{
    private:
        string m_attr;
        
    public:
        
        AC_PhysicsCondition();
        AC_PhysicsCondition(const AC_PhysicsCondition &der);

		int Parse(xml_node<> *comp_node);
        int Parse(const AIO_XMLToken &token);
        string GetAttribute();



};

#endif  
