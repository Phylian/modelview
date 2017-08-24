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



#ifndef AC_Path_hpp
#define AC_Path_hpp


#include "../kernel/AK_Component.hpp"

class AC_Path;

class AC_Path:public AK_Component{
    private:
        

    public:
        AC_Path();
        AC_Path(const AC_Path &der);
        

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        PyObject* CreateProxy();
       
};


#endif  
