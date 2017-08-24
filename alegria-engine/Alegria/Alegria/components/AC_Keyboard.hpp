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
 * Keyboard Component
*/

#ifndef AC_Keyboard_hpp
#define AC_Keyboard_hpp


#include "../kernel/AK_Component.hpp"

class AK_Condition;

class AC_Keyboard:public AK_Component{
    private:
        bool *m_keys;
        //_APY_OBJECT_HEADER

    public:
        AC_Keyboard();
        AC_Keyboard(const AC_Keyboard &der);
        

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        bool* GetKeys();
        void SetKeys(bool *keys);

        bool IsPressed(char key);
        bool IsAnyPressed();

        PyObject* CreateProxy();
        /*
        // Python
        static PyObject* _apy_IsPressed(PyObject *self, PyObject *args, PyObject *kwds);
        
        string _apy_GetRepr();*/
};


#endif  
