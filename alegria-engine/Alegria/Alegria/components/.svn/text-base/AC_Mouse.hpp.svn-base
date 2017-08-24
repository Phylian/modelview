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

#ifndef AC_Mouse_hpp
#define AC_Mouse_hpp


#include "../kernel/AK_Component.hpp"
#include "../kernel/AK_Window.hpp"
#include "../utils/AK_Utils.hpp"

class AK_Condition;
class AS_Render;

class AC_Mouse:public AK_Component{
    private:
        __MouseInfo *m_minfo;
        int  m_last_x;
        int  m_last_y;
        int  m_incr_x;
        int  m_incr_y;
        AS_Render *m_render;

    public:
        AC_Mouse();
        AC_Mouse(const AC_Mouse &der);
        

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        bool IsLButtonDown();
        bool IsMButtonDown();
        bool IsRButtonDown();

        Int2d GetMousePos();
        Float2d GetMousePosS();
        void SetMousePos(int x, int y);

        Int2d GetMouseIncrement();
        Float2d GetMouseIncrementS();
        void ShowMouse(bool show);

        void LimitMouse(bool flag);

        void ConnectRenderSubsystem(AS_Render* render);
        void SetMouseInfo(__MouseInfo *minfo);
        PyObject* CreateProxy();
       
};


#endif  
