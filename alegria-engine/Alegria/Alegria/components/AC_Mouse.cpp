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
#include "AC_Mouse.hpp"
#include "../kernel/AK_Condition.hpp"
#include "../components/AC_MouseCondition.hpp"
#include "../proxies/APY_MouseCompProxy.hpp"
#include "../subsystems/AS_Render.hpp"
#include <iostream>

using namespace std;

AC_Mouse::AC_Mouse():m_minfo(0),m_last_x(0),m_last_y(0),m_incr_x(0),m_incr_y(0){
    SetType("Mouse");
}

AC_Mouse::AC_Mouse(const AC_Mouse &der){
    SetType("Mouse");

    m_minfo = der.m_minfo;

    list<AK_Condition*>::const_iterator it;

    for(it=der.m_conds.begin(); it!=der.m_conds.end(); ++it){
        CreateCondition(*(*it));
    }

}

bool AC_Mouse::IsLButtonDown(){
    return m_minfo->mouse_lb;
}

bool AC_Mouse::IsMButtonDown(){
    return m_minfo->mouse_mb;
}

bool AC_Mouse::IsRButtonDown(){
    return m_minfo->mouse_rb;
}

Int2d AC_Mouse::GetMousePos(){
    Int2d pos;

    pos.x = m_minfo->mouse_x;
    pos.y = m_minfo->mouse_y;

    return pos;
}

Float2d AC_Mouse::GetMousePosS(){
    Float2d campos = m_render->GetCameraPosition();
    float resx = m_render->GetResX();
    float resy = m_render->GetResY();
    int winx = m_render->GetWinX();
    int winy = m_render->GetWinY();
    int posx = m_minfo->mouse_x-winx/2;
    int posy = winy-(m_minfo->mouse_y+winy/2);
    float units_per_pixel_x = resx/(winx);
    float units_per_pixel_y = resy/(winy);

    //cout << m_minfo->mouse_x << endl;
    //cout << m_minfo->mouse_y << endl;

    Float2d pos;
    pos.x = posx*units_per_pixel_x+campos.x;
    pos.y = posy*units_per_pixel_y+campos.y;

    return pos;

}

void AC_Mouse::SetMousePos(int x, int y){
    POINT pos;
    pos.x = x; pos.y=y;
    ClientToScreen(m_render->GetWindow()->GetWinHandle(),&pos);
    SetCursorPos(pos.x,pos.y);
}

Int2d AC_Mouse::GetMouseIncrement(){
    Int2d incr;
    incr.x = m_incr_x;
    incr.y = m_incr_y;

    return incr;
}

Float2d AC_Mouse::GetMouseIncrementS(){
    float resx = m_render->GetResX();
    float resy = m_render->GetResY();
    int winx = m_render->GetWinX()-16;
    int winy = m_render->GetWinY()-38;
    float units_per_pixel_x = resx/(winx+1);
    float units_per_pixel_y = resy/(winy+1);

    //cout << m_minfo->mouse_x << endl;
    //cout << m_minfo->mouse_y << endl;

    Float2d incr;
    incr.x = m_incr_x*units_per_pixel_x;
    incr.y = m_incr_y*units_per_pixel_y;

    return incr;
}

void AC_Mouse::ShowMouse(bool show){
    ShowCursor(show);
}

void AC_Mouse::LimitMouse(bool flag){
    if(flag){
        RECT clip;
        HWND hWnd = m_render->GetWindow()->GetWinHandle();
        GetClientRect(hWnd, &clip);
        POINT pt;
        pt.x = 0; pt.y=0;
        ClientToScreen(hWnd,&pt);
        OffsetRect(&clip,pt.x,pt.y);
        ClipCursor(&clip);
    }else{
        RECT clip;
        clip.left=0;
        clip.top=0;
        clip.right=GetSystemMetrics(SM_CXSCREEN);
        clip.bottom=GetSystemMetrics(SM_CYSCREEN);
        ClipCursor(&clip);
    }
}

void AC_Mouse::ConnectRenderSubsystem(AS_Render* render){
    m_render = render;
}

AK_Condition* AC_Mouse::CreateCondition(){
    AC_MouseCondition *ncond = new AC_MouseCondition();
    m_conds.push_back(ncond);

    return ncond;
}

AK_Condition* AC_Mouse::CreateCondition(const AK_Condition &cond){
    const AC_MouseCondition *old = static_cast<const AC_MouseCondition*>(&cond);
    AC_MouseCondition *ncond = new AC_MouseCondition(*old);
    
    m_conds.push_back(ncond);

    return ncond;
}

void AC_Mouse::UpdateConditions(int ticks){
    list<AK_Condition*>::iterator it;
    
    for(it=m_conds.begin();it!=m_conds.end();++it){
        AC_MouseCondition *cond = static_cast<AC_MouseCondition*>(*it);

        int type = cond->GetType();
        Int2d pos = GetMousePos();
      
        switch(type){
            case MOUSE_LB:
                if(IsLButtonDown())
                    cond->SetPositive(true,ticks);
                else
                    cond->SetPositive(false,ticks);
                break;
            case MOUSE_MB:
                if(IsMButtonDown())
                    cond->SetPositive(true,ticks);
                else
                    cond->SetPositive(false,ticks);
                break;
            case MOUSE_RB:
                if(IsRButtonDown())
                    cond->SetPositive(true,ticks);
                else
                    cond->SetPositive(false,ticks);
                break;
            case MOUSE_MOVE:
                if(m_last_x != pos.x || m_last_y != pos.y){
                    cond->SetPositive(true,ticks);
                    m_incr_x = pos.x-m_last_x;
                    m_incr_y = m_last_y-pos.y;
                }else{
                    cond->SetPositive(false,ticks);
                    m_incr_x = 0;
                    m_incr_y = 0;
                }
                break;
        }
        m_last_x = pos.x;
        m_last_y = pos.y;
    }
}

void AC_Mouse::SetMouseInfo(__MouseInfo *minfo){
    m_minfo = minfo;
}

int AC_Mouse::Parse(xml_node<> *comp_node){
	for (xml_node<> * cond_node = comp_node->first_node("condition");cond_node; cond_node = cond_node->next_sibling()){
		AK_Condition* cond = CreateCondition();
		cond->Parse(cond_node);
	}

	return 1;
}

int AC_Mouse::Parse(const AIO_XMLToken &token){
    return 0;
}


PyObject* AC_Mouse::CreateProxy(){
    APY_MouseCompProxy* aux = PyObject_New(APY_MouseCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;

    return 0;
}
