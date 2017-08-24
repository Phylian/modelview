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

#include "AC_MouseCondition.hpp"


AC_MouseCondition::AC_MouseCondition():m_type(0){}

AC_MouseCondition::AC_MouseCondition(const AC_MouseCondition &der):m_type(der.m_type){
    m_name = der.m_name;
    m_omega = der.m_omega;
    m_inv = der.m_inv;
}

int AC_MouseCondition::Parse(xml_node<> *cond_node){
	string type = cond_node->first_attribute("type")->value();

	if(type=="left_button"){
        SetType(MOUSE_LB);
    }else if(type=="mid_button"){
        SetType(MOUSE_MB);
    }else if(type=="right_button"){
        SetType(MOUSE_RB);
    }else if(type=="movement"){
        SetType(MOUSE_MOVE);
    }else{
        return 0;
    }

	return AK_Condition::Parse(cond_node);
}

int AC_MouseCondition::Parse(const AIO_XMLToken &token){
    if(AK_Condition::Parse(token)){
        return 1;
    }

    if(token.Name()=="type"){
        if(token.Value()=="left_button"){
            SetType(MOUSE_LB);
        }else if(token.Value()=="mid_button"){
            SetType(MOUSE_MB);
        }else if(token.Value()=="right_button"){
            SetType(MOUSE_RB);
        }else if(token.Value()=="movement"){
            SetType(MOUSE_MOVE);
        }else{
            return 0;
        }
    }else{
        return 0;
    }

    return 1;
 
}

void AC_MouseCondition::SetType(int type){
    m_type = type;
}

int AC_MouseCondition::GetType(){
    return m_type;
}