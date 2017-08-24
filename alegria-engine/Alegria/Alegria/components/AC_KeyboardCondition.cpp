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

#include "AC_KeyboardCondition.hpp"
#include "..\utils\AK_Utils.hpp"

AC_KeyboardCondition::AC_KeyboardCondition():m_allkeys(false){}

AC_KeyboardCondition::AC_KeyboardCondition(const AC_KeyboardCondition &der):m_allkeys(der.m_allkeys),m_key(der.m_key){
    m_name = der.m_name;
    m_omega = der.m_omega;
    m_inv = der.m_inv;
}

int AC_KeyboardCondition::Parse(xml_node<> *cond_node){
	m_key = atoi(cond_node->first_attribute("key")->value());
	m_allkeys = atoi(cond_node->first_attribute("all")->value());

	return AK_Condition::Parse(cond_node);
}

int AC_KeyboardCondition::Parse(const AIO_XMLToken &token){
    if(AK_Condition::Parse(token)){
        return 1;
    }

    if(token.Name()=="key"){
        char key = String2Int(token.Value());
        SetKey(key);
    }else if(token.Name()=="all"){
        if(token.Value()=="true"||token.Value()=="1")
            SetAllKeys(true);
        else
            SetAllKeys(false);
    }else{
        return 0;
    }

    return 1;
 
}

void AC_KeyboardCondition::SetKey(char key){
    m_key = key;
}

char AC_KeyboardCondition::GetKey(){
    return m_key;
}

void AC_KeyboardCondition::SetAllKeys(bool flag){
    m_allkeys = flag;
}

bool AC_KeyboardCondition::AllKeys(){
    return m_allkeys;
}