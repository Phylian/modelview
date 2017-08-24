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

#include "AK_Condition.hpp"
#include "../utils/AK_Utils.hpp"
#include <iostream>

using namespace std;

AK_Condition::AK_Condition(){
    m_positive = false;
    m_omega = 1;
    m_past_ticks = 0;
    m_inv = false;
    m_falsed = true;
}

int AK_Condition::Parse(xml_node<> *cond_node){
	m_name = cond_node->first_attribute("name")->value();
	m_inv = atoi(cond_node->first_attribute("inv")->value());
	m_omega = atoi(cond_node->first_attribute("omega")->value());

	return 1;
}

int AK_Condition::Parse(const AIO_XMLToken &token){
    if(token.Name()=="name"){
        m_name = token.Value();
    }else if(token.Name()=="omega"){
        m_omega = String2Int(token.Value());     
    }else if(token.Name()=="inv"){
        if(token.Value()=="true"||token.Value()=="1")
            m_inv = true;
        else
            m_inv = false;
    }else{
        return 0;
    }

    return 1;
}

bool AK_Condition::IsPositive(){
    return m_positive;
}

void AK_Condition::SetPositive(bool flag, int ticks){
    bool _flag=false;
    
    if(m_inv){
        if(flag==true)
            _flag = false;
        else
            _flag = true;
    }else{
        _flag = flag;
    }


    if(m_omega>0){
        if(_flag==false){
            m_positive = false;
            m_past_ticks = m_omega;  
        }else{
            int newticks = m_past_ticks+ticks;
            if(m_omega>200)
				cout<<m_omega<< " " << newticks << endl;
            if(newticks>=m_omega){
                m_positive = _flag;
                m_past_ticks = newticks%m_omega;  
            }else{
                m_positive = false;
                m_past_ticks = newticks;
            }
        }
    }else{
        if(_flag==false){
            m_positive = false;
            m_falsed = true;
        }else{
            if(m_falsed==true){
                m_positive=true;
                m_falsed = false;
            }else{
                m_positive = false;
            }
        }
    }
}

bool AK_Condition::GetInv(){
    return m_inv;

}

AK_Component* AK_Condition::GetOwner(){
    return m_owner;
}

void AK_Condition::SetOwner(AK_Component* owner){
    m_owner = owner;
}

void AK_Condition::SetName(const string &name){
    m_name = name;
}

string AK_Condition::GetName(){
    return m_name;
}