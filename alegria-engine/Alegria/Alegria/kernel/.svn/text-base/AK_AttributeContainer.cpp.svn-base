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
#include "AK_AttributeContainer.hpp"
#include "AK_Attribute.hpp"
#include "../attributes/AA_Int.hpp"
#include "../attributes/AA_Float.hpp"
#include "../attributes/AA_Bool.hpp"
#include "../attributes/AA_String.hpp"

using namespace std;



AK_AttributeContainer::~AK_AttributeContainer(){
    map<string,AK_Attribute*>::iterator it;
    
    for(it = m_attributes.begin(); it != m_attributes.end(); ++it){
        delete (it)->second;
    }

    m_attributes.clear();
}
        
AK_Attribute* AK_AttributeContainer::AddAttribute(const string &name, int type){
    
    AK_Attribute *attr;

    switch(type){
        case AK_ATTR_INT:
            attr = new AA_Int();
            break;
        case AK_ATTR_FLOAT:
            attr = new AA_Float();
            break;
        case AK_ATTR_BOOL:
            attr = new AA_Bool();
            break;
        case AK_ATTR_STR:
            attr = new AA_String();
            break;
        default:
            return 0;
    }
    
    map<string,AK_Attribute*>::iterator it = m_attributes.find(name);
    
    if(it == m_attributes.end()){
        m_attributes.insert(pair<string,AK_Attribute*>(name,attr));
        return attr;
    }else{
        return 0;
    }
}

AK_Attribute* AK_AttributeContainer::AddAttribute(const string &name, AK_Attribute *attr){
    
    AA_Int *aint;
    AA_Float *afloat;
    AA_Bool *abool;
    AA_String *astring;

    AK_Attribute *chosen;

    switch(attr->GetType()){
        case AK_ATTR_INT:
            aint = new AA_Int();
            aint->SetValue(static_cast<AA_Int*>(attr)->GetValue());
            chosen = aint;
            break;
        case AK_ATTR_FLOAT:
            afloat = new AA_Float();
            afloat->SetValue(static_cast<AA_Float*>(attr)->GetValue());
            chosen = afloat;
            break;
        case AK_ATTR_BOOL:
            abool = new AA_Bool();
            abool->SetValue(static_cast<AA_Bool*>(attr)->GetValue());
            chosen = abool;
            break;
        case AK_ATTR_STR:
            astring = new AA_String();
            astring->SetValue(static_cast<AA_String*>(attr)->GetValue());
            chosen = astring;
            break;
        default:
            return 0;
    }
    
    map<string,AK_Attribute*>::iterator it = m_attributes.find(name);
    
    if(it == m_attributes.end()){
        m_attributes.insert(pair<string,AK_Attribute*>(name,chosen));
        return chosen;
    }else{
        return 0;
    }
}

int AK_AttributeContainer::RemoveAttribute(const string &name){
    map<string,AK_Attribute*>::iterator it = m_attributes.find(name);
    
    if(it == m_attributes.end()){
        return 0;
    }else{
        delete(it->second);
        m_attributes.erase(it);
        return 1;
    }
}

AK_Attribute* AK_AttributeContainer::GetAttribute(const string &name){
    map<string,AK_Attribute*>::iterator it = m_attributes.find(name);
    
    if(it == m_attributes.end()){
        return 0;
    }else{
        return (it->second);
    }
}

const map<string,AK_Attribute*>* AK_AttributeContainer::GetAttributes(){
    return &m_attributes;
}