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

#include "AIO_XMLToken.hpp"

AIO_XMLToken::AIO_XMLToken(const string& str,int num):m_num(num){
    int pos;


    if(str.size()<=1){
        m_type = AIO_XML_IS_END;
    }else if(str[0] != '<' && str[str.size()] != '>'){
        m_type = AIO_XML_IS_CORRUPT;

    }else if(str.find('=')!=string::npos){
        m_type = AIO_XML_IS_SINGLE;
        
        pos = 1;

        string aux;
        while(str[pos]==' ')
            pos++;
        while(str[pos]!=' ' && str[pos]!='='){
            aux+=str[pos];
            pos++;
        }
        SetName(aux);

        while(str[pos]==' ' || str[pos]=='=')
            pos++;

        aux.clear();
        while(/*str[pos]!=' ' &&*/ str[pos]!='>'){
            aux+=str[pos];
            pos++;
        }
        int i=aux.size()-1;
        while(aux[i]==' ' && i>=0){
            aux.pop_back();
            i--;
        }
        SetValue(aux);
    }else{
        pos = 1;
        while(str[pos]==' ')
            pos++;
        if(str[pos]=='/'){
            m_type = AIO_XML_IS_CLOSER;
            pos++;
        }else{
            m_type = AIO_XML_IS_OPENER;
            //pos = 1;
        }

        string aux;
        while(str[pos]==' ')
            pos++;
        while(str[pos]!=' ' && str[pos]!='>'){
            aux+=str[pos];
            pos++;
        }
        SetName(aux);
    }   
}

AIO_XMLToken::AIO_XMLToken(unsigned int type):m_type(type),
                                              m_name("UNDEFINED"),
                                              m_value("UNDEFINED"){}

AIO_XMLToken::AIO_XMLToken(const AIO_XMLToken& der):m_type(der.m_type),
                                                    m_name(der.m_name),
                                                    m_value(der.m_value),
                                                    m_num(der.m_num){}

void AIO_XMLToken::SetName(const string &name){
    m_name = name;
}


void AIO_XMLToken::SetValue(const string &value){
    if(m_type==AIO_XML_IS_SINGLE)
        m_value = value;
}

string AIO_XMLToken::Name() const{
    return m_name;
}


string AIO_XMLToken::Value() const{
    return m_value;
}

bool AIO_XMLToken::IsCorrupt() const{
    return m_type == AIO_XML_IS_CORRUPT;
}

bool AIO_XMLToken::IsOpener() const{
    return m_type == AIO_XML_IS_OPENER;
}

bool AIO_XMLToken::IsCloser() const{
    return m_type == AIO_XML_IS_CLOSER;
}

bool AIO_XMLToken::IsSingle() const{
    return m_type == AIO_XML_IS_SINGLE;
}

bool AIO_XMLToken::IsEnd() const{
    return m_type == AIO_XML_IS_END;
}

int AIO_XMLToken::GetNum() const{
    return m_num;
}

