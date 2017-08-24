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

#include "AK_Utils.hpp"

Float2d::Float2d(){
    x = 0;
    y = 0;
}

Float2d::Float2d(const Float2d &der){
    x = der.x;
    y = der.y;
}

Float2d & Float2d::operator =(const Float2d &der){
    x = der.x;
    y = der.y;

    return *this;
}

Float2d & Float2d::operator +(const Float2d &der){
    x += der.x;
    y += der.y;

    return *this;
}

Float2d & Float2d::operator -(const Float2d &der){
    x -= der.x;
    y -= der.y;

    return *this;
}

Float2d & Float2d::operator *(int z){
    x *= z;
    y *= z;

    return *this;
}

Float2d & Float2d::operator /(int z){
    x *= z;
    y *= z;

    return *this;
}

Int2d::Int2d(){
    x = 0;
    y = 0;
}

Int2d::Int2d(const Int2d &der){
    x = der.x;
    y = der.y;
}

Int2d & Int2d::operator =(const Int2d &der){
    x = der.x;
    y = der.y;

    return *this;
}

Int2d & Int2d::operator +(const Int2d &der){
    x += der.x;
    y += der.y;

    return *this;
}

Int2d & Int2d::operator -(const Int2d &der){
    x -= der.x;
    y -= der.y;

    return *this;
}

Int2d & Int2d::operator *(int z){
    x *= z;
    y *= z;

    return *this;
}

Int2d & Int2d::operator /(int z){
    x *= z;
    y *= z;

    return *this;
}


int String2Int(const string &str){
    char *aux = new char[20];
    strcpy(aux,str.c_str());
  
    int result = atoi(aux);
        
    delete[] aux;

    return result;
}

float String2Float(const string &str){
    char *aux = new char[20];
    strcpy(aux,str.c_str());
  
    float result = atof(aux);
        
    delete[] aux;

    return result;
}

void StringSplit(const string &str, string &str1, string &str2){
    int i=0;

    while(str[i]!=' '){
        str1.push_back(str[i]);
        
        i++;
    }
    while(str[i]==' '){
        i++;
    }
    while(str[i]!=' ' && i < str.size()){
        str2.push_back(str[i]);
        i++;
    }

}