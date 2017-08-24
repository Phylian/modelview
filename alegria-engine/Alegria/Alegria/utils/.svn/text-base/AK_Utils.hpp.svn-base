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

#ifndef AK_Utils_hpp
#define AK_Utils_hpp

#include <iostream>
using namespace std;

struct Float2d{
    float x;
    float y;
    
    Float2d();
    Float2d(const Float2d &der);
    Float2d & operator =(const Float2d &der);
    Float2d & operator +(const Float2d &der);
    Float2d & operator -(const Float2d &der);
    Float2d & operator *(int z);
    Float2d & operator /(int z);
};

struct Int2d{
    int x;
    int y;
    
    Int2d();
    Int2d(const Int2d &der);
    Int2d & operator =(const Int2d &der);
    Int2d & operator +(const Int2d &der);
    Int2d & operator -(const Int2d &der);
    Int2d & operator *(int z);
    Int2d & operator /(int z);
};

int String2Int(const string &str);
float String2Float(const string &str);
void StringSplit(const string &str, string &str1, string &str2);
#endif