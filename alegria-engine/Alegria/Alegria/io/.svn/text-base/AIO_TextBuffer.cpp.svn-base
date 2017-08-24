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

#include <fstream>
#include <iostream>
#include "AIO_TextBuffer.hpp"

using namespace std;

AIO_TextBuffer::AIO_TextBuffer():m_buffer(0),m_bytecode(0),m_size(0),m_name("AIO_NONE"){}

AIO_TextBuffer::~AIO_TextBuffer(){
    Free();
}

int AIO_TextBuffer::Load(const string& filename, const string &idpath){
    Free();
    
    fstream file(filename);
    m_path = idpath;
    string line;
    string total="";
    while ( file.good() ){
      getline (file,line);
      line+='\n';
      total+=line;
    }
    
    
    

    m_size = total.size();
    m_buffer = new char[m_size+1];

    strcpy(m_buffer,total.c_str());
    
    return 1;
}

int AIO_TextBuffer::Compile(){
    m_bytecode = Py_CompileString(m_buffer, m_name.c_str(), Py_file_input);
    if(!m_bytecode)
        return 0;
    return 1;
}

int AIO_TextBuffer::GetSize() const{
    return m_size;
}

const char* AIO_TextBuffer::GetBuffer() const{
    return m_buffer;
}

PyObject* AIO_TextBuffer::GetByteCode(){
    return m_bytecode;
}

void AIO_TextBuffer::Free(){
    if(m_size>0){
        delete[] m_buffer;
        m_size = 0;
    }
}

string AIO_TextBuffer::GetName(){
    return m_name;
}

void   AIO_TextBuffer::SetName(const string &name){
    m_name = name;
}

int AIO_TextBuffer::Parse(const AIO_XMLToken &token){
    /*if(token.Name()=="path"){
        Load(token.Value());
    }else if(token.Name()=="name"){
        SetName(token.Value());
    }else{
        return 0;
    }*/
	return 0;
}