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


#include "AIO_XMLFile.hpp"
#include "AIO_TextBuffer.hpp"
#include "AIO_XMLToken.hpp"

AIO_XMLFile::AIO_XMLFile(){
    m_it = m_tokenlist.begin();
}

AIO_XMLFile::~AIO_XMLFile(){
    Free();
}

int AIO_XMLFile::Load(const AIO_TextBuffer& text){
    Free();
    
    const char* buffer = text.GetBuffer();
    string aux;
    int pos=0;
    int token =0;
    while(pos<text.GetSize()){
        aux.clear();
        while(buffer[pos]!='<' && pos<text.GetSize())
            pos++;

        while(buffer[pos]!='>' && pos<text.GetSize()){
            aux+=buffer[pos];
            pos++;
        }
        aux+=buffer[pos];
        pos++;

        m_tokenlist.push_back(AIO_XMLToken(aux,token));
        token++;
    }

    return 1;
    
}

int AIO_XMLFile::Free(){
    m_tokenlist.clear();

    return 1;
}

const AIO_XMLToken& AIO_XMLFile::GetFirstToken(){
    m_it = m_tokenlist.begin();

    return (*m_it++);
}


const AIO_XMLToken& AIO_XMLFile::GetNextToken(){
    if(m_it==m_tokenlist.end())
        return *m_it;
    
    return (*m_it++);
}

bool AIO_XMLFile::End(){
    return m_it == m_tokenlist.end();
}