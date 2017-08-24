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

#ifndef AIO_TextBuffer_hpp
#define AIO_TextBuffer_hpp

#include <string>
#include <Python.h>
#include "AIO_XMLToken.hpp"

using namespace std;

class AIO_TextBuffer{
    private:
        char* m_buffer;
        string m_name;
		string m_path;
        long m_size;
        PyObject *m_bytecode;
        
    public:
        AIO_TextBuffer();
        ~AIO_TextBuffer();

        int Load(const string& filename, const string &idpath);
        int Compile();
        int GetSize() const;
        const char* GetBuffer() const;
        PyObject* GetByteCode();
        void Free();

        string GetName();
		const string &GetPath(){return m_path;}
        void   SetName(const string &name);

        int Parse(const AIO_XMLToken &token);


};

#endif //AIO_TextBuffer_hpp