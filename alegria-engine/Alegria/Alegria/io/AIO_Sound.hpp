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

#ifndef AIO_Sound_hpp
#define AIO_Sound_hpp


#include <string>
#include <AL/al.h>

using namespace std;


class AIO_Sound{
    private:
        string	        m_name;
		string			m_path;
        int             m_loaded;

        ALuint          m_buffername;
		ALuint          m_sourcename;
		ALsizei		    m_size; 
		ALsizei         m_frequency;
        ALenum			m_format;
        

    public:
        AIO_Sound();
        ~AIO_Sound();

        string GetName() const;
        void SetName(const string &name);
		const string& GetPath() const;

        ALuint GetBufferName() const;
		ALuint GetSourceName() const;


        int Load(const string &filename, const string &idpath);
        int Free();

};

#endif