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

#ifndef AIO_Image_hpp
#define AIO_Image_hpp


#include <string>
#include <Windows.h>
#include <gl/GL.h>

#include "AIO_XMLToken.hpp"

using namespace std;


class AIO_Image{
    private:
        string	        m_name;
		string			m_path;
        int             m_loaded;

        int           m_width;
        int           m_height;
        GLubyte         m_depth;

        GLuint          m_texname;
        
        int             m_x_div;
        int             m_y_div;

    public:
        AIO_Image();
        ~AIO_Image();

        string GetName() const;
        void SetName(const string &name);
		const string& GetPath() const;
        short GetWidth() const;
        short GetHeight() const;
        GLuint GetTexName() const;

        int GetXDiv() const;
        int GetYDiv() const;
		void SetDivisions(int x, int y);

        int Load(const string &filename, const string &idpath);
        int Free();

        int Parse(const AIO_XMLToken &token);
        
};

#pragma pack(push, 1) // All data members will placed right next to each other.
typedef struct {
	unsigned char ID_length;
	unsigned char Color_map_type;
	unsigned char Image_type;
	// Color_map_specification
	unsigned short Color_map_origin;
	unsigned short Color_map_length;
	unsigned char Color_map_entry_size;
	// Image_specification
	unsigned short XOrigin;
	unsigned short YOrigin;
	unsigned short Width;
	unsigned short Height;
	unsigned char Image_pixel_size; 
	unsigned char Image_descriptor_byte;
	
} TGA_file_header;	
#pragma pack(pop)

#endif