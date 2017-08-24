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
#include <Windows.h>
#include <gl/Gl.h>
#include <gl\glu.h>			// Header File For The GLu32 Library
#include "AIO_Image.hpp"
#include "..\utils\AK_Utils.hpp"
#include "..\soil\SOIL.h"

using namespace std;




AIO_Image::AIO_Image():m_loaded(false),m_name("NONAME"),m_x_div(1),m_y_div(1){}

AIO_Image::~AIO_Image(){
    Free();
}

string AIO_Image::GetName() const{
    return m_name;
}

const string& AIO_Image::GetPath() const{
	return m_path;
}

void AIO_Image::SetName(const string &name){
    m_name = name;
}

int AIO_Image::GetXDiv() const{
    return m_x_div;
}
int AIO_Image::GetYDiv() const{
    return m_y_div;
}

int AIO_Image::Load(const string  &filename, const string &idpath){
	
    m_texname = SOIL_load_OGL_texture
        (
			filename.c_str(),
            0,
            SOIL_CREATE_NEW_ID,
            SOIL_FLAG_MIPMAPS | SOIL_FLAG_INVERT_Y | SOIL_FLAG_NTSC_SAFE_RGB | SOIL_FLAG_COMPRESS_TO_DXT,
			&m_width, &m_height
        );


    glBindTexture(GL_TEXTURE_2D, m_texname);

	m_path = idpath;

	glBindTexture(GL_TEXTURE_2D, m_texname);

	//glGetTexLevelParameteriv(GL_TEXTURE_2D,  0, GL_TEXTURE_WIDTH,  &m_width);
	//glGetTexLevelParameteriv(GL_TEXTURE_2D,  0, GL_TEXTURE_HEIGHT,  &m_height);


	/*TGA_file_header header;
	unsigned long Bread,err,size;
	GLubyte *pixel_array;
	const char *file;
	HANDLE infile;
	SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES),NULL,TRUE};

	m_path = idpath;

    //m_name = filename;
	file=filename.c_str();	

	infile=CreateFileA(file,GENERIC_READ,NULL,&sa,OPEN_EXISTING,0,NULL);
	if ((int)infile < 0){
		cerr << "Could not open the tga file " << filename << endl;
		return 0;
	}
		
	err=ReadFile(infile,&header,sizeof(header),(LPDWORD)&Bread,NULL);
	if (err==0){
		cerr << "Could not read the TGA header" << endl;
		return 0;
	}

	m_width=header.Width;
	m_height=header.Height;
	m_depth=header.Image_pixel_size;

    int bytes = header.Image_pixel_size/8;
	size=bytes*header.Height*header.Width;

	pixel_array=new GLubyte[size];

	err=ReadFile(infile,pixel_array,size,(LPDWORD)&Bread,NULL);
	if (err == 0){
		cerr << "Could not read the pixel field" << endl;
		return 0;
	}
	
    GLubyte* aux = new GLubyte[bytes];
    for(int i=0; i<size;i+=bytes){
        for(int j=0; j<bytes;j++)
            aux[j] = pixel_array[i+j];
        pixel_array[i]=aux[2];
        pixel_array[i+2]=aux[0];
    }
    delete aux;

    glGenTextures(1, &m_texname);
    glBindTexture(GL_TEXTURE_2D, m_texname);


    switch(bytes){
    case 1:
        glTexImage2D(GL_TEXTURE_2D,0, GL_LUMINANCE, m_width, m_height, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, pixel_array); 
        break;
    case 3:
        glTexImage2D(GL_TEXTURE_2D,0, bytes, m_width, m_height, 0, GL_RGB, GL_UNSIGNED_BYTE, pixel_array); 
        break;
    case 4:
        glTexImage2D(GL_TEXTURE_2D,0, bytes, m_width, m_height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixel_array);
        break;
    default:
        delete [] pixel_array;
        cerr << "Bad Tex Format" << endl;
        break;
    }

	delete [] pixel_array;
	CloseHandle(infile);*/

    m_loaded = true;




	
	



    return 1;

}

short AIO_Image::GetWidth() const{
    return m_width;
}

short AIO_Image::GetHeight() const{
    return m_height;
}

void AIO_Image::SetDivisions(int x, int y){
	m_x_div = x;
	m_y_div = y;
}

GLuint AIO_Image::GetTexName() const{
    return m_texname;
}
   

int AIO_Image::Free(){
    if(!m_loaded){
        return 0;
    }
   
    glDeleteTextures(1, &m_texname);
    
    m_loaded = false;
    return 1;

}

int AIO_Image::Parse(const AIO_XMLToken &token){
    /*if(token.Name()=="path"){
        Load(token.Value());
    }else if(token.Name()=="name"){
        SetName(token.Value());
    }else if(token.Name()=="x_div"){
        m_x_div = String2Int(token.Value());
    }else if(token.Name()=="y_div"){
        m_y_div = String2Int(token.Value());
    }else{
        return 0;
    }*/

    return 0;
}
