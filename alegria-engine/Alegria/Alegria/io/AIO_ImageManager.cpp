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
#include "AIO_ImageManager.hpp"
#include "AIO_Image.hpp"

AIO_ImageManager::AIO_ImageManager(){}



AIO_ImageManager::~AIO_ImageManager(){
    EraseAll();
}

/*AIO_Image* AIO_ImageManager::LoadImageK(const string &filename){
    AIO_Image *img;

    if(img = GetImage(filename))
        return img;
    
    img = new AIO_Image();

    if(!img->Load(filename)){
        delete img;
        return 0;
    }

    m_images[filename] = img;

    return img;
}*/

AIO_Image* AIO_ImageManager::LoadImageK(AIO_Image* img){

    if(GetImage(img->GetName()))
        return 0;
    
    

    m_images[img->GetPath()] = img;

    return img;
}

AIO_Image* AIO_ImageManager::GetImage(const string &name){

    map<string,AIO_Image*>::iterator it;

    it = m_images.find(name);

    if(it==m_images.end()){
        return 0;
    }

    return it->second;
}

unsigned int AIO_ImageManager::GetNumImages(){
    return m_images.size();
}


void AIO_ImageManager::EraseAll(){
    while(!m_images.empty()){
        map<string,AIO_Image*>::iterator it = m_images.begin();
        delete it->second;
        m_images.erase(it);
    }   
}
