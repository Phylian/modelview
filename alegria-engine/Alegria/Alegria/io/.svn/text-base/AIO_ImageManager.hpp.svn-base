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

#ifndef AIO_ImageManager_hpp
#define AIO_ImageManager_hpp

#define AIO_DEFAULT_IMG_PATH         "./images/"

#include <map>

using namespace std;

class AIO_Image;

class AIO_ImageManager{
    private:
        map<string,AIO_Image*>          m_images;
        
    public:
        AIO_ImageManager();
        ~AIO_ImageManager();

        //AIO_Image* LoadImageK(const string &filename);
        AIO_Image* LoadImageK(AIO_Image* img);

        AIO_Image* GetImage(const string &name);

        unsigned int        GetNumImages();

        void                EraseAll();

};

#endif