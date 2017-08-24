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


#ifndef AK_Attribute_hpp
#define AK_Attribute_hpp

#include <string>

using namespace std;

/// Types of attributes deriving from the base attribute class.
enum{
    AK_ATTR_INT,
    AK_ATTR_FLOAT,
    AK_ATTR_STR,
    AK_ATTR_BOOL
};


/// An attribute that will be attached to an archtype/entity.
//
/// This is the base class and must not be instantiated directly.
class AK_Attribute{
    private:
        int          m_type;
    public:
        /// Default constructor
        AK_Attribute(int type);

        /// Returns the type of the attribute.
        int GetType();
};

#endif 
