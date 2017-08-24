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


#ifndef AA_Int_hpp
#define AA_Int_hpp

#include "..\kernel\AK_Attribute.hpp"

class AA_Int:public AK_Attribute{
    private:
        int m_value;

    public:
        AA_Int();

        int  GetValue();
        void SetValue(int value);

        // Estaria bé definir operadors.

};

#endif 
