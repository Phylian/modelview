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

#include "AC_AlwaysCondition.hpp"


AC_AlwaysCondition::AC_AlwaysCondition(){}

AC_AlwaysCondition::AC_AlwaysCondition(const AC_AlwaysCondition &der){
    m_name = der.m_name;
    m_omega = der.m_omega;
    m_inv = der.m_inv;
}

int AC_AlwaysCondition::Parse(xml_node<> *cond_node){
	return AK_Condition::Parse(cond_node);
}

int AC_AlwaysCondition::Parse(const AIO_XMLToken &token){
    return AK_Condition::Parse(token);
}
