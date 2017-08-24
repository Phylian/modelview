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



#ifndef AK_Message_hpp
#define AK_Message_hpp

#include <string>
#include <list>
#include "../io/AIO_XMLToken.hpp"

using namespace std;

class AK_Entity;

class AK_Message{
    private:
		string m_sendto;
		string m_msg;
		AK_Entity *m_owner;

    public:

		AK_Message(const string &sendto, const string &msg, AK_Entity *owner);
		AK_Message(const AK_Message &msg);

		const string& GetReceiver();
		const string& GetMsg();
};
#endif 
