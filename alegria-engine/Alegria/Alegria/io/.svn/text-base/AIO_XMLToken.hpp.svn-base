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

#ifndef AIO_XMLToken_hpp
#define AIO_XMLToken_hpp

#define AIO_XML_IS_CORRUPT -1
#define AIO_XML_IS_OPENER   0
#define AIO_XML_IS_CLOSER   1
#define AIO_XML_IS_SINGLE   2
#define AIO_XML_IS_END      3

#include <string>

using namespace std;

class AIO_XMLToken{
    private:
        string	        m_name;
        string	        m_value;
        int             m_type;
        int             m_num;
    public:
        AIO_XMLToken(const string& str,int num);
        AIO_XMLToken(unsigned int type);
        AIO_XMLToken(const AIO_XMLToken& der);
        void SetName(const string &name);
        void SetValue(const string &value);

        string Name() const;
        string Value() const;

        bool IsCorrupt() const;
        bool IsOpener() const;
        bool IsCloser() const;
        bool IsSingle() const;
        bool IsEnd() const;

        int GetNum() const;

};

#endif //AIO_XMLToken_hpp