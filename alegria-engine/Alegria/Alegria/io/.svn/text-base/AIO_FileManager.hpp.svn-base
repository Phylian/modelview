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

#ifndef AIO_FileManager_hpp
#define AIO_FileManager_hpp

#define AIO_DEFAULT_MAX_CACHED_SCRIPT   100
#define AIO_DEfAULT_MAX_CACHED_XML      40
#define AIO_DEFAULT_SCRIPT_PATH         "./scripts/"
#define AIO_DEFAULT_XML_PATH            "./xml/"

#include <map>


using namespace std;

class AIO_TextBuffer;
class AIO_XMLFile;


class AIO_FileManager{
    private:
        map<string,AIO_TextBuffer*>          m_scriptqueue;
        map<string,AIO_XMLFile*>             m_xmlqueue;
        const unsigned int                   m_maxcachedscript;
        const unsigned int                   m_maxcachedxml;
        
    public:
        AIO_FileManager();
        AIO_FileManager(unsigned int maxscript, unsigned int maxxml);
        ~AIO_FileManager();

        //AIO_TextBuffer*     LoadScript(const string &filename);
        AIO_TextBuffer*     LoadScript(AIO_TextBuffer *script);

        //AIO_XMLFile*        LoadXMLFile(const string &filename);

        

        AIO_TextBuffer*     GetScript(const string &name);
        AIO_XMLFile*        GetXMLFile(const string &name);

        unsigned int        GetNumScripts();
        unsigned int        GetNumXMLFiles();

        void                EraseAll();
        void                EraseAllScripts();
        void                EraseAllXMLFiles();

};

#endif