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
#include "AIO_FileManager.hpp"
#include "AIO_TextBuffer.hpp"
#include "AIO_XMLFile.hpp"

#include <iostream>
using namespace std;

AIO_FileManager::AIO_FileManager():m_maxcachedscript(AIO_DEFAULT_MAX_CACHED_SCRIPT), 
                                    m_maxcachedxml(AIO_DEfAULT_MAX_CACHED_XML){}

AIO_FileManager::AIO_FileManager(unsigned int maxscript, unsigned int maxxml):m_maxcachedscript(maxscript), 
                                    m_maxcachedxml(maxxml){}

AIO_FileManager::~AIO_FileManager(){
    EraseAll();
}

/*AIO_TextBuffer* AIO_FileManager::LoadScript(const string &filename){
    AIO_TextBuffer *script;

    if(script = GetScript(filename))
        return script;
    
    script = new AIO_TextBuffer();

    if(!script->Load(filename)){
        delete script;
        return 0;
    }

    script->Compile();
    m_scriptqueue[filename] = script;

    return script;
}*/

AIO_TextBuffer* AIO_FileManager::LoadScript(AIO_TextBuffer *script){
    

    if(GetScript(script->GetPath()))
        return 0;

    if(!script->Compile()){
        cout << "Error compiling script: " << script->GetName() << endl;
        system("pause");
    }
    m_scriptqueue[script->GetPath()] = script;

    return script;
}
/*AIO_XMLFile* AIO_FileManager::LoadXMLFile(const string &filename){
    AIO_XMLFile *xml;

    if(xml = GetXMLFile(filename))
        return xml;
    
    
    AIO_TextBuffer buffer;

    if(!buffer.Load(filename)){
        return 0;
    }

    xml = new AIO_XMLFile();
    xml->Load(buffer);
    m_xmlqueue[filename] = xml;

    return xml;
}*/

AIO_TextBuffer* AIO_FileManager::GetScript(const string &name){
    map<string,AIO_TextBuffer*>::iterator it;

    it = m_scriptqueue.find(name);

    if(it==m_scriptqueue.end()){
        return 0;
    }

    return it->second;
}

AIO_XMLFile* AIO_FileManager::GetXMLFile(const string &name){

    map<string,AIO_XMLFile*>::iterator it;

    it = m_xmlqueue.find(name);

    if(it==m_xmlqueue.end()){
        return 0;
    }

    return it->second;
}

unsigned int AIO_FileManager::GetNumScripts(){
    return m_scriptqueue.size();
}

unsigned int AIO_FileManager::GetNumXMLFiles(){
    return m_xmlqueue.size();
}

void AIO_FileManager::EraseAll(){
    EraseAllScripts();
    EraseAllXMLFiles();
}

void AIO_FileManager::EraseAllScripts(){
    while(!m_scriptqueue.empty()){
        map<string,AIO_TextBuffer*>::iterator it = m_scriptqueue.begin();
        delete it->second;
        m_scriptqueue.erase(it);
    }   
}

void AIO_FileManager::EraseAllXMLFiles(){
    while(!m_xmlqueue.empty()){
        map<string,AIO_XMLFile*>::iterator it = m_xmlqueue.begin();
        delete it->second;
        m_xmlqueue.erase(it);
    }   
}