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



// Carrega un script a un buffer i guarda un punter dins del map.
// Paràmetres: 
//      - name : el nom de l'arxiu corresponent a l'script.
// Retorna:
//      - AIO_FAILED (0)      : Si no s'ha pogut obrir l'arxiu (probablement no existisca)
//      - AIO_OPENED (1)      : Si s'ha obert correctament.
//      - AIO_ALREADYOPEN (2) : Si ja estava obert.

#include <fstream>
#include "AK_ScriptManager.hpp"

using namespace std;

int AK_ScriptManager::loadScript(const string& name){
    
    if(m_scriptmap.find(name)!=m_scriptmap.end())
        return AIO_ALREADYOPEN; // Ja existeix l'script.
        
    ifstream file(name.c_str());
    
    if(!file.is_open())         
        return AIO_FAILED;      // No s'ha pogut obrir l'arxiu
        
    file.seekg (0, ios::end);   //
    int length = file.tellg();  // Calculem la longitud de l'arxiu.
    file.seekg (0, ios::beg);   //
    
    char *buffer = new char[length];
    
    file.read(buffer, length);  // Introduim el contingut de l'arxiu en el buffer.
    file.close();
    
    m_scriptmap[name] = buffer; // Insertem el buffer en el map.
    
    return AIO_OPENED;
}



// Llibera l'script de memòria i esborra la corresponent entrada del map.
// Paràmetres: 
//      - name : el nom de l'arxiu corresponent a l'script.
// Retorna:
//      - 1 : S'ha esborrat l'script.
//      - 0 : L'script no existia.

int AK_ScriptManager::eraseScript(const string& name){
    
    map<string,char*>::iterator it = m_scriptmap.find(name);
    
    if(it==m_scriptmap.end())
        return 0;               // No existeix l'script
    
    delete it->second;          // Lliberem la memòria corresponent.
    
    m_scriptmap.erase(it);      // Eliminem l'entrada.
    
    return 1;
    
}



// Retorna el nombre d'scripts carregats en memòria actualment.
// Paràmetres: 
//      - (cap)
// Retorna:
//      - El nombre d'scripts en el map.

int AK_ScriptManager::getScriptCount(){
    return m_scriptmap.size();
}



// Esborra tots els scripts.
// Paràmetres:
//      - (cap)
// Retorna:
//      - El nombre d'scripts esborrats.

int AK_ScriptManager::eraseAll(){
    map<string,char*>::iterator it;
    
    for(it = m_scriptmap.begin(); 
        it != m_scriptmap.end(); ++it){
        delete (it)->second;
        m_scriptmap.erase(it);
    }

	return 1;
}
