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

#include <iostream>
#include <map>

using namespace std;

class AK_Archtype;
class AIO_FileManager;

/// Stores the archtypes and manages them.
class AK_ArchtypeManager{
    private:
        map<string, AK_Archtype*> m_archtypes;
    public:
        
        ~AK_ArchtypeManager();
        
        /// Adds a new Archtype to the manager so it can be accessed later by the Entity Manager.
        //
        /// The Archtype must be allocated beforehand and the manager will take care of deallocate it.
        /// If an Archtype with the same name exists already this method returns 0. 
        int AddArchtype(AK_Archtype *arch);

        /// Returns a pointer to the archtype with the given name.
        //
        /// If no Archtype is found a null pointer is returned.
        AK_Archtype* GetArchtype(const string &name);

        /// Returns a pointer to the archtype at a given position on the map (used by AlegriaEd).
        AK_Archtype* GetArchtype(int pos);

        /// Deallocates the archtype with the given name.
        //
        /// Usually won't be used, as it's rare to deallocate a specific archtype.
        int EraseArchtype(const string &name);

        /// Deallocates all the archtypes. 
        //  
        /// Used to load a brand new scene.
        int EraseAllArchtypes();

        /// Returns the number of archtypes allocated in the manager.
        //
        /// For informative purposes only.
        int GetCount();
};
