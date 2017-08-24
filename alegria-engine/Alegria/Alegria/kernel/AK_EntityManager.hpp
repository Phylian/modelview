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



#ifndef AK_EntityManager_hpp
#define AK_EntityManager_hpp

#include <list>
#include <string>

#include "../utils/AK_Utils.hpp"

using namespace std;

class AK_Entity;
class AK_ComponentManager;
class AK_ArchtypeManager;
class AK_ScriptQueue;
class AK_SubsystemManager;

/// Creates, destroys and manages all the entities in the scene.
class AK_EntityManager{
    private:
        list<AK_Entity*>        m_entities;            // Entitats en temps real
        AK_ArchtypeManager*     m_archtypemanager;     // Manager d'arquetips
        AK_ComponentManager*    m_componentmanager;    // Manager de components
        AK_ScriptQueue*         m_scriptqueue;         // Cola d'scripts
        AK_SubsystemManager*    m_ssmanager;           // Subsystem Manager

    public:
        AK_EntityManager(AK_ArchtypeManager* archmngr,AK_ComponentManager* cmpmngr, AK_ScriptQueue* scriptqueue, AK_SubsystemManager* ssmanager);
        ~AK_EntityManager();
        

        /// Creates a new entity and returns a pointer to it.
        AK_Entity* CreateEntity(const string &name, float posx, float posy, float rot, float size);

        /// Returns an entity given its name.
        /// If there's only an entity of a specific archtype
        /// chances are that entity's name is "archtypename.0"
        AK_Entity* GetEntity(const string &name);

		/// Returns all the entities that have a certain attribute.
		list<AK_Entity*> GetEntitiesByAttribute(const string &attr);

        /// Erases an entity given its pointer.
        int EraseEntity(AK_Entity *ent);

        /// Erases all the entities. Useful when loading a new scene
        /// and current is over.
        int EraseAllEntities();

        /// Iterates through all the triggers and updates them
        void CheckTriggers();

		/// Updates some aspects of the entities
		void UpdateEntities();

        /// Checks for dead entities and erases them.
        void CleanEntities();

        /// Erases all the entities (used by LoadScene)
        void Clean();
};

#endif 
