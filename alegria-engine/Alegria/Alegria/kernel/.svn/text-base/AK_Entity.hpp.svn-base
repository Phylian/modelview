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

#ifndef AK_Entity_hpp
#define AK_Entity_hpp

#include <string>
#include <map>
#include <list>
#include "Python.h"
#include "AK_ComponentContainer.hpp"
#include "AK_TriggerContainer.hpp"
#include "AK_AttributeContainer.hpp"
#include "../utils/AK_Utils.hpp"

using namespace std;

class AK_Component;
class AK_ScriptQueue;


/// An entity is basically a container of components, triggers and attributes, which will define it's behavior as an object.
class AK_Entity:public AK_ComponentContainer, public AK_TriggerContainer, public AK_AttributeContainer{
    private:        
        string              m_name;     // Nom de la entitat.
        
        AK_Entity           *m_parent;  // Pare.
        list<AK_Entity*>    m_children; // Fills.
        bool                m_dead;     // Indica si deu ser eliminada.
		int                m_kill_next_frame;
        
        Float2d             m_position;
        float               m_rotation;
        float               m_size;

        
        /// Adds a child to the entity. Must not be called directly.
        int AddChild(AK_Entity *ent);

        /// Removes a child from the entity. Must not be called directly.
        int RemoveChild(AK_Entity *ent); 

        
    public:
        AK_Entity(const string &name, AK_ScriptQueue *scriptqueue);
        ~AK_Entity();                
        
        /// Returns the name of the entity. 
		//
        /// This name is used as identifier for the entity.
        string GetName();
        
        /// Returns if the entity must be deleted.
		//
		/// This is used internally, as entities must be
		/// deleted all at once at the end of the frame.
		/// Therefore, the entity may be killed, but
		/// it will exist until they are cleaned,
		/// hence the need to know if the entity is dead.
		///
		/// Subsystems should ignore dead entities.
        bool IsDead();

        /// Marks the entity for deletion.
        void Kill();

		/// Returns if the entity will be deleted next frame
		int IsKilledNextFrame();

		/// Marks the entity for deletion in the next frame
		void KillNextFrame();

		void SetKillNextFrame(int k);
        
        /// Returns the parent of the entity (feature not used right now).
        AK_Entity* GetParent();

        /// Sets the parent of the entity (feature not used right now).
        int SetParent(AK_Entity *ent);

        /// Unparents the entity (feature not used right now).
        int RemoveParent();
        
        /// Returns the number of children the entity has (feature not used right now).
        int GetChildrenCount();

        /// Returns a vector containing the pointers to the child entities (feature not used right now).
        AK_Entity** GetChildren();

        /// Adds a component to the entity, both registering
        /// it in the Component Manager and setting its owner.
        int AddComponentK(AK_Component *comp);

        /// Returns the position of the entity.
        /// If the entity has a Physics component it returns
        /// the physical body's position.
        Float2d GetPosition();

        /// Returns the rotation of the entity in degrees.
        /// If the entity has a Physics component it returns
        /// the physical body's rotation.
        float   GetRotation();

        /// Returns the position of the entity.
        /// Ignores the Physics component, used with
        /// initializing purposes.
        Float2d GetPositionRaw();

        /// Returns the rotation of the entity in degrees.
        /// Ignores the Physics component, used with
        /// initializing purposes.
        float   GetRotationRaw();

        /// Returns the size of the entity.
        float   GetSize();

        /// Sets the position of the entity.
        /// If the entity has a Physics component
        /// the user must call its SetPosition method
        /// for the changes to be effective.
        void    SetPosition(float x, float y);

        /// Sets the position of the entity.
        /// If the entity has a Physics component
        /// the user must call its SetPosition method
        /// for the changes to be effective.
        void    SetPosition(Float2d pos);

        /// Sets the rotation of the entity in degrees.
        /// If the entity has a Physics component
        /// the user must call its SetRotation method
        /// for the changes to be effective.
        void    SetRotation(float degrees);

        /// Sets the size of the entity.
        void    SetSize(float size);
        
        /// Returns a new proxy of the entity which
        
		/// will be allocated on Python's heap.
        /// Entities can be accessed by Python
        /// through proxies.
        PyObject* CreateProxy();
       
};

#endif 
