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


#ifndef AK_Subsystem_hpp
#define AK_Subsystem_hpp

#include <string>

using namespace std;

class AK_Component;
class AK_SubsystemManager;
class AK_Entity;

/// Base class for the subsystems.
class AK_Subsystem{
    private:
        string      m_name;
		bool		m_should_update;
        
    protected:
        AK_SubsystemManager *m_ssmanager;

        /// Assigns a name to the subsystem. 
        /// Must be called from the constructor of the
        /// derived class.
        void SetName(const string &name);
        
    public:
        AK_Subsystem();
        virtual ~AK_Subsystem();

        /// Returns the name of the subsystem.
        string GetName();

		/// Returns if the subsystem should be updated by the subsystem manager
		bool ShouldUpdate(){ return m_should_update; }

		/// Sets if the subsystem should be updated by the subsystem manager
		void SetShouldUpdate( bool should_update ) { m_should_update = should_update; }
        
        /// Connects the subsystem to the Subsystem Manager.
        /// This method must be called only by the Subsystem Manager.
        int _init_(AK_SubsystemManager *ssmngr);
        

        /// Updates the subsystem.
        /// Note that in this method the cleaning
        /// of the components must be taken care of.
        virtual void Update(int ticks, double ms) = 0;

        /// Register a component in the subsystem.
        /// Usually, the subsystem will check if 
        /// the component is of the right type.
        virtual int Register(AK_Component *comp) = 0;

        /// Checks if the given entity has a component compatible
        /// with the subsystem and, if it's the case, registers it.
        virtual void RegisterEntity(AK_Entity *ent) = 0;

        /// Cleans the subsystem deallocating all the associated components.
        virtual void Clean() = 0;

        
};

#endif
