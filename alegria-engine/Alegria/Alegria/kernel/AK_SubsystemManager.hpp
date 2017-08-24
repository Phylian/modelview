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


#ifndef AK_SubsystemManager_hpp
#define AK_SubsystemManager_hpp

#include <list>
#include <string>

using namespace std;

class AK_Subsystem;
class AK_Entity;

/// Keeps track of and updates the subsystems of the engine.
class AK_SubsystemManager{
    private:
        list<AK_Subsystem*> m_subsystems;   
        
        double m_ms;
        int m_omega;
    public:
        AK_SubsystemManager();
        ~AK_SubsystemManager();

        /// Returns the period of the ticks.
        int GetOmega();

        /// Returns a pointer to an specific subsystem.
        AK_Subsystem* GetSubsystem(const string &name);

        /// Registers a new subsystem.
		//
        /// Must be allocated beforehand and the manager will take care of deallocating it.
        int RegisterSubsystem(AK_Subsystem* subsystem);

        /// Removes an specific subsystem from the subsystem.
        int RemoveSubsystem(const string &name);

        /// Updates all the subsystems.
		//
        /// Takes the miliseconds elapsed since the last call 
        /// as argument.
        void Update(double ms);

        /// Registers an entity on the subsystem system (sorry about that, seriously)
        int AddEntity(AK_Entity* ent);

        /// Cleans all the subsystems (used to load a new scene)
        void Clean();
};

#endif 
