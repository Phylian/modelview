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

/**
 * Particle Component
*/

#ifndef AC_Particle_hpp
#define AC_Particle_hpp


#include "../kernel/AK_Component.hpp"

class AK_Condition;

class AC_Particle:public AK_Component{
    private:         
        float m_velocity;
        int m_lifetime;
        int m_current_frame;

        bool m_animate;
        bool m_fading;
        float m_scaling;
        
    public:
        float m_rad_rot;
        float m_velx,m_vely;

        AC_Particle();
        AC_Particle(const AC_Particle &der);

        int GetLifetime();
        void SetLifetime(int time);

        float GetVelocity();
        void SetVelocity(float vel);

        int GetCurrentFrame();
        void Step();

        bool IsAnimated();
        void SetAnimated(bool animated);

        void SetFading(bool fading);
        bool GetFading();

        void SetScaling(float scaling);
        float GetScaling();

        void UpdateVelComps();

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        PyObject* CreateProxy();
        
};


#endif  
