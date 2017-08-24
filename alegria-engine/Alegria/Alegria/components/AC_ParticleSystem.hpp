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
 * ParticleSystem Component
*/

#ifndef AC_ParticleSystem_hpp
#define AC_ParticleSystem_hpp


#include "../kernel/AK_Component.hpp"

class AK_Condition;

class AC_ParticleSystem:public AK_Component{
    private:    
        string m_particle;
        
        int m_totalparticles;
        int m_lifetime;
        
        int m_particle_lifetime;
        float m_particle_velocity;
        float m_particle_size;
        int m_particle_randomness;
        bool m_particle_fading;
        bool m_particle_animation;
        float m_particle_scaling;
        bool m_rotate_particles;
        float m_velocity_interpolation;

        bool m_running;
        bool m_paused;
        int m_current_frame; 

        bool m_autokill;
        bool m_loop;

        int m_interval;

    public:
        AC_ParticleSystem();
        AC_ParticleSystem(const AC_ParticleSystem &der);
        
        string GetParticle();
        void SetParticle(const string &particle);

        int GetTotalParticles();
        void SetTotalParticles(int total);

        int GetLifetime();
        void SetLifetime(int time);

        int GetParticleLifetime();
        void SetParticleLifetime(int time);

        float GetParticleVelocity();
        void SetParticleVelocity(float vel);

        float GetParticleSize();
        void SetParticleSize(float size);

        bool IsParticleAnimated();
        bool GetParticleFading();
        float GetParticleScaling();
        float GetVelocityInterpolation();

        int GetParticleRandomness();
        void SetParticleRandomness(int randomness);

        bool GetParticleRotation();
        void SetParticleRotation(bool flag);

        int GetCurrentFrame();
        int GetInterval();

        bool GetAutokill();

        bool GetLoop();

        bool IsRunning();
        void Start();
        void Step();
        void Pause();
        void Stop();

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        PyObject* CreateProxy();
        
};


#endif  
