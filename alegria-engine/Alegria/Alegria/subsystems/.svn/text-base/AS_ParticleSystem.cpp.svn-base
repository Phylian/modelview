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

#include <Windows.h>
#include "../kernel/AK_Entity.hpp"
#include "../kernel/AK_EntityManager.hpp"
#include "../components/AC_ParticleSystem.hpp"
#include "AS_ParticleSystem.hpp"
#include "../components/AC_Particle.hpp"

AS_ParticleSystem::AS_ParticleSystem(AK_EntityManager* entmngr):m_entmngr(entmngr){
    SetName("ParticleSystem");
}

void AS_ParticleSystem::Update(int ticks, double ms){
    list<AK_Component*>::iterator it;

    it=m_components.begin();

    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            if(ticks){
                AC_ParticleSystem *comp = static_cast<AC_ParticleSystem*>(*it);
                if(comp->IsRunning()){
                    int frame = comp->GetCurrentFrame();
                    int lifetime = comp->GetLifetime();
                    int interval = comp->GetInterval();
                    
                    if(frame<lifetime){
                        if(interval==1){
                            int total_particles = comp->GetTotalParticles();
                            int particles_per_frame = (total_particles)/lifetime;
                            string particle = comp->GetParticle();
                            Float2d pos = comp->GetOwner()->GetPosition();
                            int randomness = comp->GetParticleRandomness();
                            float vel = comp->GetParticleVelocity();
                            float size = comp->GetParticleSize();
                            int lifetime = comp->GetParticleLifetime();
                            bool animated = comp->IsParticleAnimated();
                            bool fading = comp->GetParticleFading();
                            float scaling = comp->GetParticleScaling();
                            float interpolation = comp->GetVelocityInterpolation();
                            bool rotate = comp->GetParticleRotation();
                            int rands[10];
                            for(int i=0;i<10;i++){
                                rands[i]=rand()%(randomness+1)-randomness/2;
                            }

                            
                            for(int i=0;i<particles_per_frame;i++){
                                int angle;

                                if(rotate)
                                    angle = rand()%360-180;
                                else
                                    angle = comp->GetOwner()->GetRotation();
                                AK_Entity* ent = m_entmngr->CreateEntity(comp->GetParticle(),pos.x,pos.y,angle,size);
                                AC_Particle *pcomp = static_cast<AC_Particle*>(ent->GetComponent("Particle"));
                                //int factor = rand()%(randomness+1)-randomness/2;
                                pcomp->SetLifetime(lifetime+rands[i%10]);
                                pcomp->SetVelocity(vel);
                                pcomp->SetAnimated(animated);
                                pcomp->SetFading(fading);
                                pcomp->SetScaling(scaling);
                                pcomp->UpdateVelComps();

                            }
                        }else if(frame%interval==0){
                            int total_particles = comp->GetTotalParticles();
                            int particles_per_frame = (total_particles*interval)/lifetime;
                            string particle = comp->GetParticle();
                            Float2d pos = comp->GetOwner()->GetPosition();
                            int randomness = comp->GetParticleRandomness();
                            float vel = comp->GetParticleVelocity();
                            float size = comp->GetParticleSize();
                            int lifetime = comp->GetParticleLifetime();
                            bool animated = comp->IsParticleAnimated();
                            bool fading = comp->GetParticleFading();
                            float scaling = comp->GetParticleScaling();
                            float interpolation = comp->GetVelocityInterpolation();
                            bool rotate = comp->GetParticleRotation();
                            int rands[10];
                            for(int i=0;i<10;i++){
                                rands[i]=rand()%(randomness+1)-randomness/2;
                            }

                            
                            for(int i=0;i<particles_per_frame;i++){
                                int angle;

                                if(rotate)
                                    angle = rand()%360-180;
                                else
                                    angle = comp->GetOwner()->GetRotation();
                                AK_Entity* ent = m_entmngr->CreateEntity(comp->GetParticle(),pos.x,pos.y,angle,size);
                                AC_Particle *pcomp = static_cast<AC_Particle*>(ent->GetComponent("Particle"));
                                //int factor = rand()%(randomness+1)-randomness/2;
                                pcomp->SetLifetime(lifetime+rands[i%10]);
                                pcomp->SetVelocity(vel);
                                pcomp->SetAnimated(animated);
                                pcomp->SetFading(fading);
                                pcomp->SetScaling(scaling);
                                pcomp->UpdateVelComps();

                            }
                        }
                        comp->Step();
                    }else{
                        if(comp->GetAutokill())
                            comp->GetOwner()->Kill();
                        else if(comp->GetLoop()){
                            comp->Stop();
                            comp->Start();
                        }
                        else
                            comp->Stop();
                    }
                }
            }

            (*it)->UpdateConditions(ticks);
            ++it;
        }
    }
}

int AS_ParticleSystem::Register(AK_Component *comp){
    if(comp->GetType()!="ParticleSystem")
        return 0;
    
    AC_ParticleSystem *pcomp = static_cast<AC_ParticleSystem*>(comp);
    
    m_components.push_back(comp);

    return 1;
}

void AS_ParticleSystem::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("ParticleSystem");
    
    if(aux)
        Register(aux);
}

void AS_ParticleSystem::Clean(){
    list<AK_Component*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();
}
