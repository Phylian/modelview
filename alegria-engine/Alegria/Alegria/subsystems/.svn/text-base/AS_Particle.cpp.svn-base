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
#include "../components/AC_Particle.hpp"
#include "../components/AC_Render.hpp"
#include "../io/AIO_Image.hpp"
#include "AS_Particle.hpp"


AS_Particle::AS_Particle(){
    SetName("Particle");
}

void AS_Particle::Update(int ticks, double ms){
    list<AC_Particle*>::iterator it;

    it=m_components.begin();

    while(it!=m_components.end()){
        if((*it)->IsErased()){
            delete (*it);
            it = m_components.erase(it);
        }else{
            AK_Entity *owner = (*it)->GetOwner();
            if(ticks){
                AC_Particle *comp = static_cast<AC_Particle*>(*it);
                
                int frame = comp->GetCurrentFrame();
                int lifetime = comp->GetLifetime();
                if(frame<lifetime){
                    float velx = comp->m_velx;
                    float vely = comp->m_vely;
                    
                    Float2d pos = owner->GetPositionRaw();
                    pos.x+=velx;
                    pos.y+=vely;
                    owner->SetPosition(pos);

                    float size = owner->GetSize();
                    size+=comp->GetScaling();
                    owner->SetSize(size);

                    AC_Render *render = static_cast<AC_Render*>(owner->GetComponent("Render"));
                    
                    if(comp->IsAnimated()){
                        AIO_Image *img = render->GetImage();
                        float totalframes = img->GetXDiv()*img->GetYDiv();
                        render->SetCurrentFrame((totalframes/lifetime)*frame);
                    }
                    
                    if(comp->GetFading()){
                        float alpha = render->GetAlpha();
                        alpha-=(1.0/lifetime);
                        render->SetAlpha(alpha);
                    }
                    
                    
                    comp->Step();
                }else{
                    owner->Kill();
                }
                
            }

            (*it)->UpdateConditions(ticks);
            ++it;
        }
    }
}

int AS_Particle::Register(AK_Component *comp){
    if(comp->GetType()!="Particle")
        return 0;
    
    AC_Particle *pcomp = static_cast<AC_Particle*>(comp);
    

    m_components.push_back(pcomp);

    return 1;
}

void AS_Particle::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Particle");
    
    if(aux)
        Register(aux);
}


void AS_Particle::Clean(){
    list<AC_Particle*>::iterator it;

    for(it=m_components.begin();it!=m_components.end();++it){
        delete *it;
    }

    m_components.clear();
}
