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
#include <gl/GL.h>
//#include <gl/wglext.h>
//#include <gl/glext.h>

#include "../kernel/AK_Component.hpp"
#include "../components/AC_Render.hpp"
#include "../io/AIO_Image.hpp"
#include "../kernel/AK_Entity.hpp"
#include "../kernel/AK_Window.hpp"
#include "../attributes/AA_String.hpp"
#include "../components/AC_Physics.hpp"

#include "AS_Render.hpp"

#include <iostream>

using namespace std;

#define GL_CLAMP_TO_EDGE 0x812F

bool LessThanComp (AC_Render* comp1, AC_Render* comp2){
    return comp1->GetRenderLayer() < comp2->GetRenderLayer();
}

AS_Render::AS_Render(AK_Window *window):m_components(10),m_window(window),m_winx(0), m_winy(0),m_ratio(0),
                                        m_zoom_distance(16),init(0),m_displaymode(AS_RENDER_DISPLAY_NORMAL){
    SetShouldUpdate(false);

}

AS_Render::~AS_Render(){
    m_components.clear();
}

void AS_Render::Update(int ticks, double ms){

    
    if(!init){
        Init();
        init = true;
    }
    

    //m_components.sort(LessThanComp);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glPushMatrix();
    glTranslatef(-m_campos.x,-m_campos.y,0);

    for(int i=0;i<10;i++){
        list<AC_Render*>::iterator it;

        it=m_components[i].begin();

    
    
        while(it!=m_components[i].end()){
            if((*it)->IsErased()){
                delete (*it);
                it = m_components[i].erase(it);
            }else{
                AC_Render *comp = (*it);

                if(comp->IsVisible() && comp->IsOnSight()){

                    AK_Entity* owner = comp->GetOwner();

                    float rot = owner->GetRotation();
                    Float2d pos = owner->GetPosition();
					AC_Physics *pcomp = static_cast<AC_Physics*>(owner->GetComponent("Physics"));
					if(pcomp){
						pos.x *= ms;
						pos.x+=(1-ms)*pcomp->m_previous_position.x;

						pos.y *= ms;
						pos.y+=(1-ms)*pcomp->m_previous_position.y;

						rot *= ms;
						rot+=(1-ms)*pcomp->m_previous_rotation;

					}
                    float size = owner->GetSize();


                    if(m_displaymode==AS_RENDER_DISPLAY_NORMAL){
                        AIO_Image* img = comp->GetImage();

                        int x_div;
                        int y_div;
                        int frame;
                        int col;
                        int row;


                        glBindTexture(GL_TEXTURE_2D, img->GetTexName());
    
                        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
                        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
                        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
                        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

                        if(!comp->IsText()){
                            x_div = img->GetXDiv();
                            y_div = img->GetYDiv();
                            frame = comp->GetCurrentFrame();
                            col = frame%x_div;
                            row = frame/x_div;
							float width = img->GetWidth()/(float)x_div;
							float height = img->GetHeight()/(float)y_div;
							float xratio = 1.0f;
							float yratio = 1.0f;

							if(width > height){
								yratio = height/width;
							}else{
								xratio = width/height;
							}

                            glPushMatrix();
                                glTranslatef(pos.x, pos.y,float(comp->GetRenderLayer()));
                                glScalef(size, size,1);
                                glRotated(rot,0,0,1);
                                float c1 = col/(float)x_div;
                                float c2 = (col+1)/(float)x_div;
                                float r1 = 1-(row+1)/(float)y_div;
                                float r2 = 1-row/(float)y_div;
                                glColor4f(1,1,1,comp->GetAlpha());
                                glBegin(GL_QUADS);
                                    glTexCoord2f(c1+0.0, r1-0.0); glVertex3f(-0.5f*xratio, -0.5f*yratio,  0.0f);	
		                            glTexCoord2f(c2+0.0, r1-0.0); glVertex3f( 0.5f*xratio, -0.5f*yratio,  0.0f);
		                            glTexCoord2f(c2+0.0, r2-0.0); glVertex3f( 0.5f*xratio,  0.5f*yratio,  0.0f);	
		                            glTexCoord2f(c1+0.0, r2-0.0); glVertex3f(-0.5f*xratio,  0.5f*yratio,  0.0f);
                                glEnd();
                            glPopMatrix();
                        }else{
                            x_div = img->GetXDiv();
                            y_div = img->GetYDiv();
                      
                        
							AK_Attribute *rawattr = owner->GetAttribute("text");
							if(rawattr && rawattr->GetType()==AK_ATTR_STR){
								AA_String *strattr = static_cast<AA_String*>(rawattr);
							
                                string str = strattr->GetValue();

								int displace_factor=0;

								AK_Attribute *align_raw_attr = owner->GetAttribute("align");
								if(align_raw_attr && align_raw_attr->GetType()==AK_ATTR_STR){
									AA_String *align_attr = static_cast<AA_String*>(align_raw_attr);
									if(align_attr->GetValue()=="center"){
										displace_factor=1;
									}else if(align_attr->GetValue()=="right"){
										displace_factor=2;
									}
								}
								
                                glPushMatrix();
                                    glTranslatef(pos.x, pos.y,float(comp->GetRenderLayer()));
                                    glScalef(size, size,1);
                                    glRotated(rot,0,0,1);
                                    for(int i=0;i<str.size();i++){
                                        col = str[i]%x_div;
                                        row = str[i]/x_div;
                                        glPushMatrix();
										glTranslatef((i-((str.size()-1)/2.0)*displace_factor)*0.5, 0, 0);
											glColor4f(1,1,1,comp->GetAlpha());
                                            glBegin(GL_QUADS);
                                                glTexCoord2f(col/(float)x_div, 1-(row+1)/(float)y_div); glVertex3f(-0.5f, -0.5f,  0.0f);	
												glTexCoord2f((col+1)/(float)x_div, 1-(row+1)/(float)y_div); glVertex3f( 0.5f, -0.5f,  0.0f);
												glTexCoord2f((col+1)/(float)x_div, 1-row/(float)y_div); glVertex3f( 0.5f,  0.5f,  0.0f);	
												glTexCoord2f(col/(float)x_div, 1-row/(float)y_div); glVertex3f(-0.5f,  0.5f,  0.0f);
                                            glEnd();
                                        glPopMatrix();
                                    }
                                glPopMatrix();
                            }
                        }
                    }else{
                        glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
                        glPushMatrix();
                            glTranslatef(pos.x, pos.y,float(-1*comp->GetRenderLayer()));
                            glScalef(size, size,1);
                            glRotated(rot,0,0,1);
                            glBegin(GL_QUADS);
                                glVertex3f(-0.5f, -0.5f,  0.0f);	
		                        glVertex3f( 0.5f, -0.5f,  0.0f);
		                        glVertex3f( 0.5f,  0.5f,  0.0f);	
		                        glVertex3f(-0.5f,  0.5f,  0.0f);
                            glEnd();
                        glPopMatrix();
                    }
                
                }
                ++it;
            }
        }
    }
    glPopMatrix();

    SwapBuffers(m_window->GetDC());
    glFlush();
    m_window->Update();
}

int AS_Render::Register(AK_Component *comp){ // Registra el component al subsistema
    if(comp->GetType()!="Render")
        return 0;
    else{
        AC_Render *rendercomp = static_cast<AC_Render*>(comp);
        rendercomp->ConnectSubsystem(this);
        int layer = rendercomp->GetRenderLayer();
        m_components[layer].push_back(rendercomp);
    }
    return 1;
}

void AS_Render::RegisterEntity(AK_Entity *ent){
    AK_Component *aux = ent->GetComponent("Render");
    
    if(aux)
        Register(aux);
}


Float2d AS_Render::GetCameraPosition(){
    return m_campos;
}
void AS_Render::SetCameraPosition(float x, float y){
    m_campos.x = x;
    m_campos.y = y;
}

void AS_Render::SetCameraPosition(Float2d pos){
    m_campos = pos;
}

float AS_Render::GetZoomDistance(){
    return m_zoom_distance;
}

void AS_Render::SetZoomDistance(float zoom){
    
    m_zoom_distance = zoom;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-m_ratio*m_zoom_distance, m_ratio*m_zoom_distance, -1*m_zoom_distance, 1*m_zoom_distance, -100, 100);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

float AS_Render::GetResX(){
    return m_ratio*m_zoom_distance*2;
}

float AS_Render::GetResY(){
    return m_zoom_distance*2;
}

float AS_Render::GetWinX(){
    return m_winx;
}

float AS_Render::GetWinY(){
    return m_winy;
}

AK_Window* AS_Render::GetWindow(){
    return m_window;
}

int AS_Render::SetDisplayMode(int mode){
    if(mode==AS_RENDER_DISPLAY_NORMAL||mode==AS_RENDER_DISPLAY_WIRE){
        m_displaymode = mode;
        return 1; 
    }
    
    return 0;
}

void AS_Render::SetBackgroundColor(float r, float g, float b){
	glClearColor(r,g,b,1);
}

int AS_Render::Init(){
    m_winx = m_window->GetResX();
    m_winy = m_window->GetResY();

    m_ratio = float(m_winx)/m_winy;
    
    glOrtho(-m_ratio*m_zoom_distance, m_ratio*m_zoom_distance, -1*m_zoom_distance, 1*m_zoom_distance, -100, 100);

	glClearColor(0.65,0.65,0.65,1);
    glClearDepth(1.0f);	
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    /*PFNWGLSWAPINTERVALEXTPROC       wglSwapIntervalEXT = NULL; 
    PFNWGLGETSWAPINTERVALEXTPROC    wglGetSwapIntervalEXT = NULL; 
    if (1) 
    { 
        // Extension is supported, init pointers. 
        wglSwapIntervalEXT = (PFNWGLSWAPINTERVALEXTPROC) wglGetProcAddress("wglSwapIntervalEXT"); 
        // this is another function from WGL_EXT_swap_control extension 
        wglGetSwapIntervalEXT = (PFNWGLGETSWAPINTERVALEXTPROC) wglGetProcAddress("wglGetSwapIntervalEXT"); 
    } 

    wglSwapIntervalEXT(1);*/
    return 0;
}

void AS_Render::Clean(){
    list<AC_Render*>::iterator it;

    SetCameraPosition(0,0);

    for(int i=0;i<10;i++){
        for(it=m_components[i].begin();it!=m_components[i].end();++it){
            delete *it;
        }
        m_components[i].clear();
    }

    
}