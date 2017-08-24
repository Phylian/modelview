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

#include <string>
#include <stdlib.h>
#include "AC_Render.hpp"
#include "../kernel/AK_Entity.hpp"
#include "../kernel/AK_Condition.hpp"
#include "../io/AIO_Image.hpp"
#include "../io/AIO_ImageManager.hpp"
#include "../proxies/APY_RenderCompProxy.hpp"
#include "../subsystems/AS_Render.hpp"
#include <iostream>

using namespace std;

AC_Render::AC_Render(AIO_ImageManager* imgmngr):m_imgmngr(imgmngr),m_image(0),m_visible(1),m_layer(3),m_frame(0),m_alpha(1),m_text(0){
    SetType("Render");
}

AC_Render::AC_Render(const AC_Render &der){
    SetType("Render");

    m_imgmngr = der.m_imgmngr;
    m_visible = der.m_visible;
    m_alpha = der.m_alpha;
    m_image = der.m_image;
    m_layer = der.m_layer;
    m_frame = der.m_frame;
    m_text = der.m_text;
}


int AC_Render::GetCurrentFrame(){
    return m_frame;
}
void AC_Render::SetCurrentFrame(int frame){
    m_frame = frame;
}

void AC_Render::SetVisibility(bool vis){
    m_visible=vis;
}

bool AC_Render::IsVisible(){
    return m_visible;
}

bool AC_Render::IsOnSight(){
	Float2d campos = GetCameraPosition();
	float zoom = GetZoomDistance();
	float ratio = m_asr->GetRatio();
	float size = m_owner->GetSize();
	Float2d pos = m_owner->GetPosition();
	if(pos.x-size < ratio*zoom+campos.x && pos.x+size > -ratio*zoom+campos.x)
		if(pos.y-size < zoom+campos.y && pos.y+size > -zoom+campos.y)
			return true;
	return false;
}

void AC_Render::SetRenderLayer(unsigned int layer){
    m_layer = layer;
}

unsigned int AC_Render::GetRenderLayer(){
    return m_layer;
}

AK_Condition* AC_Render::CreateCondition(){
    return 0;
}

AK_Condition* AC_Render::CreateCondition(const AK_Condition &cond){
    return 0;
}

void AC_Render::UpdateConditions(int ticks){
    // Pass
}

int AC_Render::Parse(xml_node<> *comp_node){

	m_visible = atoi(comp_node->first_attribute("visible")->value());
	m_layer = atoi(comp_node->first_attribute("layer")->value());
	m_alpha = atof(comp_node->first_attribute("alpha")->value());
	SetImage(comp_node->first_attribute("image")->value());
	m_text = atoi(comp_node->first_attribute("text")->value());


	return 1;
}

int AC_Render::Parse(const AIO_XMLToken &token){
    if(token.Name()=="visible"){


        int vis = String2Int(token.Value());
        
        if(vis>=1 || vis<0)
            m_visible=1;
        else
            m_visible = 0;

    }else if(token.Name()=="layer"){

        unsigned int l = String2Int(token.Value());
        SetRenderLayer(l);
        
        if(m_layer>10 || m_layer<0)
            m_layer=10;

    }else if(token.Name()=="alpha"){
        m_alpha = String2Float(token.Value());
    }else if(token.Name()=="image"){
        if(SetImage(token.Value()))
            return 1;
        else
            return 0;
    }else if(token.Name()=="text"){

        if(token.Value() == "true" || token.Value()=="1")
            m_text=true;
        
    }else{
        return 0;
    }

    return 1;
}


int AC_Render::SetImage(const string &name){
    AIO_Image *aux;
    if(aux = m_imgmngr->GetImage(name)){
        m_image = aux;
        return 1;
    }else{
        return 0;
    }
}

AIO_Image* AC_Render::GetImage(){
    return m_image;
}

bool AC_Render::IsText(){
    return m_text;
}

void AC_Render::SetBackgroundColor(float r, float g, float b){
	m_asr->SetBackgroundColor(r,g,b);
}

Float2d AC_Render::GetCameraPosition(){
    return m_asr->GetCameraPosition();
}

void AC_Render::SetCameraPosition(float x, float y){
    m_asr->SetCameraPosition(x,y);
}

float AC_Render::GetZoomDistance(){
    return m_asr->GetZoomDistance();
}
void AC_Render::SetZoomDistance(float zoom){
    m_asr->SetZoomDistance(zoom);
}

float AC_Render::GetAlpha(){
    return m_alpha;
}
void AC_Render::SetAlpha(float alpha){
    m_alpha = alpha;
}

void AC_Render::ConnectSubsystem(AS_Render *asr){
    m_asr = asr;
}

PyObject* AC_Render::CreateProxy(){
    APY_RenderCompProxy* aux = PyObject_New(APY_RenderCompProxy,&_type);
    aux->ref=this;

    return (PyObject*) aux;
}
