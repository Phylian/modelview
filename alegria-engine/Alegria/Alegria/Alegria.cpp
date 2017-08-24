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



#include "io\AIO_FileManager.hpp"
#include "io\AIO_ImageManager.hpp"
#include "io\AIO_SoundManager.hpp"
#include "kernel\AK_Archtype.hpp"
#include "kernel\AK_Entity.hpp"
#include "kernel\AK_Attribute.hpp"
#include "kernel\AK_ArchtypeManager.hpp"
#include "kernel\AK_ComponentManager.hpp"
#include "kernel\AK_Condition.hpp"
#include "kernel\AK_EntityManager.hpp"
#include "kernel\AK_ScriptInterpreter.hpp"
#include "kernel\AK_ScriptQueue.hpp"
#include "kernel\AK_SubsystemManager.hpp"
#include "kernel\AK_Trigger.hpp"
#include "kernel\AK_Window.hpp"
#include "subsystems\AS_Keyboard.hpp"
#include "subsystems\AS_Render.hpp"
#include "subsystems\AS_Physics.hpp"
#include "subsystems\AS_Always.hpp"
#include "subsystems\AS_Mouse.hpp"
#include "subsystems\AS_ParticleSystem.hpp"
#include "subsystems\AS_Particle.hpp"
#include "subsystems\AS_Message.hpp"
#include "subsystems\AS_Sound.hpp"
#include "components\AF_KeyboardFactory.hpp"
#include "components\AF_RenderFactory.hpp"
#include "components\AF_PhysicsFactory.hpp"
#include "components\AF_MouseFactory.hpp"
#include "components\AF_MessageFactory.hpp"
#include "components\AF_AlwaysFactory.hpp"
#include "components\AF_ParticleSystemFactory.hpp"
#include "components\AF_ParticleFactory.hpp"
#include "components\AF_SoundFactory.hpp"
#include "io\AIO_XMLFile.hpp"
#include "io\AIO_TextBuffer.hpp"
#include "io\AIO_Image.hpp"
#include "io\AIO_Sound.hpp"
#include "Box2D\Box2D.h"
#include "..\proxies\APY_InterfaceProxy.hpp"
#include <rapidxml\rapidxml.hpp>

#include "Alegria.hpp"

#include <iostream>
#include <fstream>

ofstream file;

using namespace std;
using namespace rapidxml;

#define STT_INITIAL 0
#define STT_IMAGE 1
#define STT_SCRIPT 2
#define STT_ARCHTYPE 3
#define STT_ENTITY 4
#define STT_COMPONENT 5
#define STT_CONDITION 6
#define STT_TRIGGER 7
#define STT_ATTRIBUTE 8
#define STT_TRIGGER_COND 9



int parse_error(const AIO_XMLToken &token){
	cout << "PARSE ERROR." << endl;
	if(token.IsSingle()){
		cout << "Token n." << token.GetNum() << ": < "<< token.Name() << " = " << token.Value() << " >" << endl;
	}else if(token.IsOpener()){
		cout << "Token n." << token.GetNum() << ": < "<< token.Name() << " > " << endl;
	}else if (token.IsCloser()){
		cout << "Token n." << token.GetNum() << ": < /"<< token.Name() << " > " << endl;
	}

	system("pause");
	return 0;
}

Alegria::Alegria(){
	m_change=false;
	m_nextscene="";
	m_ms = 0;
	m_omega = 16;
}
Alegria::~Alegria(){
	Finalize();
}

AK_SubsystemManager* Alegria::GetSubsystemManager(){
	return m_ssmngr;
}

void Alegria::Init(){
	m_archmngr = new AK_ArchtypeManager;
	m_cmpmngr = new AK_ComponentManager;
	m_scriptqueue = new AK_ScriptQueue;
	m_ssmngr = new AK_SubsystemManager;
	m_entmngr = new AK_EntityManager(m_archmngr, m_cmpmngr,m_scriptqueue,m_ssmngr);
	m_filemngr = new AIO_FileManager;
	m_imgmngr = new AIO_ImageManager;
	m_sndmngr = new AIO_SoundManager;
	m_scriptintrprtr = new AK_ScriptInterpreter(m_scriptqueue, m_filemngr);



	m_window = new AK_Window;

	AS_Keyboard *ask = new AS_Keyboard;
	ask->ConnectKeys(m_window->GetKeyBuffer());
	m_render = new AS_Render(m_window);
	AS_Physics *asp = new AS_Physics();
	AS_Mouse *asmo = new AS_Mouse(m_render);
	asmo->ConnectMouseInfo(m_window->GetMouseInfo());
	AS_Message *asmsg = new AS_Message();
	__ConnectSubsystems(m_entmngr,this);
	AS_Sound *assnd = new AS_Sound(m_sndmngr);
	assnd->Init();



	m_ssmngr->RegisterSubsystem(ask);
	m_ssmngr->RegisterSubsystem(asmo);
	m_ssmngr->RegisterSubsystem(asp);
	m_ssmngr->RegisterSubsystem(m_render);
	m_ssmngr->RegisterSubsystem(new AS_Always);
	m_ssmngr->RegisterSubsystem(new AS_ParticleSystem(m_entmngr));
	m_ssmngr->RegisterSubsystem(new AS_Particle);
	m_ssmngr->RegisterSubsystem(asmsg);
	m_ssmngr->RegisterSubsystem(assnd);

	m_cmpmngr->RegisterComponentType(new AF_RenderFactory(m_imgmngr));
	m_cmpmngr->RegisterComponentType(new AF_KeyboardFactory);
	m_cmpmngr->RegisterComponentType(new AF_PhysicsFactory);
	m_cmpmngr->RegisterComponentType(new AF_AlwaysFactory);
	m_cmpmngr->RegisterComponentType(new AF_MouseFactory);
	m_cmpmngr->RegisterComponentType(new AF_ParticleSystemFactory());
	m_cmpmngr->RegisterComponentType(new AF_ParticleFactory);
	m_cmpmngr->RegisterComponentType(new AF_MessageFactory(asmsg));
	m_cmpmngr->RegisterComponentType(new AF_SoundFactory);

}

void Alegria::Finalize(){
	delete  m_scriptintrprtr;
	delete  m_filemngr;
	delete  m_imgmngr;
	delete  m_archmngr;
	delete  m_entmngr;
	delete  m_ssmngr;
	delete  m_cmpmngr;
	delete  m_scriptqueue;
	delete  m_window;


}

string FileToDir(const string &file){
	string dir = file;
	int i=dir.size()-1;
	while(i>=0 && dir[i]!='/'){
		i--;
		dir.pop_back();
	}

	return dir;
}

bool Alegria::LoadXML(const string &filename){

	xml_document<> doc;

	ifstream file (filename);

	vector<char> buffer((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

	buffer.push_back('\0');

	doc.parse<0>(&buffer[0]);

	xml_node<> * root_node;
	root_node = doc.first_node("AlegriaScene");

	if(!root_node)
		return false;

	m_scenepath = FileToDir(filename);
	SetPaths();

	// Image parsing...
	for (xml_node<> * image_node = root_node->first_node("image");image_node; image_node = image_node->next_sibling("image")){
		int x_div = atoi(image_node->first_attribute("x_div")->value());
		int y_div = atoi(image_node->first_attribute("y_div")->value());
		string path = image_node->first_attribute("path")->value();
		string name = image_node->first_attribute("name")->value();
		AIO_Image *img = new AIO_Image();
		string finalp = m_scenepath+path;
		if(!img->Load(finalp,path)){
			delete img;
			return false;
		}
		img->SetName(name);
		img->SetDivisions(x_div,y_div);
		m_imgmngr->LoadImageK(img);
	}

	// Sound parsing...
	for (xml_node<> * sound_node = root_node->first_node("sound");sound_node; sound_node = sound_node->next_sibling("sound")){
		string path = sound_node->first_attribute("path")->value();
		string name = sound_node->first_attribute("name")->value();
		AIO_Sound *sound = new AIO_Sound();
		string finalp = m_scenepath+path;
		if(!sound->Load(finalp,path)){
			delete sound;
			return false;
		}
		sound->SetName(name);
		m_sndmngr->LoadSound(sound);
	}

	// Script parsing
	for (xml_node<> * script_node = root_node->first_node("script");script_node; script_node = script_node->next_sibling("script")){
		string path = script_node->first_attribute("path")->value();
		string name = script_node->first_attribute("name")->value();
		AIO_TextBuffer *script = new AIO_TextBuffer;
		string finalp = m_scenepath+path;
		if(!script->Load(finalp,path)){
			delete script;
			return false;
		}
		script->SetName(name);
		m_filemngr->LoadScript(script);
	}

	// Archtype parsing
	for (xml_node<> * arch_node = root_node->first_node("archtype");arch_node; arch_node = arch_node->next_sibling("archtype")){
		string name = arch_node->first_attribute("name")->value();
		AK_Archtype *arch = new AK_Archtype;
		arch->SetName(name);
		for (xml_node<> * comp_node = arch_node->first_node("component");comp_node; comp_node = comp_node->next_sibling("component")){
			string type = comp_node->first_attribute("comptype")->value();
			AK_Component *comp = m_cmpmngr->CreateComponent(type);
			comp->Parse(comp_node);
			arch->AddComponent(comp);
		}

		for (xml_node<> * trig_node = arch_node->first_node("trigger");trig_node; trig_node = trig_node->next_sibling("trigger")){
			AK_Trigger* trigger = new AK_Trigger;
			trigger->Parse(trig_node);
			arch->AddTrigger(trigger);
		}
		for (xml_node<> * attr_node = arch_node->first_node("attribute");attr_node; attr_node = attr_node->next_sibling("attribute")){
			string type = attr_node->first_attribute("type")->value();
			string name = attr_node->first_attribute("name")->value();

			if(type=="int"){
				arch->AddAttribute(name,AK_ATTR_INT);
			}else if(type=="float"){
				arch->AddAttribute(name,AK_ATTR_FLOAT);
			}else if(type=="bool"){
				arch->AddAttribute(name,AK_ATTR_BOOL);
			}else if(type=="string"){
				arch->AddAttribute(name,AK_ATTR_STR);
			}
		}

		m_archmngr->AddArchtype(arch);
	}

	// Entity parsing
	for (xml_node<> * ent_node = root_node->first_node("entity");ent_node; ent_node = ent_node->next_sibling("entity")){
		string name = ent_node->first_attribute("name")->value();
		float posx = atof(ent_node->first_attribute("pos_x")->value());
		float posy = atof(ent_node->first_attribute("pos_y")->value());
		float rot = atof(ent_node->first_attribute("rot")->value());
		float size = atof(ent_node->first_attribute("size")->value());
		m_entmngr->CreateEntity(name,posx,posy,rot,size);

	}

	file.close();

	return true;
}

void Alegria::SetPaths(){

}


int Alegria::LoadScene(const string &filename){
	return 0;
}

int Alegria::Run(const string &caption, bool fullscreen, int pos_x, int pos_y, int res_x, int res_y, const string &initial_scene){ 

	m_window->Create(caption, fullscreen, pos_x, pos_y, res_x, res_y);

	MSG msg;

	if(!LoadXML(initial_scene))
		return 0;





	LARGE_INTEGER ant,act,freq;
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&ant);
	HWND hWnd = m_window->GetWinHandle();
	while(!m_window->IsDestroyed()){
		while(PeekMessage(&msg,hWnd,0,0,PM_REMOVE)){
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}


		QueryPerformanceCounter(&act);

		double milliseconds = ((act.QuadPart-ant.QuadPart)/double(freq.QuadPart))*1000;



		int ticks = (milliseconds+m_ms)/m_omega;
		m_ms = (milliseconds+m_ms)-ticks*m_omega;

		for(int i=0;i<ticks;i++){
			m_entmngr->CheckTriggers();   
			m_entmngr->UpdateEntities();
			m_scriptintrprtr->CallAll();
			m_ssmngr->Update(m_omega);
			m_entmngr->CleanEntities();

		}		

		m_render->Update(0,m_ms/m_omega);



		if(m_change){
			m_change=false;
			Clean();
			LoadXML(m_nextscene);
		}

		ant=act;

	}

	file.close();
	return 0;
}


void Alegria::ChangeScene(const string &filename){
	m_nextscene = filename;
	m_change = true;
}
void Alegria::Clean(){
	m_ssmngr->Clean();
	m_entmngr->Clean();
	m_archmngr->EraseAllArchtypes();

	m_scriptqueue->Clean();
	m_filemngr->EraseAllScripts();
	m_filemngr->EraseAllXMLFiles();

	m_imgmngr->EraseAll();
}

void Alegria::Quit(){
	m_window->Destroy();
}