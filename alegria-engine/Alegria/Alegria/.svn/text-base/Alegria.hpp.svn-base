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

#ifndef Alegria_hpp
#define Alegria_hpp

#include <string>

using namespace std;

class AIO_FileManager;
class AIO_ImageManager;
class AIO_SoundManager;
class AK_ArchtypeManager;
class AK_EntityManager;
class AK_SubsystemManager;
class AK_ComponentManager;
class AK_ScriptQueue;
class AK_ScriptInterpreter;
class AK_Window;
class AS_Render;


class Alegria{
    private:
        AIO_FileManager         *m_filemngr;
        AIO_ImageManager        *m_imgmngr;
		AIO_SoundManager		*m_sndmngr;
        AK_ArchtypeManager      *m_archmngr;
        AK_EntityManager        *m_entmngr;
        AK_SubsystemManager     *m_ssmngr;
        AK_ComponentManager     *m_cmpmngr;
        AK_ScriptQueue          *m_scriptqueue;
        AK_ScriptInterpreter    *m_scriptintrprtr;
        AK_Window               *m_window;
		AS_Render				*m_render;

		int						m_omega;
		double					m_ms;
		
        string m_nextscene;
        bool   m_change;

		string m_scenepath;

    public:
        Alegria();
        ~Alegria();

        AK_SubsystemManager *GetSubsystemManager();

        void Init();
        void Finalize();
        
		bool LoadXML(const string &filename);
		void SetPaths();
        int LoadScene(const string &filename);
        
        int Run(const string &caption, bool fullscreen, int pos_x, int pos_y, int res_x, int res_y, const string &initial_scene);
        
        void ChangeScene(const string &filename);
        void Clean();
        void Quit();
};

#endif