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
 * Classe base per als subsystemes. És abstracta.
*/

#ifndef AS_Render_hpp
#define AS_Render_hpp

#define AS_RENDER_DISPLAY_NORMAL    0
#define AS_RENDER_DISPLAY_WIRE      1

#include <string>
#include <list>
#include <vector>

#include "../kernel/AK_Subsystem.hpp"
#include "../utils/AK_Utils.hpp"
#include <Windows.h>

using namespace std;

class AK_Component;
class AC_Render;
class AK_Window;

class AS_Render:public AK_Subsystem{
    private:
        vector<list<AC_Render*>> m_components;
        unsigned int        m_winx;
        unsigned int        m_winy;
        Float2d             m_campos;
        float               m_zoom_distance;
        float               m_ratio;

        int                 m_displaymode;

        HGLRC               m_hrc;
        HDC                 m_hdc;
        HWND                m_hwnd;

        AK_Window           *m_window;

        bool init;

    public:
        AS_Render(AK_Window* window);
        ~AS_Render();

        void Update(int ticks, double ms);
        int Register(AK_Component *comp); // Registra el component al subsistema
        void RegisterEntity(AK_Entity *ent);

        Float2d GetCameraPosition();
        void SetCameraPosition(float x, float y);
        void SetCameraPosition(Float2d pos);

        float GetZoomDistance();
        void SetZoomDistance(float zoom);
		float GetRatio(){return m_ratio;}
        float GetResX();
        float GetResY();
        float GetWinX();
        float GetWinY();

        AK_Window* GetWindow();

        int SetDisplayMode(int mode);

		void SetBackgroundColor(float r, float g, float b);

        int  Init();
        
        void Clean();
};

#endif 
