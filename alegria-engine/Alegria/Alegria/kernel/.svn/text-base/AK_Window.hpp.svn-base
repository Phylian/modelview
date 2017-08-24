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

#ifndef AK_Window_hpp
#define AK_Window_hpp

#include <Windows.h>
#include <string>

using namespace std;

struct __MouseInfo{
    int mouse_x;
    int mouse_y;

    bool mouse_lb;
    bool mouse_mb;
    bool mouse_rb;
};

/// Represents the Win32 API Window the engine will run on.
class AK_Window{
    private:
        HWND m_hWnd;
        int m_resx;
        int m_resy;
        
    public:
        ~AK_Window();
        
        /// Creates a Win32 window.
        int Create(const string &name, bool fullscreen, int posx, int posy, int resx, int resy);
        
        /// Updates the window
        void Update();

        /// Returns a pointer to the state of the keyboard.
        bool* GetKeyBuffer();

        /// Returns a pointer to the state of the mouse.
        __MouseInfo* GetMouseInfo();

        /// Checks if the window has been destroyed.
        int   IsDestroyed();

        /// Destroys the window
        void  Destroy();

        /// Returns the Win32 handle to the window.
        const HWND GetWinHandle();

        /// Returns the Device Context
        const HDC GetDC();

        /// Returns the Rendering Context
        const HGLRC GetRC();

        /// Gets the width of the client area of the window.
        int GetResX();

        /// Gets the height of the client area of the window.
        int GetResY();
};


#endif  