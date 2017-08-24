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

#include "AK_Window.hpp"



HDC __hDC;
HGLRC __hRC;

bool __keys[256];

__MouseInfo __mouseinfo;

int __destroyed=0;


// Funció auxiliar.

void SetDCPixelFormat(HDC hDC){
    int nPixelFormat;

    static PIXELFORMATDESCRIPTOR pfd = {
        sizeof(PIXELFORMATDESCRIPTOR),
        1,
        PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        PFD_TYPE_RGBA,
        24,
        0,0,0,0,0,0,
        0,0,
        0,0,0,0,0,
        32,
        0,
        0,
        PFD_MAIN_PLANE,
        0,
        0,0,0};

    nPixelFormat = ChoosePixelFormat(hDC,&pfd);

    SetPixelFormat(hDC, nPixelFormat, &pfd);
}

long APIENTRY WndProc(
    HWND hWnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam){
    WORD ht;
    PAINTSTRUCT ps; 
    switch(uMsg){
        case WM_CREATE:
            __hDC = GetDC(hWnd);

            SetDCPixelFormat(__hDC);

            __hRC = wglCreateContext(__hDC);
            wglMakeCurrent(__hDC, __hRC);

            for(int i=0;i<256;i++)
                __keys[i] = false;
            
            break;
        
        case WM_DESTROY:
            __destroyed=1;
            wglMakeCurrent(__hDC, NULL);
            wglDeleteContext(__hRC);
            PostQuitMessage(0);
            break;

        case WM_KEYDOWN:
            __keys[wParam] = true;
            break;
            
        case WM_KEYUP:
            __keys[wParam] = false;
            break;

        case WM_MOUSEMOVE:
            __mouseinfo.mouse_x = LOWORD(lParam);
            __mouseinfo.mouse_y = HIWORD(lParam);
            break;

        case WM_LBUTTONDOWN:
            __mouseinfo.mouse_lb=true;
            break;
        case WM_MBUTTONDOWN:
            __mouseinfo.mouse_mb=true;
            break;
        case WM_RBUTTONDOWN:
            __mouseinfo.mouse_rb=true;
            break;
        case WM_LBUTTONUP:
            __mouseinfo.mouse_lb=false;
            break;
        case WM_MBUTTONUP:
            __mouseinfo.mouse_mb=false;
            break;
        case WM_RBUTTONUP:
            __mouseinfo.mouse_rb=false;
            break;
        case WM_PAINT:
            
            ValidateRect(hWnd, NULL);
            
            break;
         /*
        case WM_TIMER:
            if(__updatefunc!=0){
                __updatefunc();
                InvalidateRect(hWnd,NULL,FALSE);
            }
            break;
        */
        default:
            return DefWindowProc(hWnd,uMsg,wParam,lParam);
    }

    return 0;
}

AK_Window::~AK_Window(){
    DestroyWindow(m_hWnd);
}

int AK_Window::Create(const string &name, bool fullscreen, int posx, int posy, int resx, int resy){
    
    m_resx = resx;
    m_resy = resy;

    HINSTANCE hInstance = GetModuleHandle(NULL);

    WNDCLASS wc;
    wc.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wc.lpfnWndProc = WndProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon=0;
    wc.hCursor=LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground=0;
    wc.lpszMenuName = NULL;
    wc.lpszClassName = "OpenGL";

    RegisterClass(&wc);
       
    DWORD style;
    DWORD exStyle;
    

    int rx, ry;

    if(fullscreen){
        exStyle = WS_EX_APPWINDOW;
        style = WS_MAXIMIZE | WS_POPUP | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
        rx = GetSystemMetrics( SM_CXSCREEN );
        ry = GetSystemMetrics( SM_CYSCREEN );
        m_resx = rx;
        m_resy = ry;
    }else{
        exStyle = WS_EX_APPWINDOW;
        style =  WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
        RECT client;
        client.top=0;
        client.left=0;
        client.bottom=resy;
        client.right=resx;
        BOOL flag =AdjustWindowRectEx(&client,style,FALSE,exStyle);
        rx = client.right-client.left;
        ry = client.bottom-client.top;
    }

    
    m_hWnd = CreateWindowEx(exStyle, 
                            "OpenGL", 
                            name.c_str(),
                            style,
                            posx,
                            posy,
                            rx,
                            ry,
                            NULL,
                            NULL,
                            hInstance,
                            NULL);

    if(m_hWnd==NULL)
        return 0;

    ShowWindow(m_hWnd, SW_SHOW);
    UpdateWindow(m_hWnd);

    return 1;
}


void AK_Window::Update(){
    //InvalidateRect(m_hWnd,NULL,FALSE);
    UpdateWindow(m_hWnd);
}
const HWND AK_Window::GetWinHandle(){
    return m_hWnd;
}
const HDC AK_Window::GetDC(){
    return __hDC;
}

const HGLRC AK_Window::GetRC(){
    return __hRC;
}

bool* AK_Window::GetKeyBuffer(){
    return __keys;
}

__MouseInfo* AK_Window::GetMouseInfo(){
    return &__mouseinfo;
}

int AK_Window::IsDestroyed(){
    return __destroyed;
}

void  AK_Window::Destroy(){
    __destroyed = 1;
}

int AK_Window::GetResX(){
    return m_resx;
}

int AK_Window::GetResY(){
    return m_resy;
}