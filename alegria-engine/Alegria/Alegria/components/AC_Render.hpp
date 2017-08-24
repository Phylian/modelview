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
 * Render Component
*/

#ifndef AC_Render_hpp
#define AC_Render_hpp

#include "../kernel/AK_Component.hpp"
#include "../utils/AK_Utils.hpp"

class AIO_ImageManager;
class AIO_Image;
class AK_Condition;
class AS_Render;

class AC_Render:public AK_Component{
    private:
        AIO_Image           *m_image;
        bool                m_visible;
        unsigned int        m_layer;
        float               m_alpha;
        AIO_ImageManager*   m_imgmngr;
        int                 m_frame;
        bool                m_text;

        AS_Render           *m_asr;
    public:
        AC_Render(AIO_ImageManager* imgmngr);
        AC_Render(const AC_Render &der);
        

        void SetVisibility(bool vis);
        bool IsVisible();

		bool IsOnSight();

        void SetRenderLayer(unsigned int layer);
        unsigned int GetRenderLayer();

        int Parse(const AIO_XMLToken &token);
		int Parse(xml_node<> *comp_node);

        AK_Condition* CreateCondition();
        AK_Condition* CreateCondition(const AK_Condition &cond);
        void UpdateConditions(int ticks);

        int SetImage(const string &name);
        AIO_Image* GetImage();
        int GetCurrentFrame();
        void SetCurrentFrame(int frame);

        Float2d GetCameraPosition();
        void SetCameraPosition(float x, float y);

        float GetZoomDistance();
        void SetZoomDistance(float zoom);

        float GetAlpha();
        void SetAlpha(float alpha);

        bool IsText();

		void SetBackgroundColor(float r, float g, float b);

        PyObject* CreateProxy();
        
        void ConnectSubsystem(AS_Render *asr);

};


#endif  