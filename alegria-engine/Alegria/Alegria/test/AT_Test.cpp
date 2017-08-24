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



#include <stdlib.h>


#include <iostream>
#include "..\Alegria.hpp"
#include "Python.h"

using namespace std;



int main(int argc, char *argv[]){
   // _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
    Alegria *engine = new Alegria;

    
    engine->Init();
	if(argc==2)
		engine->Run("Alegria Demo #1", false, 100, 100, 800, 600,argv[1]);
	else if(argc==3)
		if(string(argv[2])=="1")
			engine->Run("Alegria Demo #1", true, 100, 100, 800, 600,argv[1]);
		else
			engine->Run("Alegria Demo #1", false, 100, 100, 800, 600,argv[1]);
	else
		engine->Run("Alegria Demo #1", false, 100, 100, 800, 600,"main.xml");

    delete engine;

   
  
    return 0;
    
}



