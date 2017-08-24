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

#include "../io/AIO_FileManager.hpp"
#include "../io/AIO_TextBuffer.hpp"
#include "AK_ScriptCall.hpp"
#include "AK_ScriptInterpreter.hpp"
#include "AK_ScriptQueue.hpp"
#include "AK_Entity.hpp"
#include "..\proxies\APY_InterfaceProxy.hpp"

#include <iostream>
#include <fstream>

using namespace std;



AK_ScriptInterpreter::AK_ScriptInterpreter(AK_ScriptQueue* squeue, AIO_FileManager* filemngr):m_filemngr(filemngr),m_squeue(squeue){
    Py_Initialize();

    main_module = PyImport_AddModule("__main__");

    main_dict = PyModule_GetDict(main_module);
    
    Py_InitModule("Alegria", EmbMethods);
}

AK_ScriptInterpreter::~AK_ScriptInterpreter(){
    Py_Finalize();
}



int AK_ScriptInterpreter::CallScript(const AK_ScriptCall &call){
	string name = call.GetScriptName();
    
    AIO_TextBuffer *script = m_filemngr->GetScript(name);

    if(!script){
       cout << "Script " << name << " not loaded" <<endl;
       system("pause");
       return 0;
    }

    PyObject *bytecode = script->GetByteCode();
    
    AK_Entity *ent = call.GetCaller();
   
    PyObject* main_dict = PyModule_GetDict(main_module);
    PyObject* main_dict_copy = PyDict_Copy(main_dict);


    PyObject *prox = ent->CreateProxy();
    PyDict_SetItemString(main_dict_copy,"owner", prox);
    
    PyEval_EvalCode((PyCodeObject*)bytecode,main_dict_copy,main_dict_copy);
 
    PyObject *error = PyErr_Occurred();
    
    if(error){
        PyErr_PrintEx(0);
    }
    PyDict_Clear(main_dict_copy);
    Py_DECREF(main_dict_copy);
    Py_DECREF(prox);
    return 1;
}

void AK_ScriptInterpreter::CallAll(){
    
    
    
    while(!m_squeue->IsEmpty()){
        AK_ScriptCall call = m_squeue->Pop();
     
        CallScript(call);
    }
    
}