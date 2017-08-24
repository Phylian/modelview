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

#include <Python.h>

class AIO_FileManager;
class AK_ScriptCall;
class AK_ScriptQueue;

class AK_ScriptInterpreter{
    private:
        AIO_FileManager* m_filemngr;
        AK_ScriptQueue  *m_squeue;

        PyObject* main_module;
        PyObject* main_dict;
    public:
        AK_ScriptInterpreter(AK_ScriptQueue* squeue, AIO_FileManager* filemngr);
        ~AK_ScriptInterpreter();

        /// Given a script call tells the 
        /// python interpreter to execute a precompiled
        /// script.
        int CallScript(const AK_ScriptCall &call);

        /// Processes all the script calls on the queue,
        /// effectively executing all of them.
        void CallAll();
};