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



#ifndef AK_ScriptQueue_hpp
#define AK_ScriptQueue_hpp

#include <queue>
#include "AK_ScriptCall.hpp"
using namespace std;


/// Queue containing all the scripts that must be
/// executed at a specific frame.
class AK_ScriptQueue{
    private:
        queue<AK_ScriptCall> m_queue;
        
    public:

        /// Insert a script call in the queue.
        int Push(const string &name, AK_Entity* ent);

        /// Pop a script call from the queue.
        AK_ScriptCall Pop();

        /// Number of script calls on the queue.
        int Count();

        /// Is the queue empty?
        int IsEmpty();

        /// Clean the queue
        void Clean();
};

#endif
