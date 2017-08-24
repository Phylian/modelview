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


#ifndef AK_Component_hpp
#define AK_Component_hpp

#include <string>
#include <list>
#include <rapidxml/rapidxml.hpp>
#include "Python.h"
#include "../io/AIO_XMLToken.hpp"

using namespace std;
using namespace rapidxml;

class AK_Entity;
class AK_Condition;


/// Serves as base class to define new components.
//
/// This means all the components must be derived
/// from this class and implement the virtual methods.
class AK_Component{
    private:
        string     m_type;     
        
        
        bool        m_erase;    // Determines whether the component must be removed or not.

        
        

    protected:
        list<AK_Condition*> m_conds; 
        AK_Entity  *m_owner; 
        
        /// Sets the type of the component.
        //
        /// This method must be called by the derived classes to
        /// assign a type to the component.
        /// Its encouraged to set the type with only
        /// the first letter being uppercase and
        /// the most concisely and descriptive as possible
        /// (v.g "Render" or "Physics")
        int SetType(const string &type); 

 

    public:
        AK_Component();
        virtual ~AK_Component();

        /// Returns the type of the component.
        string GetType();

        /// Marks the component to be erased.
        void Erase();

        /// Returns if the component is marked for erasing.
        bool IsErased();

        /// Assigns an owner to the component.
        int SetOwner(AK_Entity *ent);

        /// Returns a pointer to the owner of the component.
        AK_Entity* GetOwner();
        
        /// The component analyzes a token and modifies 
        /// it's own parameters if needed. (deprecated)
        virtual int Parse(const AIO_XMLToken &token)=0;

        /// The component parses the information from an XML node.
        virtual int Parse(xml_node<> *cond_node)=0;

        /// Creates a new condition on the component.
        virtual AK_Condition* CreateCondition()=0;

        /// Creates a new condition on the component based on another condition.
        //
        /// Works as a copy constructor in some kind of way.
        virtual AK_Condition* CreateCondition(const AK_Condition &cond)=0;

        /// Updates the state of all the conditions.
        //
        /// This means they will be set positive or negative.
        /// The ticks are passed on by the respective
        /// subsystem.
        virtual void UpdateConditions(int ticks)=0;

        /// Returns the condition with the given name.
        AK_Condition* GetCondition(const string &name);
        
        /// Returns a new Python proxy of the component. 
        //
        /// It will be allocated on Python's heap.
        /// Components can be accessed by Python
        /// through proxies.
        virtual PyObject* CreateProxy()=0;

};

/*
#define _APY_COMPONENT_BASE_H \
    static PyObject* _apy_GetType(PyObject *self, PyObject *args, PyObject *kwds); \
    static PyObject* _apy_GetOwner(PyObject *self, PyObject *args, PyObject *kwds);

#define _APY_COMPONENT_METHODS \
    _APY_METHOD("GetType", _apy_GetType, "Returns the type of the component") \
    _APY_METHOD("GetOwner", _apy_GetOwner, "Returns the entity who owns the component")
*/

#endif  
