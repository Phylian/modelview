// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H5FloatType_H
#define _H5FloatType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP FloatType : public AtomType {
   public:
        // Creates a floating-point type using a predefined type
        FloatType( const PredType& pred_type );

	// Gets the floating-point datatype of the specified dataset
	FloatType( const DataSet& dataset );

	// Retrieves the exponent bias of a floating-point type.
	size_t getEbias() const;

	// Sets the exponent bias of a floating-point type.
	void setEbias( size_t ebias ) const;

	// Retrieves floating point datatype bit field information.
	void getFields( size_t& spos, size_t& epos, size_t& esize, size_t& mpos, size_t& msize ) const;

	// Sets locations and sizes of floating point bit fields.
	void setFields( size_t spos, size_t epos, size_t esize, size_t mpos, size_t msize ) const;

	// Retrieves the internal padding type for unused bits in floating-point datatypes.
	H5T_pad_t getInpad( string& pad_string ) const;

	// Fills unused internal floating point bits.
	void setInpad( H5T_pad_t inpad ) const;

	// Retrieves mantissa normalization of a floating-point datatype.
	H5T_norm_t getNorm( string& norm_string ) const;

	// Sets the mantissa normalization of a floating-point datatype.
	void setNorm( H5T_norm_t norm ) const;

	// Returns this class name
	virtual string fromClass () const { return ("FloatType"); }

	// Default constructor
	FloatType();

	// Creates a floating-point datatype using an existing id
	FloatType( const hid_t existing_id );

	// Copy constructor: makes a copy of the original FloatType object.
	FloatType( const FloatType& original );

	// Noop destructor.
	virtual ~FloatType();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
