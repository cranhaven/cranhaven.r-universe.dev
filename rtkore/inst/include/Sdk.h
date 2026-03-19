/*--------------------------------------------------------------------*/
/*     Copyright (C) 2004-2016  Serge Iovleff, Université Lille 1, Inria

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this program; if not, write to the
    Free Software Foundation, Inc.,
    59 Temple Place,
    Suite 330,
    Boston, MA 02111-1307
    USA

    Contact : S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
*/

/*
 * Project:  stkpp::Sdk
 * Purpose:  main include file for Sdk project.
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 *
 **/

/** @file Sdk.h
 *  @brief This file include all the other header files of the project Sdk.
 *
 * @defgroup Sdk Software Development Kit.
 * @brief The Sdk project propose a set of high level interfaces, template for
 * meta-programming and macros that are used throughout the STK++ projects.
 *
 * In Sdk we define the pure Interface classes than can be used throughout the
 * STK++ whole project. The aim is to unified the syntax and the treatment
 * realized by the statistical methods.
 **/
/** @ingroup Sdk
 *  @defgroup Arithmetic Arithmetic properties.
 *
 *  These classes extend the @c numeric_limits C++ struct. They
 *  allow to handle in a transparent way a possible
 *  Not Available (NA) value in the data.
 **/
/** @ingroup Sdk
 *  @defgroup RTTI Runtime Type Identification.
 *
 *  These classes allow to handle the Runtime type identification (RTTI)
 *  problem and are useful when working with heterogeneous data.
 **/
/** @ingroup Sdk
 *  @defgroup iostream I/O stream declarations
 *
 *  Nearly all of the I/O classes are parameterized on the type of
 *  characters they read and write (The major exception is ios_base at
 *  the top of the hierarchy).
 *
 *  For ease of use, all of the basic_* I/O-related
 *  classes are given typedef names in the namespace STK. For example:
 *
 *  @code
 *     typedef basic_ifstream<Char>  ifstream;
 *  @endcode
 *  These declarations in the STK namespace would be very useful if you
 *  modify the built-in type representation of Char (say wchar_t).
 **/


/** @ingroup Sdk
 *  @namespace STK::Base
 *  @brief the namespace Base contain all the internal stuff needed by the STK++
 *  fundamental types.
 **/

#ifndef SDK_H
#define SDK_H

/* Macros and typedefs */
#include <Sdk/include/STK_MacrosVersion.h>
/* Constants */
#include <Sdk/include/STK_Constants.h>
/* Interface for all classes using the curious recursive template paradigm.*/
#include <Sdk/include/STK_IRecursiveTemplate.h>
/* struct extending numeric_limits.*/
#include <Sdk/include/STK_Arithmetic.h>
/* struct for RTTI (Run Time Type Identification) */
#include <Sdk/include/STK_IdTypeImpl.h>
/* typedefs */
#include <Sdk/include/STK_Typedefs.h>
/* streaming */
#include <Sdk/include/STK_Stream.h>
/* Templates */
#include <Sdk/include/STK_MetaTemplate.h>
/* Static Assert. */
#include <Sdk/include/STK_StaticAssert.h>
/* Exceptions */
#include <Sdk/include/STK_Exceptions.h>
/* Macros */
#include <Sdk/include/STK_Macros.h>
/* Interface for all runners classes */
#include <Sdk/include/STK_IRunner.h>

#endif  /* SDK_H */
