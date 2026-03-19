/*--------------------------------------------------------------------*/
/*  Copyright (C) 2004-2016  Serge Iovleff, Université Lille 1, Inria

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
 * created on: 23 août 2012
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_Typedefs.h
 *  @brief In this file we define the typedef used throughout the stk++ library.
 **/


#ifndef STK_TYPEDEFS_H
#define STK_TYPEDEFS_H

#include <string>

namespace STK
{
/** @ingroup Sdk
 *  @brief STK fundamental type of a Char.
 *
 *  The type Char is defined for the internal representation
 *  of the characters. Note that if you change the representation,
 *  you don't have to modify the stream classes defined in
 *  the file STK_StreamBase.h, but you have to modify the
 *  global stream objects std::cout, std::cin, std::cerr, std::clog
 *  in order to access to the standard I/O channels.
 **/
#ifdef STK_UNICODE

typedef wchar_t Char;
/** Transform x to wchar_t*/
#define _T(x) L ## x
/** Standard stk output stream */
#define stk_cout std::wcout
/** Standard stk input stream */
#define stk_cin  std::wcin
/** Standard stk error stream*/
#define stk_cerr std::wcerr
/** Standard stk log stream*/
#define stk_clog std::wclog

#else // !Unicode

typedef char Char;
/** Let x unmodified*/
#define _T(x) x
/** Standard stk output stream*/
#define stk_cout std::cout
/** Standard stk input stream*/
#define stk_cin  std::cin
/** Standard stk error stream*/
#define stk_cerr std::cerr
/** Standard stk log stream*/
#define stk_log std::clog

#endif // STK_UNICODE

/** @ingroup Sdk
 *  @brief STK fundamental type of a String.
 *
 *  The type String is defined for the internal representation
 *  of the string variables (strings).
 **/
typedef std::basic_string<Char> String;

/** @ingroup Sdk
 *  @brief STK fundamental type of integer values.
 *
 *  The type Integer is defined for the numerical computation and the
 *  internal representation of the discrete variables.
 **/
typedef int Integer ;

#ifdef STKREALAREFLOAT
/**  @ingroup Sdk,Base
  *  @brief STK fundamental type of Real values.
  *
  *  The type Real is defined for the numerical computation and the
  *  internal representation of the continuous variables. By default it is the
  *  @c double, but it can be overridden at compile-time by enabling
  *  the STKREALAREFLOAT macro.
  **/
typedef  float Real;

#else
/**  @ingroup Sdk
  *  @brief STK fundamental type of Real values.
  *
  *  The type Real is defined for the numerical computation and the
  *  internal representation of the continuous variables. By default it is the
  *  double type, but it can be overridden at compile-time, if needed.
  **/
typedef double Real;

#endif

} // namespace STK

#endif /* STK_TYPEDEFS_H */
