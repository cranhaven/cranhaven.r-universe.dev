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
 * Project:  STKernel::Base
 * Purpose:  Define the fundamental type Char.
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 *
 **/

/** @file STK_Char.h
 *  @brief In this file we define the fundamental type Char.
 **/

#ifndef STK_CHAR_H
#define STK_CHAR_H

namespace STK
{

/** @ingroup Arithmetic
 *  @brief Specialization of the struct Arithmetic for Char.
 *
 * The STK fundamental type Char does not have NA value. If the user
 * ask for a NA value, the method return 0.
 */
template<>
struct Arithmetic<Char> : public std::numeric_limits<Char>
{
  /** Adding a Non Available (NA) special number.
   *  @return the 0 character
   **/
  static Char NA() throw() { return static_cast<Char>(0); }

  /** True if the type has a representation for a "Not Available."
   **/
  static const bool hasNA = false;

  /** Test if x is a Non Available (NA) Char.
   *  @param x the Char to test.
   *  @return always @c false as the Char type does not have NA value.
   **/
  static bool isNA(const Char& x) throw() { return false; }

  /** Test if x is  infinite.
   *  @param x the Char to test.
   *  @return always @c false
   **/
  static bool isInfinite(const Char& x) throw() { return false; }

  /** Test if x is  finite.
   *  @param x the Char to test.
   *  @return always @c true
   **/
  static bool isFinite(const Char& x) throw() { return true; }
};

/** @ingroup RTTI
 *  @brief Specialization of the IdTypeImpl for the Type Char.
 *  This struct return the IdType of a Char.
 **/
template<>
struct IdTypeImpl<Char>
{
  /** Give the IdType of the variable.
   *  @return the IdType
   **/
  static Base::IdType returnType()  { return(Base::character_);}
};


} // namespace STK

#endif /*STK_CHAR_H*/
