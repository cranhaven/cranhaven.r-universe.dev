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
 * Project:  stkpp::Arrays
 * created on: 04/12/2018
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_Appliers.h
 *  @brief In this file we define the appliers classes.
 **/

#ifndef STK_APPLIERS_H
#define STK_APPLIERS_H


namespace STK
{

namespace hidden
{
/** @ingroup hidden
 *  @brief Applier setting uniform random numbers
 *
 * @sa STK::ExprBase::randGauss(), STK::ExprBase::rand()
 */
template <typename Type>
struct RandUnifApplier
{
  typedef typename RemoveConst<Type>::Type TypeConst;
  inline TypeConst operator()()
  { return Type(Law::generator.randUnif());}
};

/** @ingroup hidden
 *  @brief Visitor setting Gaussian random variables
 *  @sa STK::ExprBase::RandUnif()
 */
template <typename Type>
struct RandGaussApplier
{
  typedef typename RemoveConst<Type>::Type TypeConst;
  inline TypeConst operator()()
  { return Type(Law::generator.randGauss());}
};

/** @ingroup hidden
 *  @brief Visitor putting a choosen random variable
 */
template <typename Type>
struct RandApplier
{
  typedef typename RemoveConst<Type>::Type TypeConst;
  inline RandApplier( Law::IUnivLaw<Type> const& law):law_(law){}
  inline TypeConst operator()() { return law_.rand();}
  Law::IUnivLaw<Type> const& law_;
};

/** @ingroup hidden
  * @brief visitor putting a constant value
  */
template <typename Type>
struct ValueApplier
{
  typedef typename RemoveConst<Type>::Type TypeConst;
  Type value_;
  inline ValueApplier(Type const& value) : value_(value) {};
  inline TypeConst operator()() { return value_;}
};

} //namespace hidden

} // namespace STK

#endif /* STK_APPLIERS_H */
