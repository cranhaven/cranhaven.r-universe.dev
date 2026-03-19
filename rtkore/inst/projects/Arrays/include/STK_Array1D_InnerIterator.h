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

    Contact: S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
*/

/*
 * Project:  stkpp::Arrays
 * created on: 26 nov. 2007
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_Array1D_InnerIterator.h
  * @brief In this file we define and implement the inner class InnerIterator of Array1D class
 **/

#ifndef STK_ARRAY1D_INNERITERATOR_H
#define STK_ARRAY1D_INNERITERATOR_H

#include "STK_Array1D.h"
#include "iterators/STK_InnerIteratorBase.h"
namespace STK
{

template<class Type_, int Size_ >
class Array1D<Type_, Size_>::InnerOperator: public InnerIteratorBase< Array1D<Type_, Size_>, Array1D<Type_, Size_>::InnerOperator >
{
  public:
    typedef  InnerIteratorBase< Array1D<Type_, Size_>, Array1D<Type_, Size_>::InnerOperator > Base;

    enum
    {
      structure_ = hidden::Traits< Array1D<Type_, Size_> >::structure_,
      orient_    = hidden::Traits< Array1D<Type_, Size_> >::orient_,
      sizeCols_  = hidden::Traits< Array1D<Type_, Size_> >::sizeCols_,
      sizeRows_  = hidden::Traits< Array1D<Type_, Size_> >::sizeRows_,
      size_      = hidden::Traits< Array1D<Type_, Size_> >::size_,
      storage_   = hidden::Traits< Array1D<Type_, Size_> >::storage_
    };

    typedef typename hidden::Traits< Array1D <Type_, Size_> >::Type Type;
    typedef typename hidden::Traits< Array1D <Type_, Size_> >::TypeConst TypeConst;

    typedef typename hidden::RemoveConst<Type>::Type&  reference;
    typedef typename hidden::RemoveConst<Type>::Type*  pointer;

    using Base::p_array_;
    using Base::row_;
    using Base::col_;

    /** default constructor.
     *  @param array instance to iterate
     **/
    InnerOperator( Array1D <Type_, Size_>& array): Base(&array) {}
    /** copy constructor.
     *  @param it iterator to copy
     **/
    InnerOperator( InnerOperator const& it): Base(it) {}

    /** @return @c true if iteration are terminated, @c false otherwise */
    inline operator bool() const { return row_ != p_array_->end(); }
    /** @return current value of the iterator*/
    inline TypeConst valueImpl() const { return p_array_->allocator().elt(row_);}
    /** @return current value of the iterator*/
    inline reference valueRefImpl() const { return p_array_->allocator().elt(row_);}
    /** go to next position */
    inline InnerOperator const& nextImpl() const { row_++; return *this;}
    /** go to previous position */
    inline InnerOperator const& prevImpl() const { row_--; return *this;}
};

} // namespace STK

#endif // STK_ARRAY1DINNERITERATOR_H
