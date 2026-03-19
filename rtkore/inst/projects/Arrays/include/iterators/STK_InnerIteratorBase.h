
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
 * created on: 28 marsh 2017
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_InnerIteratorBase.h
 *  @brief In this file we define the base class for inner iterators on arrays.
 **/

#ifndef STK_INNERITERATORBASE_H
#define STK_INNERITERATORBASE_H

#include <iterator>

#include <Sdk.h>

namespace STK
{
/** @ingroup Arrays
 *  @brief InnerIteratorBase is a base class for all iterators on allocators.
 *  Derived class can iterate through allocators what ever how storage is implemented.
 *
 *  Derived classes have to implement the following pseudo virtual methods
 *  @code
 *    TypeConst valueImpl() const; // get current element
 *    reference valueRefImpl() const; // get current element
 *    Derived& nextImpl() const;   // move to next element
 *    Derived& prevImpl() const;   // move to previous element
 *  @endcode
 *
 *  @tparam Derived array to iterate
 *  @tparam Array array to iterate
 **/
template<class Array, class Derived>
class InnerIteratorBase: public IRecursiveTemplate<Derived>
{
  protected:
    /** default constructor.
     *  @param p_array pointer on the instance to iterate
     **/
    InnerIteratorBase( Array* p_array)
                     : p_array_(p_array)
                     , row_(p_array->beginRows())
                     , col_(p_array->beginCols())
    {}
    /** copy constructor.
     *  @param it iterator to copy
     **/
    InnerIteratorBase( InnerIteratorBase const& it)
                     : p_array_(it.p_array)
                     , row_(it.row_)
                     , col_(it.col_)
    {}
    /** destructor */
    ~InnerIteratorBase() {}

  public:
    enum
    {
      structure_ = hidden::Traits< Array >::structure_,
      orient_    = hidden::Traits< Array >::orient_,
      sizeCols_  = hidden::Traits< Array >::sizeCols_,
      sizeRows_  = hidden::Traits< Array >::sizeRows_,
      size_      = hidden::Traits< Array >::size_,
      storage_   = hidden::Traits< Array >::storage_
    };

    typedef typename hidden::Traits< Array >::Type Type;
    typedef typename hidden::Traits< Array >::TypeConst TypeConst;

    typedef typename hidden::RemoveConst<Type>::Type&  reference;
    typedef typename hidden::RemoveConst<Type>::Type*  pointer;

    /** get current column of the iterator*/
    inline int col() const {return col_;}
    /** get current row of the iterator*/
    inline int row() const {return row_;}

    /** go to next position */
    inline Derived const& next() const { return this->asDerived().nextImpl();}
    /** go to previous position */
    inline Derived const& prev() const { return this->asDerived().prevImpl();}
    /** go to next position */
    inline Derived const& operator++() const { return next();}
    /** go to next position */
    inline Derived const& operator++(int) const  { return next();}
    /** go to previous position */
    inline Derived const& operator--()   const  { return prev();}
    /** go to previous position */
    inline Derived const& operator--(int) const  { return prev();}

    /** get current value of the iterator*/
    inline TypeConst value() const  { return this->asDerived().valueImpl();}
    /** get current value of the iterator*/
    inline reference valueRef() const  { return this->asDerived().valueRefImpl();}
    /** get a reference on the current element (the one at position pos_) */
    inline reference operator*() const { return value();}
    /** get address of the current element (the one at position pos_) */
    inline pointer operator->() const { return &(value());}

  protected:
    /** pointer on instance */
    Array* const p_array_;
    /** current row position */
    mutable int row_;
    /** current column position */
    mutable int col_;
};


} // namespace STK

#endif /* STK_INNERITERATORBASE_H */
