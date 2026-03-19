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
 * created on: 4 déc. 2015
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_BiDirectionalIterator.h
 *  @brief In this file we implement the BiDirectionalIterator class.
 **/

#ifndef STK_BIDIRECTIONALITERATOR_H
#define STK_BIDIRECTIONALITERATOR_H

#include "STK_DenseIteratorBase.h"

namespace STK
{

// forward declaration
template<class Array> struct BiDirectionalIterator;
template<class Array> struct ConstBiDirectionalIterator;

namespace hidden
{
  /** @ingroup hidden
   *  @brief Specialization for the BiDirectionalIterator iterator class
   **/
  template<class Array_>
  struct IteratorTraits<BiDirectionalIterator<Array_> >
  {
    typedef Array_ Array;

    enum
    {
      structure_ = hidden::Traits< Array_ >::structure_,
      orient_    = hidden::Traits< Array_ >::orient_,
      sizeCols_  = hidden::Traits< Array_ >::sizeCols_,
      sizeRows_  = hidden::Traits< Array_ >::sizeRows_,
      size_      = hidden::Traits< Array_ >::size_,
      storage_   = hidden::Traits< Array_ >::storage_
    };

    typedef typename hidden::Traits< Array_ >::Type Type;
    typedef typename hidden::Traits< Array_ >::TypeConst TypeConst;

    typedef typename hidden::Traits< Array_ >::RowRange RowRange;
    typedef typename hidden::Traits< Array_ >::ColRange ColRange;

    typedef typename hidden::Traits< Array_ >::Index Index;

    // std compatibility
    typedef std::bidirectional_iterator_tag iterator_category;
    typedef Type value_type;
    typedef Index difference_type;
    typedef value_type* pointer;
    typedef value_type& reference;
};

  /** @ingroup hidden
   *  @brief Specialization for the ConstBiDirectionalIterator iterator class
   **/
  template<class Array_>
  struct IteratorTraits<ConstBiDirectionalIterator<Array_> >
  {
    typedef Array_ Array;

    enum
    {
      structure_ = hidden::Traits< Array_ >::structure_,
      orient_    = hidden::Traits< Array_ >::orient_,
      sizeCols_  = hidden::Traits< Array_ >::sizeCols_,
      sizeRows_  = hidden::Traits< Array_ >::sizeRows_,
      size_      = hidden::Traits< Array_ >::size_,
      storage_   = hidden::Traits< Array_ >::storage_
    };

    typedef typename hidden::Traits< Array_ >::Type Type;
    typedef typename hidden::Traits< Array_ >::TypeConst TypeConst;

    typedef typename hidden::Traits< Array_ >::RowRange RowRange;
    typedef typename hidden::Traits< Array_ >::ColRange ColRange;

    typedef typename hidden::Traits< Array_ >::Index Index;

    // std compatibility
    typedef std::bidirectional_iterator_tag iterator_category;
    typedef Type value_type;
    typedef Index difference_type;
    typedef value_type const* pointer;
    typedef value_type const& reference;
};

} // namespace hidden


/** @ingroup Arrays
 *  @brief BiDirectionalIterator allows to loop over the element of
 *  one dimensional list containers.
 *
 *  @sa STK::List1D
 **/
template<class Array>
struct BiDirectionalIterator: public DenseIteratorBase< BiDirectionalIterator<Array> >
                            , public hidden::Traits< Array >::RowRange
{
  public:
    typedef DenseIteratorBase< BiDirectionalIterator > Base;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::Index Index;

    enum
    {
      structure_ = hidden::IteratorTraits< BiDirectionalIterator >::structure_,
      orient_    = hidden::IteratorTraits< BiDirectionalIterator >::orient_,
      sizeCols_  = hidden::IteratorTraits< BiDirectionalIterator >::sizeCols_,
      sizeRows_  = hidden::IteratorTraits< BiDirectionalIterator >::sizeRows_,
      size_      = hidden::IteratorTraits< BiDirectionalIterator >::size_,
      storage_   = hidden::IteratorTraits< BiDirectionalIterator >::storage_
    };

    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::Type Type;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::TypeConst TypeConst;

    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::RowRange RowRange;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::ColRange ColRange;

    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::iterator_category iterator_category;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::value_type value_type;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::reference reference;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::pointer pointer;
    typedef typename hidden::IteratorTraits< BiDirectionalIterator >::difference_type difference_type;

    using Base::pos_;

    // creating
    BiDirectionalIterator( Array& list1D, int pos)
                         : Base(pos), RowRange(), list1D_(list1D)
    {}
    BiDirectionalIterator( BiDirectionalIterator const& it)
                          : Base(it), RowRange(it), list1D_(it.list1D_) {}
    ~BiDirectionalIterator() {}
    BiDirectionalIterator& operator=(BiDirectionalIterator const& it)
    {
      Base::operator =(it);
      RowRange::operator =(it);
      list1D_ = it.list1D_;
      return *this;
    }
    // getting
    reference operator*() { return list1D_[pos_]; }
    pointer operator->()  { return &(list1D_[pos_]); }
    // misc
    friend void swap(BiDirectionalIterator& lhs, BiDirectionalIterator& rhs)
    {
      Base::swap(lhs, rhs);
      std::swap(lhs.list1D_, rhs.list1D_);
    }

  private:
    Array& list1D_;
};

/** @ingroup Arrays
 *  @brief ConstBiDirectionalIterator allows to loop over the element of
 *  one dimensional list containers.
 *
 *  @sa STK::List1D
 **/
template<class Array>
struct ConstBiDirectionalIterator: public DenseIteratorBase< ConstBiDirectionalIterator<Array> >
                                 , public hidden::Traits< Array >::RowRange
{
  public:
    typedef DenseIteratorBase< ConstBiDirectionalIterator > Base;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::Index Index;

    enum
    {
      structure_ = hidden::IteratorTraits< ConstBiDirectionalIterator >::structure_,
      orient_    = hidden::IteratorTraits< ConstBiDirectionalIterator >::orient_,
      sizeCols_  = hidden::IteratorTraits< ConstBiDirectionalIterator >::sizeCols_,
      sizeRows_  = hidden::IteratorTraits< ConstBiDirectionalIterator >::sizeRows_,
      size_      = hidden::IteratorTraits< ConstBiDirectionalIterator >::size_,
      storage_   = hidden::IteratorTraits< ConstBiDirectionalIterator >::storage_
    };

    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::Type Type;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::TypeConst TypeConst;

    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::RowRange RowRange;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::ColRange ColRange;

    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::iterator_category iterator_category;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::value_type value_type;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::reference reference;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::pointer pointer;
    typedef typename hidden::IteratorTraits< ConstBiDirectionalIterator >::difference_type difference_type;

    using Base::pos_;

  public:
    // creating
    ConstBiDirectionalIterator( Array const& list1D, int pos)
                              : Base(pos)
                              , RowRange()
                              , list1D_(list1D)
    {}
    ConstBiDirectionalIterator( ConstBiDirectionalIterator const& it)
                              : Base(it), RowRange(it), list1D_(it.list1D_)
    {}
    ~ConstBiDirectionalIterator() {}
    ConstBiDirectionalIterator& operator=(ConstBiDirectionalIterator const& it)
    {
      Base::operator=(it);
      RowRange::operator=(it);
      list1D_ = it.list1D_;
      return *this;
    }
    // getting
    reference operator*() { return list1D_[pos_]; }
    pointer operator->()  { return &(list1D_[pos_]); }
    // misc
    friend void swap(ConstBiDirectionalIterator& lhs, ConstBiDirectionalIterator& rhs)
    {
      Base::swap(lhs, rhs);
      std::swap(lhs.list1D_, rhs.list1D_);
    }

  private:
    Array const& list1D_;
};

} // namespace STK

#endif /* STK_BIDIRECTIONALITERATOR_H */
