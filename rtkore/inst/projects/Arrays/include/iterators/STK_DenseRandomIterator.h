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
 * created on: 10 mars 2017
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_DenseRandomIterator.h
 *  @brief In this file we define and implement the DenseRandomIterator and
 *  ConstDenseRandomIterator classes.
 **/

#ifndef STK_DENSERANDOMITERATOR_H
#define STK_DENSERANDOMITERATOR_H

#include "STK_DenseIteratorBase.h"

namespace STK
{
// forward declaration
template<class Array_> struct DenseRandomIterator;
template<class Array_> struct ConstDenseRandomIterator;

namespace hidden
{
/** @ingroup hidden
 *  @brief Specialization of the IteratorTraits class for the DenseRandomIterator iterator class
 **/
template<class Array_>
struct IteratorTraits< DenseRandomIterator<Array_> >
{
  typedef int Index;
  typedef Array_ Array;

  enum
  {
    structure_ = Traits< Array_ >::structure_,
    orient_    = Traits< Array_ >::orient_,
    sizeCols_  = Traits< Array_ >::sizeCols_,
    sizeRows_  = Traits< Array_ >::sizeRows_,
    size_      = Traits< Array_ >::size_,
    storage_   = Traits< Array_ >::storage_
  };

  typedef typename Traits< Array_ >::Type Type;
  typedef typename Traits< Array_ >::TypeConst TypeConst;

  typedef typename Traits< Array_ >::RowRange RowRange;
  typedef typename Traits< Array_ >::ColRange ColRange;

  // std compatibility
  typedef std::random_access_iterator_tag iterator_category;
  typedef typename Array_::Type value_type;
  typedef int difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;
};

/** @ingroup hidden
 *  @brief Specialization of the IteratorTraits for the ConstDenseRandomIterator iterator class
 **/
template<class Array_>
struct IteratorTraits< ConstDenseRandomIterator<Array_> >
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
  typedef std::random_access_iterator_tag iterator_category;
  typedef Type value_type;
  typedef Index difference_type;
  typedef value_type const* pointer;
  typedef value_type const& reference;
};

} // namespace hidden

/** @ingroup Arrays
 *  @brief DenseRandomIterator allows to loop over the elements of containers Array
 **/
template<class Array_>
struct DenseRandomIterator: public DenseIteratorBase< DenseRandomIterator<Array_> >
                          , public hidden::Traits< Array_ >::RowRange
{
  public:
    typedef DenseIteratorBase< DenseRandomIterator > Base;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::Index Index;

    enum
    {
      structure_ = hidden::IteratorTraits< DenseRandomIterator >::structure_,
      orient_    = hidden::IteratorTraits< DenseRandomIterator >::orient_,
      sizeCols_  = hidden::IteratorTraits< DenseRandomIterator >::sizeCols_,
      sizeRows_  = hidden::IteratorTraits< DenseRandomIterator >::sizeRows_,
      size_      = hidden::IteratorTraits< DenseRandomIterator >::size_,
      storage_   = hidden::IteratorTraits< DenseRandomIterator >::storage_
    };

    typedef typename hidden::IteratorTraits< DenseRandomIterator >::Type Type;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::TypeConst TypeConst;

    typedef typename hidden::IteratorTraits< DenseRandomIterator >::RowRange RowRange;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::ColRange ColRange;

    typedef typename hidden::IteratorTraits< DenseRandomIterator >::iterator_category iterator_category;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::value_type value_type;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::reference reference;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::pointer pointer;
    typedef typename hidden::IteratorTraits< DenseRandomIterator >::difference_type difference_type;

    using Base::pos_;

    /** Default constructor */
    DenseRandomIterator(): Base(), RowRange(), p_array_(0) {}
    /** Constructor with array and position given */
    DenseRandomIterator( Array_& array, int pos)
                       : Base(pos)
                       , RowRange(array.range())
                       , p_array_(&array)
    {}
    /** copy constructor */
    DenseRandomIterator( DenseRandomIterator const& it)
                       : Base(it)
                       , RowRange(it)
                       , p_array_(it.p_array_)
    {}
    ~DenseRandomIterator() {}
    /** assignment operator */
    DenseRandomIterator& operator=(DenseRandomIterator const& it)
    {
      Base::operator=(it);
      RowRange::operator=(it);
      p_array_ = it.p_array_;
      return *this;
    }

    // comparing
    /** comparing two iterators (only position is compared !) */
    bool operator==( DenseRandomIterator const& rhs) { return(pos_==rhs.pos_); }
    /** comparing two iterators (only position is compared !) */
    bool operator!=( DenseRandomIterator const& rhs) { return(pos_!=rhs.pos_); }

    // getting
    inline reference operator*()           { return p_array_->elt(pos_); }
    inline pointer operator->()            { return &(p_array_->elt(pos_)); }
    inline reference operator[](Index pos) { return p_array_->elt(pos); }

    // misc
    friend void swap(DenseRandomIterator& lhs, DenseRandomIterator& rhs)
    {
      Base::swap(lhs, rhs);
      std::swap(lhs.p_array_, rhs.p_array_);
    }

  private:
    Array_* p_array_;
};

/** @ingroup Arrays
 *  @brief ConstDenseRandomIterator allows to loop over the elements of containers Array
 **/
template<class Array>
struct ConstDenseRandomIterator: public DenseIteratorBase< ConstDenseRandomIterator<Array> >
                               , public hidden::Traits< Array >::RowRange
{
    typedef  DenseIteratorBase< ConstDenseRandomIterator<Array> > Base;

    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::Index Index;

    enum
    {
      structure_ = hidden::IteratorTraits< ConstDenseRandomIterator >::structure_,
      orient_    = hidden::IteratorTraits< ConstDenseRandomIterator >::orient_,
      sizeCols_  = hidden::IteratorTraits< ConstDenseRandomIterator >::sizeCols_,
      sizeRows_  = hidden::IteratorTraits< ConstDenseRandomIterator >::sizeRows_,
      size_      = hidden::IteratorTraits< ConstDenseRandomIterator >::size_,
      storage_   = hidden::IteratorTraits< ConstDenseRandomIterator >::storage_
    };

    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::Type Type;
    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::TypeConst TypeConst;

    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::RowRange RowRange;
    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::ColRange ColRange;

    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::iterator_category iterator_category;
    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::value_type value_type;
    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::reference reference;
    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::pointer pointer;
    typedef typename hidden::IteratorTraits< ConstDenseRandomIterator >::difference_type difference_type;

    using Base::pos_;

    /** Default constructor */
    ConstDenseRandomIterator(): Base(), RowRange(), p_array_(0) {}
    /** Constructor with array and position given */
    ConstDenseRandomIterator( Array const& array, int pos)
                            : Base(pos)
                            , RowRange(array.range())
                            , p_array_(&array)
    {}
    ConstDenseRandomIterator( ConstDenseRandomIterator const& it)
                            : Base(it)
                            , RowRange(it)
                            , p_array_(it.p_array_)
    {}
    ~ConstDenseRandomIterator() {}
    ConstDenseRandomIterator& operator=(ConstDenseRandomIterator const& it)
    {
      Base::operator=(it);
      RowRange::operator=(it);
      p_array_ = it.p_array_;
      return *this;
    }
    // getting
    reference operator*() const       { return p_array_->elt(pos_); }
    pointer operator->()  const       { return &(p_array_->elt(pos_)); }
    reference operator[](int pos) const { return p_array_->elt(pos); }

    // comparing
    /** comparing two iterators (only position is compared !) */
    bool operator==( ConstDenseRandomIterator const& rhs) { return(pos_==rhs.pos_); }
    /** comparing two iterators (only position is compared !) */
    bool operator!=( ConstDenseRandomIterator const& rhs) { return(pos_!=rhs.pos_); }

    // misc
    friend void swap(ConstDenseRandomIterator& lhs, ConstDenseRandomIterator& rhs)
    {
      std::swap(lhs.p_array_, rhs.p_array_);
      Base::swap(lhs, rhs);
    }

  private:
    Array const* p_array_;
};

} // namespace STK

#endif /* STK_DENSERANDOMITERATOR_H */
