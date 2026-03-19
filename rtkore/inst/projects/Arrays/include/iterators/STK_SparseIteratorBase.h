
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

/** @file STK_SparseIteratorBase.h
 *  @brief In this file we define the base class for Iterators on sparse arrays
 **/

#ifndef STK_ITERATORBASE_H
#define STK_ITERATORBASE_H

#include <Sdk.h>

namespace STK
{
namespace hidden
{
/** @ingroup hidden, Arrays
 *  @brief The traits struct IteratorTraits must be specialized for any iterator
 *  derived from the base class SparseIteratorBase.
 *
 *  @note We use the type names defined by the STL for the iterator_traits class.
 *
 *  For example:
 *  @code
 *  template<typename Type>
 *  struct IteratorTraits
 *  {
 *    /// One of the iterator_tags types
 *    typedef std::random_access_iterator_tag  iterator_category;
 *    /// The type "pointed to" by the iterator.
 *    typedef Type        value_type;
 *    /// Distance between iterators is represented as this type.
 *    typedef int  difference_type;
 *    /// This type represents a pointer-to-value_type.
 *    typedef Type*   pointer;
 *    /// This type represents a reference-to-value_type.
 *    typedef Type& reference;
 *  };
 *  @endcode
 */
template <typename Derived> struct IteratorTraits;

} // namespace hidden

/** @ingroup Arrays
 *  @brief IteratorBase is a base class for all iterators on dense
 *  arrays/matrix/vector/expressions
 *
 *  @tparam Derived the derived class
 **/
template<class Derived>
struct SparseIteratorBase: public IRecursiveTemplate<Derived>
{
  private:
    /** default constructor */
    SparseIteratorBase(): pos_(){}

    typedef typename hidden::IteratorTraits<Derived>::Index Index;

    typedef typename hidden::IteratorTraits<Derived>::iterator_category iterator_category;
    typedef typename hidden::IteratorTraits<Derived>::value_type value_type;
    typedef typename hidden::IteratorTraits<Derived>::reference reference;
    typedef typename hidden::IteratorTraits<Derived>::pointer pointer;
    typedef typename hidden::IteratorTraits<Derived>::difference_type difference_type;

  protected:

    /** constructor with specified position
     *  @param pos position of the iterator on the array
     **/
    SparseIteratorBase( Index& pos): pos_(pos) {}
    /** copy constructor.
     *  @param it iterator to copy
     **/
    SparseIteratorBase( SparseIteratorBase const& it):  pos_(it.pos_) {}
    /** destructor */
    ~SparseIteratorBase() {}

  public:
    /** @return the position of the iterator */
    int pos() const  { return pos_;}

    // moving
    /** next position */
    Derived& operator++()         { ++pos_; return this->asDerived(); }
    /** next position */
    Derived& operator++(int junk) { ++pos_; return this->asDerived(); }
    /** previous position */
    Derived& operator--()         { --pos_; return this->asDerived(); }
    /** previous position */
    Derived& operator--(int)      { --pos_; return this->asDerived(); }

    Derived& operator+=(int n)    { pos_+=n; return this->asDerived(); }
    Derived& operator-=(int n)    { pos_-=n; return this->asDerived(); }
    friend SparseIteratorBase operator+( SparseIteratorBase const& it, int n)
    { SparseIteratorBase r(it); r+=n ; return r; }
    friend SparseIteratorBase operator+(int n, SparseIteratorBase const& it)
    { SparseIteratorBase r(it); r+=n ; return r; }
    friend SparseIteratorBase operator-( SparseIteratorBase const& it, int n)
    { SparseIteratorBase r(it); r-=n ; return r; }
    friend SparseIteratorBase operator-(int n, SparseIteratorBase const& it)
    { SparseIteratorBase r(it); r-=n ; return r; }

    friend difference_type operator-(SparseIteratorBase it1, SparseIteratorBase it2)
    { return it1.pos_ - it2.pos_;}

    // comparing
    /** comparing two iterators (only position is compared !) */
    bool operator==( SparseIteratorBase const& rhs) { return(pos_ ==rhs.pos_); }
    /** comparing two iterators (only position is compared !) */
    bool operator!=( SparseIteratorBase const& rhs) { return(pos_!=rhs.pos_); }

    /** comparing two iterators (only position is compared !) */
    friend bool operator<(SparseIteratorBase const& lhs, SparseIteratorBase const& rhs)
    { return lhs.pos_ < rhs.pos_; };
    /** comparing two iterators (only position is compared !) */
    friend bool operator>(SparseIteratorBase const& lhs, SparseIteratorBase const& rhs)
    { return lhs.pos_ > rhs.pos_; };
    /** comparing two iterators (only position is compared !) */
    friend bool operator<=(SparseIteratorBase const& lhs, SparseIteratorBase const& rhs)
    { return lhs.pos_ <= rhs.pos_; };
    /** comparing two iterators (only position is compared !) */
    friend bool operator>=(SparseIteratorBase const& lhs, SparseIteratorBase const& rhs)
    { return lhs.pos_ >= rhs.pos_; };

    /** swap two iterators (only position is swaped) */
    friend void swap(SparseIteratorBase& lhs, SparseIteratorBase& rhs)
    { std::swap(lhs.pos_, rhs.pos_);}

  protected:
    /** Current position */
    Index& pos_;
};


} // namespace STK

#endif /* STK_ITERATORBASE_H */
