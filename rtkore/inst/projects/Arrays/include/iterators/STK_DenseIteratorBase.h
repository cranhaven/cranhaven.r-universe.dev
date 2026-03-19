
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

/** @file STK_DenseIteratorBase.h
 *  @brief In this file we define the base class for Iterators on dense arrays.
 **/

#ifndef STK_DENSEITERATORBASE_H
#define STK_DENSEITERATORBASE_H

namespace STK
{
namespace hidden
{
/** @ingroup hidden, Arrays
 *  @brief The traits struct IteratorTraits must be specialized for any iterator
 *  derived from the base class IteratorBase.
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
struct DenseIteratorBase: public IRecursiveTemplate<Derived>
{
    typedef typename hidden::IteratorTraits<Derived>::Index Index;

    typedef typename hidden::IteratorTraits<Derived>::iterator_category iterator_category;
    typedef typename hidden::IteratorTraits<Derived>::value_type value_type;
    typedef typename hidden::IteratorTraits<Derived>::reference reference;
    typedef typename hidden::IteratorTraits<Derived>::pointer pointer;
    typedef typename hidden::IteratorTraits<Derived>::difference_type difference_type;

  protected:
    /** default constructor */
    DenseIteratorBase(): pos_(baseIdx) {}
    /** constructor with specified position
     *  @param pos position of the iterator on the array
     **/
    DenseIteratorBase( Index pos): pos_(pos) {}
    /** copy constructor.
     *  @param it iterator to copy
     **/
    DenseIteratorBase( DenseIteratorBase const& it):  pos_(it.pos_) {}
    /** destructor */
    ~DenseIteratorBase() {}

  public:
    /** @return current position of the iterator */
    inline Index pos() const  { return pos_;}

    // moving
    /** next position */
    Derived& operator++()         { ++pos_; return this->asDerived(); }
    /** next position */
    Derived& operator++(int junk) { ++pos_; return this->asDerived(); }
    /** previous position */
    Derived& operator--()         { --pos_; return this->asDerived(); }
    /** previous position */
    Derived& operator--(int)      { --pos_; return this->asDerived(); }
    /** add n positions to current position */
    Derived& operator+=(Index n)    { pos_+=n; return this->asDerived(); }
    /** Subtract n positions to current position */
    Derived& operator-=(Index n)    { pos_-=n; return this->asDerived(); }

    /** add n positions to current position */
    friend DenseIteratorBase operator+( DenseIteratorBase const& it, int n)
    /** add n positions to current position */
    { DenseIteratorBase r(it); r+=n ; return r; }
    friend DenseIteratorBase operator+(int n, DenseIteratorBase const& it)
    /** Subtract n positions to current position */
    { DenseIteratorBase r(it); r+=n ; return r; }
    /** Subtract n positions to current position */
    friend DenseIteratorBase operator-( DenseIteratorBase const& it, int n)
    { DenseIteratorBase r(it); r-=n ; return r; }
    friend DenseIteratorBase operator-(int n, DenseIteratorBase const& it)
    { DenseIteratorBase r(it); r-=n ; return r; }

    /** comparing two iterators (only position is compared !) */
    friend bool operator<(DenseIteratorBase const& lhs, DenseIteratorBase const& rhs)
    { return lhs.pos_ < rhs.pos_; };
    /** comparing two iterators (only position is compared !) */
    friend bool operator>(DenseIteratorBase const& lhs, DenseIteratorBase const& rhs)
    { return lhs.pos_ > rhs.pos_; };
    /** comparing two iterators (only position is compared !) */
    friend bool operator<=(DenseIteratorBase const& lhs, DenseIteratorBase const& rhs)
    { return lhs.pos_ <= rhs.pos_; };
    /** comparing two iterators (only position is compared !) */
    friend bool operator>=(DenseIteratorBase const& lhs, DenseIteratorBase const& rhs)
    { return lhs.pos_ >= rhs.pos_; };

    // misc
    /** swap two iterators (only position is swaped) */
    friend void swap(DenseIteratorBase& lhs, DenseIteratorBase& rhs)
    { std::swap(lhs.pos_, rhs.pos_);}
    /** difference of two positions */
    friend difference_type operator-(DenseIteratorBase it1, DenseIteratorBase it2)
    { return it1.pos_ - it2.pos_;}


  protected:
    /** Current position */
    Index pos_;
};


} // namespace STK

#endif /* STK_DENSEITERATORBASE_H */
