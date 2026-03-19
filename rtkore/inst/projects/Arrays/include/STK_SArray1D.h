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

/** @file STK_SArray1D.h
  * @brief In this file we define and implement the final class SArray1D
 **/

#ifndef STK_SARRAY1D_H
#define STK_SARRAY1D_H

#include "allocators/STK_MemSAllocator1D.h"

namespace STK
{
template<class Type_, int Size_ = UnknownSize, int NzMax_ = UnknownSize > class SArray1D;

namespace hidden
{
/** @ingroup hidden
 *  @brief Specialization of the Traits class for SArray1D class.
 **/
template<class Type_, int Size_, int NzMax_>
struct Traits< SArray1D<Type_, Size_, NzMax_> >
{
  typedef SArray1D<Type_, 1, 1>     Row;
  typedef SArray1D<Type_, Size_, NzMax_> Col;
  typedef SArray1D<Type_, UnknownSize, NzMax_> SubVector;

  typedef Type_ Type;
  typedef typename RemoveConst<Type>::Type const& TypeConst;

  enum
  {
    structure_ = Arrays::vector_,
    orient_    = Arrays::by_col_,
    sizeCols_  = 1,
    sizeRows_  = Size_,
    size_      = Size_,
    nzmax_     = NzMax_,
    storage_   = Arrays::sparse_
  };

  typedef MemSAllocator1D<Type, nzmax_> Allocator;

  typedef TRange<size_> RowRange;
  typedef TRange<1>     ColRange;

  typedef int Index;

  typedef DenseRandomIterator< SArray1D<Type_, Size_, NzMax_> > Iterator;
  typedef ConstDenseRandomIterator< SArray1D<Type_, Size_, NzMax_> > ConstIterator;

  typedef std::reverse_iterator<Iterator> ReverseIterator;
  typedef std::reverse_iterator<ConstIterator> ConstReverseIterator;
};

} // namespace hidden

}

namespace STK
{
/** @ingroup Arrays
 *  @brief template one dimensional Arrays.
 *
 * An SArray1D is a template non-oriented container (even if the @c Traits
 * struct define it as column oriented) implementing the interface
 * class @c IArray1D.
 *
 * @note It is a final class for the curious recursive paradigm.
 *
 * @tparam Type of the objects stored in the @c SArray1D
 * @tparam Size_ size of the vector if it known. Default is @c UnknownSize
 **/
template<class Type_, int Size_, int NzMax_ >
class SArray1D: public IArray1D< SArray1D<Type_, Size_, NzMax_> >
{
  public:
    typedef IArray1D< SArray1D<Type_, Size_, NzMax_> > Base;

    typedef typename hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::Row Row;
    typedef typename hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::Col Col;
    typedef typename hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::SubVector SubVector;

    enum
    {
      structure_ = hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::structure_,
      orient_    = hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::orient_,
      sizeCols_  = hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::sizeCols_,
      sizeRows_  = hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::sizeRows_,
      size_      = hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::size_,
      storage_   = hidden::Traits< SArray1D<Type_, Size_, NzMax_> >::storage_
    };

    typedef typename hidden::Traits< SArray1D <Type_, Size_> >::Type Type;
    typedef typename hidden::Traits< SArray1D <Type_, Size_> >::TypeConst TypeConst;

    typedef typename hidden::Traits< SArray1D <Type_, Size_> >::Allocator Allocator;

    typedef TRange<size_> RowRange;
    typedef TRange<1>     ColRange;

    using Base::range;
    using Base::begin;
    using Base::end;
    using Base::resize;
    using Base::allocator;

    /** Default constructor. */
    SArray1D(): Base(){}
    /** constructor with a specified Range
     *  @param I range of the container
     **/
    SArray1D( Range const& I): Base(I) {}
    /** Misc constructor with beg and end, initialization with a constant.
     *  @param I,v range and initial value of the container
     *  @note if v is not zero, you get a dense array !
     **/
    SArray1D( Range const& I, Type const& v): Base(I, v) {}
    /** Copy constructor
     *  @param T the container to copy
     *  @param ref @c true if T is wrapped
     **/
    SArray1D( SArray1D const& T, bool ref =false): Base(T, ref) {}
    /** Copy constructor
     *  @param T the container to copy
     *  @param ref @c true if T is wrapped
     **/
    template<int OtherSize_, int OtherNzMax_>
    SArray1D( SArray1D<Type, OtherSize_, OtherNzMax_> const& T, bool ref =false)
            : Base(T, ref) {}
    /** constructor by reference, ref_=1.
     *  @param T,I the container and the range of data to wrap
     *  @param ref @c true if T is wrapped (the default)
     **/
    template<int OtherSize_, int OtherNzMax_>
    SArray1D( SArray1D<Type, OtherSize_, OtherNzMax_> const& T, RowRange const& I, bool ref = true)
            : Base(T, I, ref) {}
    /** Wrapper constructor: the container is a reference of a C-Array.
     *  @param q, I pointer and range of data
     **/
    SArray1D( Type* q, Range const& I): Base(q, I) {}
    /** destructor: allocated memory is liberated by MemAllocator base class. */
    ~SArray1D() {}
    /** operator = : overwrite the SArray1D with T.
     *  @param T the container to copy
     **/
    SArray1D& operator=(SArray1D const& T)
    { return Base::assign(T);}
    /** Copy an other type of 1D array in an SArray1D.
     *  @param T the array to copy
     **/
    template<class OtherArray>
    SArray1D& operator=(ITContainer1D<OtherArray> const& T)
    {
      // check size
      if (range() != T.range()) { resize(T.range());}
      for (int i=begin(); i<end(); i++)
      { allocator().setValue(i, T.elt(i));}
      return *this;
    }
    /** operator= : set the container to a constant value.
     *  @param v the value to set
     **/
    SArray1D& operator=(Type const& v)
    {
      allocator().assign(range(), v);
      return *this;
    }
};

} // namespace STK

#endif // STK_SARRAY1D_H
