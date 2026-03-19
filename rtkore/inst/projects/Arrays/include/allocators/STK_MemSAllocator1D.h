/*--------------------------------------------------------------------*/
/*     Copyright (C) 2004-2018  Serge Iovleff, Université Lille 1, Inria

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

/* Project: stkpp::Arrays
 * created on: Apr 13, 2018
 * Author: iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_MemSAllocator1D.h
 *  @brief In this file we
 **/



#ifndef STK_MEMSALLOCATOR1D_H
#define STK_MEMSALLOCATOR1D_H

#include "../STK_Array1D.h"

namespace STK
{
// forward declaration
template< typename Type_, int NzMax_=UnknownSize> class MemSAllocator1D;

/** @ingroup Arrays
 *  @brief memory allocator for sparse vectors classes.
 *
 *  @tparam Type_ type of elements stored in this allocator
 *  @tparam NzMax_ maximal number of element in sparse vector
 */
template< typename Type_, int NzMax_>
class MemSAllocator1D: public IContainerRef
{
  public:
    enum
    {
      nzmax_ = (NzMax_< (UnknownSize-2)) ? NzMax_+2 : UnknownSize
    };
    typedef Type_  Type;
    typedef typename hidden::RemoveConst<Type_>::Type const& TypeConst;

    /** Type of the array storing indexes */
    typedef Array1D<int, nzmax_> Indexes;
    /** Type of the array storing data */
    typedef Array1D<Type, nzmax_> Values;
    /** Type of the range of the data */
    typedef TRange<UnknownSize> AllocatorRange;

    using IContainerRef::isRef;
    using IContainerRef::setRef;

    /** default constructor */
    MemSAllocator1D();
    /** constructor with specified dimension
     *  @param I range of the rows
     **/
    MemSAllocator1D( AllocatorRange const& I);
    /** constructor with specified dimensions
     * @param I range of the rows
     * @param nzmax maximal number of data
     **/
    MemSAllocator1D( AllocatorRange const& I, int nzmax);
    /** copy constructor
     *  @param A allocator to copy
     *  @param ref @c true if this copy is just a reference, @c false otherwise
     **/
    MemSAllocator1D( MemSAllocator1D const& A, bool ref =false);
    /** Copy constructor.
     *  @param A allocator to copy
     **/
    template< int OtherNzMax_>
    MemSAllocator1D( MemSAllocator1D<Type, OtherNzMax_> const& A);
    /** Copy constructor. Map a range of A.
     *  @param A,I the allocator and range to reference
     *  @param ref is this a wrapper of A ? (true by default)
     **/
    template<int OtherNzMax_>
    MemSAllocator1D( MemSAllocator1D<Type, OtherNzMax_> const& A
                   , AllocatorRange const& I, bool ref = true);
    /** destructor. Release memory.*/
    ~MemSAllocator1D();

    // getters
    /** @return the range of the data*/
    inline AllocatorRange const& range() const { return range_;}
    /** @return the first index of the data. */
    inline int begin() const { return range_.begin();}
    /**@return the ending index of the data */
    inline int end() const { return range_.end();}
    /** @return the size of the data */
    inline int size() const { return range_.size();}

    /** @return the first index of the index array */
    inline int beginIdx() const { return p_idx_->begin();}
    /**@return the ending index of the index array */
    inline int endIdx() const { return p_idx_->end();}
    /** @return the size of the  index array */
    inline int sizeIdx() const { return p_idx_->size();}

    /** @return the vector with the indexes */
    inline Indexes const& idx() const { return *p_idx_;}
    /** @return the vector with the values */
    inline Values const& val() const { return *p_val_;}

    // manipulator
    /** This method allows to get the jth value
     *  @param pos position of the element
     *  @return 0 if the element is not stored, the value of the element otherwise
     **/
    TypeConst elt(int pos) const;
    /** This method allows to overwrite or insert an element to the given position
     *  @param pos,value index and value to set
     **/
    void setValue(int pos, Type const& value);
    /** This method allows to write zero to the given position
     *  @param pos index
     **/
    void setZero(int pos);

    /** @brief main method for memory allocation.
     *  @note do nothing for sparse arrays.
     *  @param I range of the data allocated
     **/
    template<int OtherSize>
    MemSAllocator1D& malloc( TRange<OtherSize> const& I);
    /** @brief function for main ptr memory reallocation.
     *
     *  If the size requested is greater than the allocated size,
     *  the Type stored are saved and copied using the operator=. the Type
     *  class have to provide this operator.
     *
     *  If the size requested is lesser than the allocated size, only
     *  the first elements fitting in the container are copied.
     *  @param I range of the data to reserve
     **/
    template<int OtherSize>
    MemSAllocator1D& realloc( TRange<OtherSize> const& I);
    /** function releasing all stored values. */
    void free();
    /** function copying a part of allocator T in this.
     *  @param pos position where will be copied data
     *  @param T,range the array of data and the range of the data to copy
     *  @note only which are not zero are copied
     **/
    template<int OtherSize_, int RangeSize_>
    MemSAllocator1D& memcpy(int pos, MemSAllocator1D<Type, OtherSize_> const& T, TRange<RangeSize_> const& range);
    /** function moving a part of the allocator.
     *  @param pos,range position and range in form [begin,end) to move
     **/
    void memmove(int pos, Range const& range);

    /** exchange this with T.
     *  @param T the container to exchange with T
     **/
    MemSAllocator1D& exchange(MemSAllocator1D &T);
    /** @brief copy the Allocator T by value.
     *  The memory is free and the Allocator T is physically copied in this.
     *  @param T the allocator to copy by value
     *  @return a copy of T
     **/
    MemSAllocator1D& assign( MemSAllocator1D const& T);
    /** @brief assign a value to allocator.
     *  @param I,value range and value to assign to the allocator
     *  @return this
     *  @note allocator is no more sparse !
     **/
    template<int OtherSize_>
    MemSAllocator1D& assign(TRange<OtherSize_> const& I, Type const& value);

    /** @brief move the Allocator T to this.
     *  The memory of this is freed and T becomes a reference of this. This
     *  method allow to move the data of T to this without using physical copy.
     *
     *  @param T the allocator to move to this
     *  @return this object.
     *  @note the data member ref_ is mutable so that T can be passed as a
     *  constant reference.
     **/
    MemSAllocator1D& move( MemSAllocator1D const& T);
    /** shift the first index of the data to first.
     *  @param first the index of the first data to set
     **/
    MemSAllocator1D& shift(int first);

  protected:
    /** array of indexes */
    Indexes* p_idx_;
    /** array of values */
    Values* p_val_;

  private:
    /** Range of the data */
    AllocatorRange range_;
    /** zero value */
    const Type zero_;
    /** Current indexes used for internal lookup */
    mutable int idx1_, idx2_;

    /** Write a value at the given position. If p_idx_->elt(pos) != idx
     *  value is inserted at this position, otherwise exiting value is
     *  overwritten.
     *  @param pos,value position and value to write
     **/
    void writeValue( int pos, Type_ const& value);
    /** Write a value at position idx1_. If p_idx_->elt(pos) != idx1_
     *  value is inserted at this position, otherwise exiting value is
     *  overwritten.
     *  @param pos,value position and value to write
     **/
    void setValue1( int pos, TypeConst value);

    /** find first position in p_idx_ less or equal to pos
     *  @param pos position to find
     **/
    inline void findIdx1(int pos) const
    { for ( idx1_ = beginIdx(); p_idx_->elt(idx1_+1) <= pos; ++idx1_) {} }
    /** find first position in p_idx_ less or equal to pos
     *  @param pos position to find
     **/
    inline void findIdx1Next(int pos) const
    { if (p_idx_->elt(idx1_) == Arithmetic<int>::max()) return;
      for ( ; p_idx_->elt(idx1_+1) <= pos; ++idx1_) {}
    }
    /** find idx1_ previous position in p_idx_ greater than pos
     *  @param pos position to find
     **/
    inline void findIdx1Prev(int pos) const
    { for ( ; p_idx_->elt(idx1_) >= pos; --idx1_) {}}

    /** find first position in p_idx_ less or equal to pos
     *  @param pos position to find
     **/
    inline void findIdx2(int pos) const
    { for ( idx2_ = beginIdx(); p_idx_->elt(idx2_+1) <= pos; ++idx2_) {} }
    /** find idx2_ next position in p_idx_ less or equal to pos
     *  @param pos position to find
     **/
    inline void findIdx2Next(int pos) const
    { if (p_idx_->elt(idx2_) == Arithmetic<int>::max()) return;
      for ( ; p_idx_->elt(idx2_+1) <= pos; ++idx2_) {}
    }
    /** find idx2_ previous position in p_idx_ greater than pos
     *  @param pos position to find
     **/
    inline void findIdx2Prev(int pos) const
    { for ( ; p_idx_->elt(idx2_) >= pos; --idx2_) {}}

    /** Set zero at position idx1_
     *  @param pos position of the zero to set
     **/
    void setZero1(int pos);
    /** Set value idx2_ at position idx1_ when idx1 < idx2
     *  @param pos position to overwrite
     **/
    void setValue1(int pos);
    /** Set value idx2_ at position idx1_ when idx1 > idx2
     *  @param pos position to overwrite
     **/
    void setValue2(int pos);
};

/* default constructor */
template< typename Type_, int NzMax_>
MemSAllocator1D< Type_, NzMax_>::MemSAllocator1D(): IContainerRef(false)
                                                  , p_idx_(new Indexes(2))
                                                  , p_val_(new Values(2))
                                                  , range_()
                                                  , zero_(0)
{
  p_idx_->setValue(Arithmetic<int>::max());
  p_idx_->front() = -Arithmetic<int>::max();
  p_val_->setValue(Arithmetic<Type>::NA());
}

/* constructor with specified dimension
 *  @param I range of the rows
 **/
template< typename Type_, int NzMax_>
MemSAllocator1D< Type_, NzMax_>::MemSAllocator1D( AllocatorRange const& I)
                                               : IContainerRef(false)
                                               , p_idx_(new Indexes(2))
                                               , p_val_(new Values(2))
                                               , range_(I)
                                               , zero_(0)
{
  p_idx_->setValue(Arithmetic<int>::max());
  p_idx_->front() = -Arithmetic<int>::max();
  p_val_->setValue(Arithmetic<Type>::NA());
}

/* constructor with specified dimensions
 * @param I range of the rows
 * @param nzmax maximal number of data
 **/
template< typename Type_, int NzMax_>
MemSAllocator1D< Type_, NzMax_>::MemSAllocator1D( AllocatorRange const& I, int nzmax)
                                               : IContainerRef(false)
                                               , p_idx_( new Indexes(2) )
                                               , p_val_( new Values(2) )
                                               , range_(I)
                                               , zero_(0)
{
  p_idx_->reserve(nzmax);
  p_val_->reserve(nzmax);
  p_idx_->setValue(Arithmetic<int>::max());
  p_idx_->front() = -Arithmetic<int>::max();
  p_val_->setValue(Arithmetic<Type>::NA());
}

/* copy constructor
 *  @param A allocator to copy
 *  @param ref @c true if this copy is just a reference, @c false otherwise
 **/
template< typename Type_, int NzMax_>
MemSAllocator1D< Type_, NzMax_>::MemSAllocator1D( MemSAllocator1D const& A, bool ref)
                                               : IContainerRef(ref)
                                               , p_idx_(ref ? A.p_idx_ : new Indexes(A.idx()))
                                               , p_val_(ref ? A.p_val_ : new Values(A.val()))
                                               , range_(A.range_)
                                               , zero_(A.zero_)
{}
/* Copy constructor.
 *  @param A allocator to copy
 **/
template< typename Type_, int NzMax_>
template< int OtherNzMax_>
MemSAllocator1D< Type_, NzMax_>::MemSAllocator1D( MemSAllocator1D<Type, OtherNzMax_> const& A)
                                               : IContainerRef(false)
                                               , p_idx_(new Indexes(A.idx()))
                                               , p_val_(new Values(A.val()))
                                               , range_(A.range_)
                                               , zero_(A.zero_)
{}
/* Copy constructor. Map a range of A.
 *  @param A,I the allocator and range to reference
 *  @param ref is this a wrapper of A ? (true by default)
 **/
template< typename Type_, int NzMax_>
template<int OtherNzMax_>
MemSAllocator1D< Type_, NzMax_>::MemSAllocator1D( MemSAllocator1D<Type, OtherNzMax_> const& A
                                                , AllocatorRange const& I
                                                , bool ref
                                                )
                                               : IContainerRef(ref)
                                               , p_idx_(ref ? A.p_idx_ : new Indexes(A.idx()))
                                               , p_val_(ref ? A.p_val_ : new Values(A.val()))
                                               , range_(I)
                                               , zero_(A.zero_)
{}

/* destructor. */
template<typename Type_, int NzMax_>
MemSAllocator1D< Type_, NzMax_>::~MemSAllocator1D()
{
  if (!isRef())
  {
    delete p_idx_;
    delete p_val_;
  }
}

/* This method allows to get the element (p_idx, s_idx)
 *  @param p_idx the index of the row (or column)
 *  @param s_idx the index of the column (or row)
 *  @return 0 if the element is not stored, the value of the element otherwise
 **/
template< typename Type_, int NzMax_>
typename MemSAllocator1D<Type_, NzMax_>::TypeConst
MemSAllocator1D<Type_, NzMax_>::elt( int pos) const
{
#ifdef STK_BOUNDS_CHECK
  if (pos < begin())
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::elt,pos,MemSAllocator1D::begin() > pos);}
  if (pos >= end())
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::elt,pos,MemSAllocator1D::end() <= pos);}
#endif
  findIdx1(pos);
  return (p_idx_->elt(idx1_) == pos) ? p_val_->elt(idx1_) : zero_;
}

/* This method allows to overwrite or insert an element to the position (p_idx,  s_idx)
 *  @param p_idx index of the row (respectively column)
 *  @param s_idx index of the column (respectively row)
 *  @param value value to set
 **/
template< typename Type_, int NzMax_>
void MemSAllocator1D<Type_, NzMax_>::setValue( int pos, Type const& value)
{
#ifdef STK_BOUNDS_CHECK
  if (pos < begin())
  { STKOUT_OF_RANGE_2ARG(MemSAllocator1D::setValue,pos, value,MemSAllocator1D::begin() > pos);}
  if (pos >= end())
  { STKOUT_OF_RANGE_2ARG(MemSAllocator1D::setValue,pos, value,MemSAllocator1D::end() <= pos);}
#endif
  if (value == zero_) { setZero(pos);}
  else                { writeValue(pos, value);}
}

template< typename Type_, int NzMax_>
void MemSAllocator1D<Type_, NzMax_>::setZero( int pos)
{
#ifdef STK_BOUNDS_CHECK
  if (pos < begin())
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::setZero,pos,MemSAllocator1D::begin() > pos);}
  if (pos >= end())
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::setZero,pos,MemSAllocator1D::end() <= pos);}
#endif
  findIdx1(pos);
  if (p_idx_->elt(idx1_) == pos)
  {
    p_idx_->erase(idx1_);
    p_val_->erase(idx1_);
  }
}

/* @brief main method for memory allocation.
 *  @note do nothing for sparse arrays.
 *  @param I range of the data allocated
 **/
template<typename Type_, int NzMax_>
template<int OtherSize>
MemSAllocator1D<Type_, NzMax_>& STK::MemSAllocator1D<Type_, NzMax_>::malloc( TRange<OtherSize> const& I)
{
  // there is no necessity to allocate if range_ is the same
  if (range_ == I) return *this;
  if (isRef()) { STKRUNTIME_ERROR_1ARG(MemSAllocator1D::malloc,I,cannot operate on references);}
  p_idx_->resize( Range(I.begin(),2) );
  p_val_->resize( Range(I.begin(),2) );
  p_idx_->setValue(Arithmetic<int>::max());
  p_idx_->front() = -Arithmetic<int>::max();
  p_val_->setValue(Arithmetic<Type>::NA());
  range_ = I;
  return *this;
}
/* @brief function for main ptr memory reallocation.
 *
 *  If the size requested is greater than the allocated size,
 *  the Type stored are saved and copied using the operator=. the Type
 *  class have to provide this operator.
 *
 *  If the size requested is lesser than the allocated size, only
 *  the first elements fitting in the container are copied.
 *  @param I range of the data to reserve
 **/
template<typename Type_, int NzMax_>
template<int OtherSize>
MemSAllocator1D<Type_, NzMax_>& STK::MemSAllocator1D<Type_, NzMax_>::realloc( TRange<OtherSize> const& I)
{
  // there is no necessity to allocate if range_ is the same
  if (range_ == I) return *this;
  if (isRef()) { STKRUNTIME_ERROR_1ARG(MemSAllocator1D::realloc,I,cannot operate on references);}
  p_idx_->resize(I);
  p_val_->resize(I);
  range_ = I;
  return this;
}
/* function releasing all stored values. */
template<typename Type_, int NzMax_>
void STK::MemSAllocator1D<Type_, NzMax_>::free()
{
  p_idx_->clear();
  p_val_->clear();
  range_ = AllocatorRange();
  return;
}

/* function copying a part of allocator T in this.
 *  @param pos position where will be copied data
 *  @param T,range the array of data and the range of data to copy
 *  @note only which are not zero are copied
 **/
template<typename Type_, int NzMax_>
template<int OtherSize_, int RangeSize_>
MemSAllocator1D<Type_, NzMax_>& STK::MemSAllocator1D<Type_, NzMax_>::memcpy(int pos, MemSAllocator1D<Type, OtherSize_> const& T, TRange<RangeSize_> const& range)
{
  if (range.size() <= 0) return *this;
#ifdef STK_BOUNDS_CHECK
  if (pos < begin()) { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::memcpy,pos,begin() > pos);}
  if (pos >= end())  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::memcpy,pos,end() <= pos);}
  if (!T.range().isContaining(range))
  { STKOUT_OF_RANGE_1ARG(MemSAllocator::memcopy,range,range not in T.range());}
#endif
  for (int k=range.begin(); k<range.end(); ++k, ++pos)
  { setValue(pos, T.elt(k));}
  return *this;
}

/* function moving a part of the allocator.
 *  @param pos,range position and range in form [begin,end) to move
 **/
template<typename Type_, int NzMax_>
void STK::MemSAllocator1D<Type_, NzMax_>::memmove(int pos, Range const& range)
{
  if ((range.size() <= 0)||(range.begin() == pos)) return;
#ifdef STK_BOUNDS_CHECK
  if (pos < begin())
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::memmove,pos,begin() > pos);}
  if (pos >= end())
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::memmove,pos,end() <= pos);}
  if (!range_.isContaining(range))
  { STKOUT_OF_RANGE_1ARG(MemSAllocator1D::memmove,range,range not in range_);}
#endif
  if (pos < range.begin()) // ==> idx1 < idx2
  {
    // initialize idx1_ and idx2_
    findIdx1(pos);
    findIdx2(range.begin());
    // start loop over range
    for (int k= range.begin(); k<range.end(); ++k, ++pos)
    {
      // update idx1_ and idx2 (do nothing at the beginning)
      findIdx1Next(pos);
      findIdx2Next(k);
      if (p_idx_->elt(idx2_) != k)  { setZero1(pos);}
      else                          { setValue1(pos);}
    }
  }
  else // ==> idx2 < idx1
  {
    // initialize idx1_ and idx2_
    findIdx2(range.lastIdx());
    pos += range.size();
    findIdx1(pos);
    for (int k= range.lastIdx(); k>=range.begin(); --k, --pos)
    {
      // update idx1_ and idx2 (do nothing at the beginning)
      findIdx1Prev(pos);
      findIdx2Prev(k);
      if (p_idx_->elt(idx2_) != k) { setZero1(pos);}
      else                         { setValue2(pos);}
    }
  }
}

/* exchange this with T.
 *  @param T the container to exchange with T
 **/
template<typename Type_, int NzMax_>
MemSAllocator1D<Type_, NzMax_>& STK::MemSAllocator1D<Type_, NzMax_>::exchange(MemSAllocator1D& T)
{
  std::swap(p_idx_, T.p_idx_);
  std::swap(p_val_, T.p_val_);
  std::swap(range_, T.range_);
  IContainerRef::exchange(T);
  return *this;
}

/* @brief copy the Allocator T by value.
 *  The memory is free and the Allocator T is physically copied in this.
 *  @param T the allocator to copy by value
 *  @return a copy of T
 **/
template<typename Type_, int NzMax_>
MemSAllocator1D<Type_, NzMax_>&
                STK::MemSAllocator1D<Type_, NzMax_>::assign(const MemSAllocator1D& T)
{
  *p_idx_ = T.idx();
  *p_val_ = T.val();
  range_  = T.range();
  return *this;
}
/* @brief set a value.*/
template<typename Type, int NzMax_>
template<int OtherSize_>
MemSAllocator1D<Type, NzMax_>&
     MemSAllocator1D<Type, NzMax_>::assign(TRange<OtherSize_> const& I, Type const& value)
{
  // reserve first if not zero_ using upper-bound current size + range size
  findIdx1(I.begin());
  if (value!=zero_)
  {
    p_idx_->reserve(I.size()+p_idx_->size());
    p_val_->reserve(I.size()+p_val_->size());
    for (int pos= I.begin(); pos < I.end(); ++pos)
    {
      findIdx1Next(pos);
      setValue1(pos, value);
    }
  }
  else
  {
    for (int pos= I.begin(); pos < I.end(); ++pos)
    {
      findIdx1Next(pos);
      setZero1(pos);
    }
  }
  return *this;
}

/* @brief move the Allocator T to this.
 *  The memory of this is freed and T becomes a reference of this. This
 *  method allow to move the data of T to this without using physical copy.
 *
 *  @param T the allocator to move to this
 *  @return this object.
 *  @note the data member ref_ is mutable so that T can be passed as a
 *  constant reference.
 **/
template<typename Type_, int NzMax_>
MemSAllocator1D<Type_, NzMax_>& STK::MemSAllocator1D<Type_, NzMax_>::move(const MemSAllocator1D& T)
{
  p_idx_->move(T.p_idx_);
  p_val_->move(T.p_val_);
  range_ = T.range_;
  setRef(T.ref());
  return *this;
}

/* shift the first index of the data to first.
 *  @param first the index of the first data to set
 **/
template<typename Type_, int NzMax_>
MemSAllocator1D<Type_, NzMax_>& STK::MemSAllocator1D<Type_, NzMax_>::shift(int first)
{
  int inc = first - begin();
  for (int t=p_idx_->begin()+1; p_idx_->elt(t) != Arithmetic<int>::max(); ++t)
  { p_idx_->elt(t) += inc;}
  range_.shift(first);
  return *this;
}

/* write a value at the given position. If p_idx_->elt(pos) != idx
 *  value is inserted at this position, otherwise exiting value is
 *  overwritten.
 *  @param pos position in p_idx_
 *  @param value value to write
 **/
template<typename Type_, int NzMax_>
void STK::MemSAllocator1D<Type_, NzMax_>::writeValue( int pos, Type_ const& value)
{
  findIdx1(pos);
  setValue1(pos, value);
}
/* write a value at the given position. If p_idx_->elt(pos) != idx
 *  value is inserted at this position, otherwise exiting value is
 *  overwritten.
 *  @param pos position in p_idx_
 *  @param value value to write
 **/
template<typename Type_, int NzMax_>
void STK::MemSAllocator1D<Type_, NzMax_>::setValue1( int pos, TypeConst value)
{
  // there is an existing stored value for this entry
  if (p_idx_->elt(idx1_) == pos) { p_val_->elt(idx1_) =value;}
  else // value is not yet an entry, so add it
  {
    // Otherwise insert it
    p_idx_->insert(idx1_+1, pos);
    p_val_->insert(idx1_+1, value);
  }
}


template< typename Type_, int NzMax_>
void MemSAllocator1D<Type_, NzMax_>::setZero1(int pos)
{
  // there is an existing stored value for this entry
  if (p_idx_->elt(idx1_) == pos)
  {
    p_idx_->erase(idx1_);
    p_val_->erase(idx1_);
  }
}

/* write a value at the given position. If p_idx_->elt(pos) != idx
 *  value is inserted at this position, otherwise exiting value is
 *  overwritten.
 *  @param pos position in p_idx_
 **/
template<typename Type_, int NzMax_>
void STK::MemSAllocator1D<Type_, NzMax_>::setValue1(int pos)
{
  // there is an existing stored value for this entry
  if (p_idx_->elt(idx1_) == pos)
  { p_val_->elt(idx1_) = p_val_->elt(idx2_);}
  else // value is not yet an entry, so add it
  {
    idx1_++;
    p_idx_->insert(idx1_, pos);
    p_val_->insertElt(idx1_, 1);
    p_val_->elt(idx1_) = p_val_->elt(idx2_+1); // take care that elt1 and elt2 are shifted
  }
}
/* write a value at the given position. If p_idx_->elt(pos) != idx
 *  value is inserted at this position, otherwise exiting value is
 *  overwritten.
 *  @param pos position in p_idx_
 **/
template<typename Type_, int NzMax_>
void STK::MemSAllocator1D<Type_, NzMax_>::setValue2(int pos)
{
  // there is an existing stored value for this entry
  if (p_idx_->elt(idx1_) == pos)
  { p_val_->elt(idx1_) = p_val_->elt(idx2_);}
  else // value is not yet an entry, so add it
  {
    idx1_++;
    p_idx_->insert(idx1_, pos);
    p_val_->insertElt(idx1_, 1);
    p_val_->elt(idx1_) = p_val_->elt(idx2_);

  }
}


} // namespace STK


#endif /* STK_MEMSALLOCATOR1D_H */
