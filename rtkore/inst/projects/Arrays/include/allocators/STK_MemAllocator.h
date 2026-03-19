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
 * Purpose:  Define the Base Interface for the Array classes.
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 *
 **/

/** @file STK_MemAllocator.h
  * @brief In this file we define the class MemAllocator
 **/

#ifndef STK_MEMALLOCATOR_H
#define STK_MEMALLOCATOR_H

#include <cstring>
#include "../STK_IContainerRef.h"
#include <Sdk/include/STK_Arithmetic.h>
#include <Sdk/include/STK_IdTypeImpl.h>
#include <STKernel/include/STK_String.h>
#include <Sdk/include/STK_Exceptions.h>
#include <STKernel/include/STK_Range.h>
#include <Sdk/include/STK_Macros.h>

namespace STK
{

namespace hidden
{

/** @ingroup hidden
 *  Helper class allowing to free memory (or not)
 *  @note for fixed size containers, free do nothing, realloc adjust the shift
 **/
template<typename Type_, int Size_> struct MemHandler
{
  typedef TRange<Size_> AllocatorRange;
  /** do nothing for fixed size allocators ! */
  static Type_* free(Type_* p_data, AllocatorRange const& range) { return p_data;}
  /** malloc memory. allocate memory is not allocated yet, otherwise
   *  do nothing.
   **/
  static Type_* malloc(Type_* p_data, AllocatorRange const& range)
  {
    if (!p_data)
    {
      if(range.size()>0)
      {
        p_data = new Type_[range.size()];
        p_data -= range.begin();
      }
    }
    return p_data;
  }
  /** realloc main pointer and copy existing data. */
  template<int OtherSize>
  static Type_* realloc( Type_* p_data, AllocatorRange const& range, TRange<OtherSize> const& I)
  {
    // fixed size, just shift
    return p_data + range.begin() - I.begin();
  }
};

/** @ingroup hidden
 *  Specialization for dynamic size arrays.
 * */
template<class Type_> struct MemHandler<Type_, UnknownSize>
{
  typedef TRange<UnknownSize> AllocatorRange;
  /** shift data pointer and delete allocated memory */
  static Type_* free(Type_* p_data, AllocatorRange const& range)
  {
    // if there is elts
    if (p_data)
    {
      p_data+=range.begin();
      delete [] p_data; // erase
      p_data = 0;
    }
    return p_data;
  }
  /** p_data should not point on existing data. Just set default value. */
  static Type_* malloc(Type_* p_data, AllocatorRange const& range)
  {
    if(range.size()>0)
    {
      p_data = new Type_[range.size()];
      p_data -= range.begin();
    }
    else
    { p_data = 0;}
    return p_data;
  }
  /** realloc main pointer and copy existing data. */
  template<int OtherSize>
  static Type_* realloc( Type_* p_data, AllocatorRange const& range, TRange<OtherSize> const& I)
  {
    Type_* p = 0;
    p = malloc(p, I);
    Range r = inf(range, I);
    for (int i = r.begin(); i<r.end(); ++i) { p[i] = p_data[i];}
    p_data = free(p_data, range);
    return p;
  }
};

/** @ingroup hidden Helper class allowing to use std::memcpy or std::memmove for
 *  fundamental types
 **/
template<int, class Type_> struct MemChooser;

/** @ingroup hidden
 *  Specialization for fundamental types.
 *  copy or move memory using standard C copy and move functions
 * */
template<class Type_> struct MemChooser<1, Type_>
{
  static Type_* memcpy(Type_* p, Type_* q, size_t size)
  { return static_cast<Type_*>(std::memcpy( p , q, sizeof(Type_)*size));}

  static Type_* memmove(Type_* p, Type_* q, size_t size)
  { return static_cast<Type_*>(std::memmove( p, q, sizeof(Type_)*size));}
};

/** @ingroup hidden
 *  Specialization for other types using loop and operator= */
template<class Type_> struct MemChooser<0, Type_>
{
  static Type_* memcpy(Type_* p, Type_* q, size_t size)
  {
    for (size_t k=0; k<size; k++) { p[k] = q[k];}
    return p;
  }
  static Type_* memmove(Type_* p, Type_* q, size_t size)
  {
    if (size == 0) return p;
    if (p<q) { for (size_t k=0; k<size; k++)    { p[k] = q[k];}}
    else     { for (size_t k=size-1; k==0; --k) { p[k] = q[k];}}
    return p;
  }
};

} // namespace hidden

/** @ingroup Arrays
 *  @brief template base class for all Allocator classes.
 *
 *  The MemAllocator class is the base class of all memory allocators. This
 *  class manages the main pointer on the data. It derives from the IContainerRef
 *  class as an array stored in memory can always be wrapped in some way or be
 *  a wrapper of some data stored in memory.
 *
 *  @tparam Type_ can be any type of data that can be stored in memory.
 *  @tparam Size_ size of the data if it is known at compile time
 **/
template<typename Type_, int Size_>
struct MemAllocator: public IContainerRef
{
  enum { isNumeric_ = hidden::IsArithmetic<Type_>::yes};
  typedef hidden::MemHandler<Type_, Size_> MemHandler;
  typedef hidden::MemChooser<isNumeric_, Type_> MemChooser;

  typedef Type_  Type;
  typedef typename hidden::RemoveConst<Type>::Type const& TypeConst;

  typedef TRange<Size_> AllocatorRange;
  using IContainerRef::isRef;
  using IContainerRef::setRef;

  /** Default constructor. */
  MemAllocator();
  /** constructor with specified Range
   *  @param I range of the data
   **/
  MemAllocator( AllocatorRange const& I);
  /** constructor with specified Range and initial value
   *  @param I,value range and value of the data
   **/
  MemAllocator( AllocatorRange const& I, Type const& value);
  /** Copy constructor.
   *  @param T the array to copy or reference
   *  @param ref is this a wrapper of T ? (@c false by default)
   **/
  MemAllocator( MemAllocator const& T, bool ref = false);
  /** Copy constructor. Copy data or create a reference.
   *  @param T the array to copy or reference
   *  @param ref is this a wrapper of T ? (false by default)
   **/
  template<int OtherSize_>
  MemAllocator( MemAllocator<Type, OtherSize_> const& T, bool ref = false);
  /** Copy constructor. Map a range of T.
   *  @param T,I the allocator and range to reference
   *  @param ref is this a wrapper of a part of T ? (@c true by default)
   **/
  template<int OtherSize_>
  MemAllocator( MemAllocator<Type, OtherSize_> const& T,  AllocatorRange const& I, bool ref = true);
  /** @brief Wrapper or copy constructor.
   *  @param q,I ptr and range of the data to wrap
   *  @param ref is this a wrapper ? If @c true data will not be freed when this
   *  object is released
   **/
  MemAllocator( Type* const& q, Range const& I, bool ref);
  /** Wrapper or copy constructor: second form. This constructor assumes the
   *  data as a C-like array. Thus first index is 0.
   *  @param q,size ptr and size of the data to wrap
   *  @param ref is this a wrapper ? If @c true data will not be freed when this
   *  object is released
   **/
  MemAllocator( Type* const& q, int size, bool ref);
  /** destructor. Release memory.*/
  ~MemAllocator();

  /** @return the range of the data*/
  inline AllocatorRange const& range() const { return range_;}
  /** @return the first index of the data. */
  inline int begin() const { return range_.begin();}
  /**@return the ending index of the data */
  inline int end() const { return range_.end();}
  /** @return the size of the data */
  inline int size() const { return range_.size();}

  /** Get constant pointer on data */
  inline Type* const& p_data() const { return p_data_;}

  /** This method allows to set a value to the allocator
   *  @param value value to set
   **/
  void setValue(TypeConst value)
  {
    for(int pos=begin(); pos < end(); ++pos)
    { p_data_[pos] = value;}
  }
  /** This method allows to set a value to the position @c pos
   *  @param pos,value index and value to set
   **/
  inline void setValue(int pos, TypeConst value)
  {
#ifdef STK_BOUNDS_CHECK
    if (pos < begin())
    { STKOUT_OF_RANGE_1ARG(MemAllocator::elt,pos,MemAllocator::begin() > pos);}
    if (pos >= end())
    { STKOUT_OF_RANGE_1ARG(MemAllocator::elt,pos,MemAllocator::end() <= pos);}
#endif
    p_data_[pos] = value;
  }

  /** Get the const element number pos.
   *  @param pos the position of the element we get
   **/
  inline TypeConst elt( int pos) const
  {
#ifdef STK_BOUNDS_CHECK
    if (pos < begin())
    { STKOUT_OF_RANGE_1ARG(MemAllocator::elt,pos,MemAllocator::begin() > pos);}
    if (pos >= end())
    { STKOUT_OF_RANGE_1ARG(MemAllocator::elt,pos,MemAllocator::end() <= pos);}
#endif
    return p_data_[pos];
  }
  /** @return a constant reference on the ith  element
   *  @param i index of the element to get
   **/
  inline TypeConst operator[](int i) const { return elt(i);}
  /** Get the element number pos.
   *  @param pos the position of the element we get
   **/
  inline Type& elt(int pos) { return p_data_[pos];}
  /** @return ith element
   *  @param i index of the element to get
   **/
  inline Type& operator[](int i) { return elt(i);}
  /** swap two elements of the Allocator.
   *  @param pos1, pos2 the positions to swap
   **/
  inline void swap(int pos1, int pos2)
  {
#ifdef STK_BOUNDS_CHECK
    if (begin() > pos1) { STKOUT_OF_RANGE_2ARG(MemAllocator::swap, pos1, pos2, begin() > pos1);}
    if (begin() > pos2) { STKOUT_OF_RANGE_2ARG(MemAllocator::swap, pos1, pos2, begin() > pos2);}
    if (end() <= pos1)  { STKOUT_OF_RANGE_2ARG(MemAllocator::swap[], pos1, pos2, end() <= pos1);}
    if (end() <= pos2)  { STKOUT_OF_RANGE_2ARG(MemAllocator::swap[], pos1, pos2, end() <= pos2);}
#endif
    std::swap(p_data_[pos1], p_data_[pos2]);
  }

  /** @brief main method for memory allocation.
   *  @param I range of the data allocated
   **/
  template<int OtherSize>
  void malloc( TRange<OtherSize> const& I);
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
  void realloc( TRange<OtherSize> const& I);
  /** function for main ptr memory deallocation. */
  void free();

  /** function copying a part of allocator T in this.
   *  @param pos position where will be copied data
   *  @param T,range the array of data and the range of the data to copy  */
  template<int OtherSize_, int RangeSize_>
  void memcpy(int pos, MemAllocator<Type, OtherSize_> const& T, TRange<RangeSize_> const& range);
  /** function moving a part of the allocator.
   *  @param pos,range range in form [begin,end) to move at position pos
   **/
  template< int RangeSize_>
  void memmove(int pos, TRange<RangeSize_> const& range);
  /** function moving a part of the allocator.
   *  @param pos,begin,size range in form [begin,begin+size) to move at position pos
   **/
  void memmove(int pos, int begin, int size);

  /** exchange this with T.
   *  @param T the container to exchange with T
   **/
  void exchange(MemAllocator &T);
  /** @brief copy the Allocator T by value.
   *  The memory is free and the Allocator T is physically copied in this.
   *  @param T the allocator to copy by value
   *  @return a copy of T
   **/
  MemAllocator& assign( MemAllocator const& T);
  /** @brief assign a value to allocator.
   *  @param I,value range and value to assign
   *  @return a copy
   **/
  template<int OtherSize_>
  MemAllocator& assign(TRange<OtherSize_> const& I, Type const& value);

  /** @brief move the Allocator T to this.
   *  The memory of this is freed and T becomes a reference of this. This
   *  method allow to move the data of T to this without using physical copy.
   *
   *  @param T the allocator to move to this
   *  @return this object.
   *  @note the data member ref_ is mutable so that T can be passed as a
   *  constant reference.
   **/
  MemAllocator& move( MemAllocator const& T);
  /** shift the first index of the data to first.
   *  @param first the index of the first data to set
   **/
  MemAllocator& shift(int first);
  /** @brief Set address and range of allocated data.
   *  This method is to be used when the memory have been allocated outside.
   *  If allocator wrap allocated memory, it is not freed.
   *  @param p_data the address to set
   *  @param range range of the data
   *  @param ref is p_data_ a wrapper ?
   **/
  void setPtr( Type* p_data,  Range const& range, bool ref)
  { p_data_ = p_data; range_ = range; setRef(ref);}

  protected:
    /** Main pointer on the data. */
    Type* p_data_;

  private:
    /** Range of the data */
    AllocatorRange range_;
    /** move main pointer on data
     *  @param inc the increment to apply
     **/
    void shiftPtr( int inc)
    {
      if (p_data_) { p_data_ -= inc;}
      range_.inc(inc);
    }
    /** function for main ptr memory deallocation. Force memory deallocation, even
     *  for fixed size allocators.
     **/
    void forcedFree()
    {
      if(!isRef())
      { p_data_ = hidden::MemHandler<Type, UnknownSize>::free(p_data_, range_);}
    }
};

/* Default constructor. */
template<typename Type, int Size_>
MemAllocator<Type,Size_>::MemAllocator(): IContainerRef(false)
                                        , p_data_(0)
                                        , range_()
{ malloc(range_);}
/* constructor with specified Range
 *  @param I range of the data
 **/
template<typename Type, int Size_>
MemAllocator<Type,Size_>::MemAllocator( TRange<Size_> const& I)
                                      : IContainerRef(false), p_data_(0), range_(I)
{ malloc(I);}

/* constructor with specified Range and value
 *  @param I range of the data
 **/
template<typename Type, int Size_>
MemAllocator<Type,Size_>::MemAllocator( TRange<Size_> const& I, Type const& value)
                                      : IContainerRef(false), p_data_(0), range_(I)
{ malloc(I);
  assign(I, value);
}

/* Copy constructor. We don't know, how the user classes want to copy the
 *  data if this is not a reference.
 *  @param T the array to copy or reference
 *  @param ref is this a wrapper of T ?
 *  @note if ref is @c false, the derived class is responsible of the data copy
 **/
template<typename Type, int Size_>
MemAllocator<Type, Size_>::MemAllocator( MemAllocator const& T, bool ref)
                                       : IContainerRef(ref)
                                       , p_data_(ref ? T.p_data(): 0)
                                       , range_(T.range())
{
  if (!ref)
  {
    malloc(range());
    memcpy(begin(), T, range());
  }
}

/* Copy constructor. We don't know, how the user classes want to copy the
 *  data if this is not a reference.
 *  @param T the array to copy or reference
 *  @param ref is this a wrapper of T ?
 *  @note if ref is @c false, the derived class is responsible of the data copy
 **/
template<typename Type, int Size_>
template<int OtherSize_>
MemAllocator<Type, Size_>::MemAllocator( MemAllocator<Type, OtherSize_> const& T, bool ref)
                                       : IContainerRef(ref)
                                       , p_data_(ref ? T.p_data(): 0)
                                       , range_(T.range())
{
  if (!ref)
  {
    malloc(range());
    memcpy(begin(), T, range());
  }
}
/* constructor by reference.
 *  @param T,I the allocator and range to reference
 **/
template<typename Type, int Size_>
template<int OtherSize_>
MemAllocator<Type, Size_>::MemAllocator( MemAllocator<Type, OtherSize_> const& T, TRange<Size_> const& I, bool ref)
                                      : IContainerRef(true)
                                      , p_data_(ref ? T.p_data() : 0)
                                      , range_(I)
{
  if (!ref)
  {
    malloc(I);
    for(int i=I.begin(); i< I.end(); ++i)
    { p_data_[i] = T.elt(i);}
  }
}

/* @brief Wrapper or copy constructor.
 *  @param q,I ptr and range of the data to wrap
 *  @param ref is this a wrapper ? If @c true data will not be freed when this
 *  object is released
 **/
template<typename Type, int Size_>
MemAllocator<Type,Size_>::MemAllocator( Type* const& q, Range const& I, bool ref)
             : IContainerRef(ref), p_data_(q), range_(I)
{ /* derived class have to delete data if ref==false */}

/* Wrapper or copy constructor: second form. This constructor assumes the
 *  data as a C-like array. Thus first index is 0.
 *  @param q, size ptr and size of the data to wrap
 *  @param ref is this a wrapper ? If @c true data will not be freed when this
 *  object is released
 **/
template<typename Type, int Size_>
MemAllocator<Type,Size_>::MemAllocator( Type* const& q, int size, bool ref)
             : IContainerRef(ref), p_data_(q), range_(AllocatorRange(0,size))
{ /* derived class have to copy the data if ref==false */}

/* destructor. */
template<typename Type, int Size_>
MemAllocator<Type,Size_>::~MemAllocator()
{ forcedFree();}

/* exchange this with T.
 *  @param T the container to exchange with T
 **/
template<typename Type, int Size_>
void MemAllocator<Type,Size_>::exchange(MemAllocator<Type,Size_> &T)
{
  std::swap(p_data_, T.p_data_);
  std::swap(range_, T.range_);
  IContainerRef::exchange(T);
}
/* @brief copy the Allocator T by value.
 *  The memory is free and the Allocator T is physically copied in this.
 *  @param T the allocator to copy by value
 *  @return a copy of this
 **/
template<typename Type, int Size_>
MemAllocator<Type,Size_>& MemAllocator<Type,Size_>::assign( MemAllocator<Type,Size_> const& T)
{
  // allocate memory if necessary
  malloc(T.range_);
  memcpy(begin(), T, range());
  return *this;
}

/* @brief a value.*/
template<typename Type, int Size_>
template<int OtherSize_>
MemAllocator<Type,Size_>& MemAllocator<Type,Size_>::assign(TRange<OtherSize_> const& I, Type const& value)
{
  for (int pos= I.begin(); pos < I.end(); ++pos) { p_data_[pos] = value;}
  return *this;
}
/* @brief move the Allocator T to this.
 *  The memory is free and T become a reference of this. This method allow
 *  to steal the data of T without physical copy.
 *
 *  @return this object.
 *  @note the data member ref_ is mutable so that T can be passed as a
 *  constant reference.
 *  @param T the allocator to move to this
 **/
template<typename Type, int Size_>
MemAllocator<Type, Size_>& MemAllocator<Type,Size_>::move( MemAllocator<Type, Size_> const& T)
{
  if (this == &T) return *this;
  forcedFree();
  setPtr(T.p_data_, T.range_, T.isRef());
  T.setRef(true); // T become a reference of the data it own
  return *this;
}
template<typename Type, int Size_>
MemAllocator<Type,Size_>& MemAllocator<Type,Size_>::shift(int first)
{
  // check if there is something to do
  if (first == begin()) return *this;
  // check for reference
  if (isRef())
    STKRUNTIME_ERROR_1ARG(MemAllocator::shift,first,cannot operate on reference);
  // translate data
  shiftPtr(first - begin());
  return *this;
}

template<typename Type, int Size_>
template<int OtherSize>
void MemAllocator<Type,Size_>::malloc( TRange<OtherSize> const& I)
{
  // there is no necessity to allocate if data is already allocated, range_
  // is the same and the data is not owned by an other allocator
  if ((range_ == I)&&(p_data_)&&(!isRef())) return;
  // allocate memory
  try
  {
    // free any existing data and allocate it again (do nothing for fixed size allocators)
    p_data_ = MemHandler::free(p_data_, range_);
    p_data_ = MemHandler::malloc(p_data_, I);
    setPtr(p_data_, I, false);
  }
  catch (std::bad_alloc const& error)
  {
    setPtr(0, AllocatorRange(), false);
    STKRUNTIME_ERROR_1ARG(MemAllocator::malloc, I, memory allocation failed);
  }
}

template<typename Type, int Size_>
template<int OtherSize>
void MemAllocator<Type,Size_>::realloc( TRange<OtherSize> const& I)
{
  // there is no necessity to allocate if data is already allocated, range_
  // is the same and the data is not owned by an other allocator
  if ((range_ == I)&&(p_data_)&&(!isRef())) return;

  try
  {
    // allocate memory and copy data in the same range
    Type* p  = MemHandler::malloc(p_data_, I);
    // copy data in common range
    Range r = inf(range_, I);
    if (r.size()>0)
    { MemChooser::memcpy(p+r.begin(), p_data_+r.begin(), r.size());      }
//    for (int i = r.begin(); i<r.end(); ++i) { p[i] = p_data_[i];}
    p_data_ = MemHandler::free(p_data_, range_);
    setPtr(p, I, false);
  }
  catch (std::bad_alloc const& error)
  { STKRUNTIME_ERROR_1ARG(MemAllocator::realloc, I, memory allocation failed);}
}
/* function for main ptr memory deallocation. */
template<typename Type, int Size_>
void MemAllocator<Type,Size_>::free()
{
  // nothing to do for reference
  if (isRef()) return;
  p_data_ = MemHandler::free(p_data_, range_);
  // if there is no elements range_ is set to default
  if (!p_data_) { range_ = AllocatorRange();}
}

template<typename Type, int Size_>
template<int OtherSize_, int RangeSize_>
void MemAllocator<Type,Size_>::memcpy(int pos, MemAllocator<Type, OtherSize_> const& T, TRange<RangeSize_> const& range)
{
  if (range.size() <= 0) return;
#ifdef STK_BOUNDS_CHECK
  if (pos < begin()) { STKOUT_OF_RANGE_1ARG(MemAllocator::memcpy,pos,begin() > pos);}
  if (pos >= end())  { STKOUT_OF_RANGE_1ARG(MemAllocator::memcpy,pos,end() <= pos);}
  if (!T.range().isContaining(range))
  { STKOUT_OF_RANGE_1ARG(MemAllocator::memcopy,range,range not in T.range());}
#endif
  MemChooser::memcpy(p_data_+pos, T.p_data()+range.begin(), range.size());
}

template<typename Type, int Size_>
void MemAllocator<Type,Size_>::memmove(int pos, int start, int size)
{
  if ((size <= 0)||(start == pos)) return;
#ifdef STK_BOUNDS_CHECK
  if (pos < begin())
  { STKOUT_OF_RANGE_1ARG(MemAllocator::memmove,pos,MemAllocator::begin() > pos);}
  if (pos >= end())
  { STKOUT_OF_RANGE_1ARG(MemAllocator::memmove,pos,MemAllocator::end() <= pos);}
  if (!range_.isContaining(Range(start,size)))
  { STKOUT_OF_RANGE_2ARG(MemAllocator::memmove,start,size,Range(start,size) not in range_);}
#endif
  MemChooser::memmove( p_data_+pos, p_data_+ start, size);
}

template<typename Type, int Size_>
template< int RangeSize_>
void MemAllocator<Type,Size_>::memmove(int pos, TRange<RangeSize_> const& range)
{
  if ((range.size() <= 0)||(range.begin() == pos)) return;
#ifdef STK_BOUNDS_CHECK
  if (pos < begin())
  { STKOUT_OF_RANGE_1ARG(MemAllocator::memmove,pos,MemAllocator::begin() > pos);}
  if (pos >= end())
  { STKOUT_OF_RANGE_1ARG(MemAllocator::memmove,pos,MemAllocator::end() <= pos);}
  if (!range_.isContaining(range))
  { STKOUT_OF_RANGE_1ARG(MemAllocator::memmove,range,range not in range_);}
#endif
  MemChooser::memmove( p_data_+pos, p_data_+range.begin(), range.size());
}


} // namespace STK

#endif /* STK_MEMALLOCATOR_H */
