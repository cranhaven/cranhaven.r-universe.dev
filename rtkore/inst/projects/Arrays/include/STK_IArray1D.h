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
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_IArray1D.h
 *  @brief Interface base class for the Array1D, this is an internal header file,
 *  included by other Containers library headers.
 *
 *  You should not attempt to use it directly but rather used one of the
 *  derived class like Array1D, except if you want to create your own
 *  Container Class.
 **/

#ifndef STK_IARRAY1D_H
#define STK_IARRAY1D_H

#include <STKernel.h>
#include "STK_IContainerRef.h"
#include "STK_ArraysTraits.h"
#include "STK_Arrays_Util.h"
#include "STK_ITContainer1D.h"
#include "allocators/STK_MemAllocator.h"
#include "iterators/STK_BiDirectionalIterator.h"
#include "iterators/STK_DenseRandomIterator.h"

namespace STK
{

namespace Arrays
{
/** @ingroup Arrays
 *  @return n+m, where n is the first number such that m < 2^n.
 *  @param m the size of the container
 **/
inline int evalSizeCapacity(int m)
{
  int n = 0;
  for (int k=1; k <= m; k <<= 1) {n++;}
  return(m+n);
}

/** @ingroup Arrays
 *  @param I range of the container
 *  @tparam Size_ The size of the array. For fixed size return the range unmodified
 **/
template<int Size_>
inline TRange<Size_> evalRangeCapacity(TRange<Size_> const& I) { return I;}
/** @ingroup Arrays
 * Specialization for array with unknown size
 *  @param I the range of the container
 **/
template<>
inline Range evalRangeCapacity(Range const& I)
{
  int n = 0;
  for (int k=1; k <= I.size(); k <<= 1){ n++;}
  return Range(I.begin(),I.size() + n);
}

} // namespace Arrays

/** @ingroup Arrays
 *  @brief template one dimensional Array.
 *
 * An IArray1D is a template one column container implementing the interface
 * base class ITContainer1D.
 **/
template<class Derived>
class IArray1D: public ITContainer1D<Derived>
{
  public:

    typedef ITContainer1D< Derived > Base;
    typedef typename hidden::Traits<Derived>::Allocator Allocator;

    enum
    {
      structure_ = hidden::Traits< Derived >::structure_,
      orient_    = hidden::Traits< Derived >::orient_,
      sizeCols_  = hidden::Traits< Derived >::sizeCols_,
      sizeRows_  = hidden::Traits< Derived >::sizeRows_,
      size_      = hidden::Traits< Derived >::size_,
      storage_   = hidden::Traits< Derived >::storage_,
      isFixedSize_ = (size_ != UnknownSize)
    };

    typedef typename hidden::Traits< Derived >::Type Type;
    typedef typename hidden::Traits< Derived >::TypeConst TypeConst;

    typedef typename hidden::Traits< Derived >::RowRange RowRange;
    typedef typename hidden::Traits< Derived >::ColRange ColRange;

    typedef typename hidden::Traits< Derived >::Iterator Iterator;
    typedef typename hidden::Traits< Derived >::ConstIterator ConstIterator;
    typedef typename hidden::Traits< Derived >::ReverseIterator ReverseIterator;
    typedef typename hidden::Traits< Derived >::ConstReverseIterator ConstReverseIterator;

    using Base::range;
    using Base::begin;
    using Base::end;
    using Base::size;
    using Base::lastIdx;

    using Base::decLast;
    using Base::incLast;
    using Base::incRange;
    using Base::setRange;

    using Base::elt;

  protected:
    /** Default constructor. */
    IArray1D();
    /** constructor with a specified Range.
      *  @param I range of the container
     **/
    IArray1D( Range const& I);
    /** Misc constructor with first and last, initialization with a constant.
     *  @param I range of the container
     *  @param v initial value of the container
     **/
    IArray1D( Range const& I, Type const& v);
    /** Copy constructor
     *  @param T the container to copy
     *  @param ref true if T is wrapped
     **/
    IArray1D( IArray1D const& T, bool ref);
    /** Copy constructor
     *  @param T the container to copy
     *  @param ref true if T is wrapped
     **/
    template<class OtherDerived>
    IArray1D( IArray1D<OtherDerived> const& T, bool ref);
    /** copy constructor.
     *  @param T,I the container and the range of data to wrap
     *  @param ref @c true if T is wrapped (the default)
     **/
    template<class OtherDerived>
    IArray1D( IArray1D<OtherDerived> const& T, RowRange const& I, bool ref);
    /** Wrapper constructor
     *  @param A,I range and allocator to wrap
     *  @param ref @c true if A is wrapped
     **/
    IArray1D( Allocator const& A, Range const& I, bool ref);
    /** destructor: allocated memory is liberated by MemAllocator class.*/
    ~IArray1D();

  public:
    /** @return @c true if *this is reference container, @c false otherwise */
    inline bool isRef() const { return allocator_.isRef();}
    /** Modify the state of the container: this become a reference (if ref is
     *  @c true) or the owner of the data (if ref is @c false).
     *  @note To use with care in order to avoid memory leak
     *  @param ref : has top be false if this own its own data
     **/
    inline void setRef(bool ref) const { allocator_.setRef(ref);}

    /** @return a constant reference on the allocator */
    Allocator const& allocator() const { return allocator_;}

    /**  @return the range of the rows of the container */
    inline RowRange const& rows() const  { return range();}
     /** @return the index of the first element */
    inline int beginRows() const { return begin();}
    /**  @return the ending index of the elements */
    inline int endRows() const { return end();}
    /**  @return the size of the container */
    inline int sizeRows() const  { return size();}

    /** @return the Horizontal range (1 column) */
    inline ColRange cols() const { return ColRange(1);}
    /** @return the index of the first column */
    inline int beginCols() const { return baseIdx;}
    /**  @return the index of the ending column */
    inline int endCols() const  { return baseIdx+1;}
    /** @return the number of columns */
    inline int sizeCols() const  { return 1;};

    /**  @return the index of the last element */
    inline int lastIdxRows() const  { return this->lastIdx();}
    /**  @return the index of the last element */
    inline int lastIdxCols() const  { return baseIdx;}

    /** @return the maximum possible number of elements without reallocation*/
    inline int capacity() const { return allocator_.size();}

    /** access to an element
     *  @param pos index of the element
     *  @return a reference on the element to modify
     **/
    inline Type& elt1Impl(int pos) { return allocator_.elt(pos);}
    /** access to a constant element
     *  @param pos index of the const element
     *  @return a constant reference on the element
     **/
    inline TypeConst elt1Impl(int pos) const { return allocator_.elt(pos);}

    /** set a value to this container.
     *  @param value the value to set
     **/
    Derived& setValue(Type const& value);
    /** Write a value at a given position
     *  @param pos,value index of the element
     *  @return a reference on the element to modify
     **/
    inline void setValue(int pos, TypeConst value)
    { allocator_.setValue(pos, value);}

    /** New beginning index for the object.
     *  @param beg the index of the first column to set
     **/
    void shiftImpl(int beg = baseIdx);
    /**  Resize the container.
     * - call @c shift
     * - call @c pushBack if there will be more elements
     * - call @c popBack if three will be less elements
     * @param I the range to set to the Array1D
     **/
    Derived& resizeImpl(Range const& I);
    /** reserve internal memory for at least size elements.
     *  @param size number of elements to reserve
     **/
    void reserve(int size);
    /** Clear the object. Memory is liberated and the
     *  range of the Container is set to 0:-1 or 1:0 (@see baseIdx).
     **/
    void clear();
    /** move T to this.
     *  @note : T is not modified but just set as a reference of the data it was owner.
     *  @param T the container to move to this.
     **/
    void move(Derived const& T);
    /** Add n Elements to the end of the container.
     *  @param n number of elements to add
     **/
    Derived& pushBack( int n=1);
    /** Delete last elts of the container.
     *  @param n number of elts to delete
     **/
    Derived& popBack(int n = 1);
    /** Delete n elements at the pos index to the container.
     *  @param pos index where to delete elements
     *  @param n number of elements to delete (default 1)
     **/
    Derived& erase(int pos, int n=1);
    /** Insert n elements at the position pos of the container.
     *  @param pos,n index where to insert the @c n elements (default is 1)
     **/
    Derived& insertElt( int pos, int n =1);
    /** STL compatibility: Insert element @c v at position @c pos of the Array.
     *  @param pos position to insert elements
     *  @param v value to insert
     **/
    Derived& insert( int pos, Type const& v);
    /** STL compatibility: Insert element @c v in the range @c I of the Array.
     *  @param I range of the index where to insert elements
     *  @param v value to insert
     **/
    Derived& insert( Range const& I, Type const& v);
    /** STL compatibility: push front an element.
     *  @param v value to append
     **/
    Derived& push_front(Type const& v);
    /** STL compatibility: append an element v.
     *  @param v value to append
     **/
    Derived& push_back(Type const& v);
    /** Swapping the pos1 elt and the pos2 elt.
     *  @param pos1,pos2 positions of the elements to swap
     **/
    void swap(int pos1, int pos2);
    /** exchange this Container with T.
     *  @param T the Array to exchange with this
     **/
    void exchange(IArray1D &T);
    /** overwrite @c this with @c src.
     *  @note If the size match, @c this is not resized, and in this case,
     *  the method take care of the possibility of overlapping.
     *  @param src the container to assign
     **/
    Derived& assign( IArray1D const& src);

    /** Copy n elements from pos2 to pos1, guaranteeing correct behavior for overlapping.
     *  @param pos1,pos2 positions of the elements to move
     *  @param n number of elements to move
     **/
    void memmove(int pos1, int pos2, int n);

  protected:
    /** @return a reference on the allocator */
    Allocator& allocator() { return allocator_;}

    /** function for memory allocation and initialization.
     *  This method will free all allocated memory owned by this container before initialization.
     *  @param I range of the container
     **/
    void initialize(RowRange const& I);
    /** function for memory allocation.
     *  The range is not set in this method. If a bad_alloc occur, we set the
     *  range of the container to default before throwing it.
     *  @param I range of the container
     **/
    void allocate(RowRange const& I);
    /** Method for memory deallocation. Memory is liberated and the
     *  range of the Container is set to begin:begin-1.
     **/
    void freeMem();

  private:
    Allocator allocator_;
};

template<class Derived>
IArray1D<Derived>::IArray1D(): Base(), allocator_(Arrays::evalRangeCapacity(range())) {}

template<class Derived>
IArray1D<Derived>::IArray1D( Range const& I)
                           : Base(I), allocator_(Arrays::evalRangeCapacity(I)) {}

template<class Derived>
IArray1D<Derived>::IArray1D( Range const& I, Type const& v)
                           : Base(I)
                           , allocator_(Arrays::evalRangeCapacity(I))
{ allocator_.assign(I,v);}

/* Copy constructor
 *  @param T the container to copy
 *  @param ref
 **/
template<class Derived>
IArray1D<Derived>::IArray1D( IArray1D const& T, bool ref)
                           : Base(T.range())
                           , allocator_(T.allocator(), ref)
{}
/* Copy constructor
 *  @param T the container to copy
 *  @param ref
 **/
template<class Derived>
template<class OtherDerived>
IArray1D<Derived>::IArray1D( IArray1D<OtherDerived> const& T, bool ref)
                           : Base(T.range())
                           , allocator_(T.allocator(), ref)
{}

/* copy constructor.
 *  @param T,I the container and the range of data to wrap
 *  @param ref @c true if T is wrapped
 **/
template<class Derived>
template<class OtherDerived>
IArray1D<Derived>::IArray1D( IArray1D<OtherDerived> const& T, RowRange const& I, bool ref)
                           : Base(I)
                           , allocator_(T.allocator(), I, ref)
{}

/* Wrapper constructor
 *  @param A,I range and allocator to wrap
 *  @param ref @c true if A is wrapped
 **/
template<class Derived>
IArray1D<Derived>::IArray1D( Allocator const& A, Range const& I, bool ref)
                           : Base(I)
                           , allocator_(A, I, ref)
{}
/* destructor: allocated memory is liberated by MemAllocator class.*/
template<class Derived>
IArray1D<Derived>::~IArray1D(){}

/* implement shift */
template<class Derived>
void IArray1D<Derived>::shiftImpl(int beg)
{
  // compute increment
  int inc = beg - begin();
  if (inc == 0) return;
  if (isRef())  // is this structure just a pointer ?
  { STKRUNTIME_ERROR_1ARG(IArray1D::shiftImpl,beg,cannot operate on references);}
  // translate range and data
  incRange(inc);
  allocator_.shift(beg);
}

template<class Derived>
Derived& IArray1D<Derived>::resizeImpl(Range const& I)
{
  // check if there is something to do
  if ( range() == I) return this->asDerived();
  if (isRef()) { STKRUNTIME_ERROR_1ARG(IArray1D::resizeImpl,I,cannot operate on references);}
  // translate
  shiftImpl(I.begin());
  // compute number of elements to delete or add
  const int inc = I.end() - end();
  // adjust size of the container
  if (inc > 0) pushBack(inc);  // more elements
  else         popBack(-inc);  // less elements
  return this->asDerived();
}

template<class Derived>
void IArray1D<Derived>::reserve(int size)
{
  // nothing to do
  if (size < this->capacity() || isFixedSize_) return;
  // is this structure a ptr ?
  if (isRef()) { STKRUNTIME_ERROR_1ARG(IArray1D::reserve,size,cannot operate on references);}
  allocator_.realloc(Range(begin(), size));
}

template<class Derived>
void IArray1D<Derived>::clear()
{
  if (isRef()) return;   // Nothing to do for ref
  freeMem();  // Free Mem
}

template<class Derived>
void IArray1D<Derived>::move(Derived const& T)
{
  if (this->asPtrDerived() == &T) return; // avoid move on itself
  if (!isRef()) { freeMem();}
  allocator_.move(T.allocator_);  // move Allocator part
  setRange(T.range());            // Set ITContainer1D part
}

template<class Derived>
Derived& IArray1D<Derived>::pushBack( int n)
{
#ifdef STK_ARRAYS_VERY_VERBOSE
    stk_cout << _T("Entering IArray1D<Derived>::pushBack(") << n << _T(")\n");
#endif
  // checks
  if (n <= 0) return this->asDerived();
  if (isRef()) { STKRUNTIME_ERROR_1ARG(IArray1D::pushBack,n,cannot operate on references);}
  // If container is empty : create it, otherwise add element from end() position
  if (this->empty()) { initialize(RowRange(begin(), n));}
  else               { insertElt(end(), n);}
  return this->asDerived();
}

template<class Derived>
Derived& IArray1D<Derived>::popBack(int n)
{
  // checks
  if (n <= 0 || isFixedSize_) return this->asDerived();
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray1D::popBack,n,cannot operate on reference);}
  decLast(n);
  if (size() <= 0) this->freeMem(); // release mem if there's no more elts
  return this->asDerived();
}

template<class Derived>
Derived& IArray1D<Derived>::erase(int pos, int n)
{
#ifdef STK_DEBUG_ARRAY2D
      stk_cout << _T("Entering IArray1D::erase\n");
      stk_cout << _T("Deleting pos=") << pos <<_T(", n=") << n <<_T("\n");
#endif

//  STK_STATIC_ASSERT_DENSE_ONLY(Derived)
  // checks
  if (n<=0) return this->asDerived();
  if (isRef())
  { STKRUNTIME_ERROR_2ARG(IArray1D::erase,pos, n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
  if (begin() > pos)
  { STKOUT_OF_RANGE_2ARG(IArray1D::erase,pos, n,begin() > pos);}
  if (end() <= pos)
  { STKOUT_OF_RANGE_2ARG(IArray1D::erase,pos, n,end() <= pos);}
  if (end() < pos+n)
  { STKOUT_OF_RANGE_2ARG(IArray1D::erase,pos, n,end() < pos+n);}
#endif

  // translate remaining elements and update dimensions
  allocator_.memmove(pos, _R(pos+n, end()-1));

  decLast(n);
  if (size() <= 0) this->freeMem(); // if empty release allocated memory
  return this->asDerived();
}

template<class Derived>
Derived& IArray1D<Derived>::insertElt( int pos, int n)
{
  // checks
  if (n <= 0 ) return this->asDerived();
  if (isRef()) { STKRUNTIME_ERROR_2ARG(IArray1D::insertElt,pos,n,cannot operate on references);}
#ifdef STK_BOUNDS_CHECK
  if (begin() > pos)
  { STKOUT_OF_RANGE_2ARG(IArray1D::insertElt,pos, n,begin() > pos);}
  if (end() < pos)
  { STKOUT_OF_RANGE_2ARG(IArray1D::insertElt,pos, n,end() < pos);}
#endif

  // allocate, if necessary, memory for the elements
  if ( (capacity() < size()+n) && !isFixedSize_ )
  {
    // temporary container
    IArray1D Taux;
    exchange(Taux);
    // compute range of the container after insertion
    RowRange range(Taux.range());
    range.incLast(n);
    // allocate
    try { allocate(range);}
    catch (Exception const& error)   // if an error occur
    {
      exchange(Taux); // restore this
      throw error;    // and send again the Exception
    }
    // set range
    setRange(Taux.range());
    // copy original elements
    allocator_.memcpy(Taux.begin(), Taux.allocator_, Range(Taux.begin(), pos - begin()) );
    allocator_.memcpy(pos+n, Taux.allocator_, Range(pos, end()-pos) );
  }
  else // enough space -> shift the last elements
  {
    if (!isFixedSize_)
    { allocator_.memmove(pos+n, Range(pos, end() - pos));}
    else
    { allocator_.memmove(pos+n, Range(pos, end() - pos - n));}
  }
  incLast(n);
  return this->asDerived();
}

/* STL compatibility: Insert element @c v at position @c pos of the Array.
 *  @param pos position to insert elements
 *  @param v value to insert
 **/
template<class Derived>
Derived& IArray1D<Derived>::insert( int pos, Type const& v)
{
  insertElt(pos, 1);
  allocator_.setValue(pos,v);
  return this->asDerived();
}

template<class Derived>
Derived&  IArray1D<Derived>::insert( Range const& I, Type const& v)
{
  insertElt(I.begin(), I.size());
  for (int i=I.begin(); i<I.end(); i++) allocator_.setValue(i,v);
  return this->asDerived();
}

template<class Derived>
Derived& IArray1D<Derived>::push_front(Type const& v)
{
  insert(Range(begin(), 1), v);
  return this->asDerived();
}

template<class Derived>
Derived& IArray1D<Derived>::push_back(Type const& v)
{
  pushBack();
  allocator_.setValue(this->lastIdx(),v);
  return this->asDerived();
}

template<class Derived>
void IArray1D<Derived>::swap(int pos1, int pos2)
{
#ifdef STK_BOUNDS_CHECK
  if (begin() > pos1) { STKOUT_OF_RANGE_2ARG(IArray1D::swap,pos1,pos2,begin()>pos1);}
  if (end() <= pos1)  { STKOUT_OF_RANGE_2ARG(IArray1D::swap,pos1,pos2,end()<=pos1);}
  if (begin() > pos2) { STKOUT_OF_RANGE_2ARG(IArray1D::swap,pos1,pos2,begin()>pos2);}
  if (end() <= pos2)  { STKOUT_OF_RANGE_2ARG(IArray1D::swap,pos1,pos2,end()<=pos2);}
#endif
  std::swap(elt(pos1), elt(pos2));
}

template<class Derived>
void IArray1D<Derived>::exchange(IArray1D &T)
{
  allocator_.exchange(T.allocator_);
  Base::exchange(T);
}

template<class Derived>
Derived& IArray1D<Derived>::assign( IArray1D const& src)
{
  // cannot assign same data
  if (&allocator_ == &(src.allocator())) { return this->asDerived();}
  // Resize if necessary.
  if ( size() != src.size() ) { this->resize(src.range());}
  allocator_.memcpy(begin(), src.allocator_, src.range());
  return this->asDerived();
}

/* Copy n elements from pos2 to pos1, guaranteeing correct behavior for overlapping.
 *  @param pos1,pos2 positions of the elements to move
 *  @param n number of elements to move
 **/
template<class Derived>
void IArray1D<Derived>::memmove(int pos1, int pos2, int n)
{
#ifdef STK_BOUNDS_CHECK
  if (begin() > pos1) { STKOUT_OF_RANGE_2ARG(IArray1D::memmove,pos1,pos2,begin()>pos1);}
  if (end() <= pos1)  { STKOUT_OF_RANGE_2ARG(IArray1D::memmove,pos1,pos2,end()<=pos1);}
  if (begin() > pos2) { STKOUT_OF_RANGE_2ARG(IArray1D::memmove,pos1,pos2,begin()>pos2);}
  if (end() < pos2+n)  { STKOUT_OF_RANGE_2ARG(IArray1D::memmove,pos1,pos2,end()<pos2+n);}
#endif
  allocator_.memmove(pos1, Range(pos2, n));
}


/* set a value to this container.
 *  @param value the value to set
 **/
template<class Derived>
Derived& IArray1D<Derived>::setValue(Type const& value)
{
  allocator_.assign(range(), value);
  return this->asDerived();
}

template<class Derived>
void IArray1D<Derived>::initialize(RowRange const& I)
{
  allocate(I);
  allocator_.setRef(false);
  setRange(I);
}

template<class Derived>
void IArray1D<Derived>::allocate(RowRange const& I)
{
  try
  {
    allocator_.malloc(Arrays::evalRangeCapacity(I));
  }
  catch (Exception const& error)
  {
    setRange(); // if an error occur set default range
    throw error;
  }
}

template<class Derived>
void IArray1D<Derived>::freeMem()
{
  if (isRef()) return;  // Nothing to do for ref
  allocator_.free();
  setRange(RowRange(begin(),0));          // set range to default (Base)
}

} // namespace STK

#endif // STK_IARRAY1D_H
