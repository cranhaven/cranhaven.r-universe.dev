/*--------------------------------------------------------------------*/
/*     Copyright (C) 2004-2017  Serge Iovleff, Université Lille 1, Inria

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

/** @file STK_ISparseArray1D.h
 *  @brief In this file we define the base class for Sparse Matrices.
 **/

#ifndef STK_ISPARSEARRAY1D_H
#define STK_ISPARSEARRAY1D_H

#include "allocators/STK_MemSAllocator.h"


namespace STK
{

/** @class ISparseArray1D
  * @ingroup Arrays
  *
  * @brief Interface class for SparseArray1D
  *
  * This class is the base class that is inherited by all objects (matrix, vector,
  * point) which are not expressions and stored as SparseArrays. The common API
  * for these objects is contained in this class.
  *
  * @tparam Derived is the derived type, e.g., a matrix type.
  **/
template<class Derived>
class ISparseArray1D: public ITContainer1D<Derived>
{
  public:
    typedef ITContainer1D<Derived> Base;

    typedef typename hidden::Traits<Derived>::Type Type;
    //typedef typename hidden::Traits<Derived>::Row  Row;
    typedef typename hidden::Traits<Derived>::Col  Col;
    //typedef typename hidden::Traits<Derived>::SubRow SubRow;
    //typedef typename hidden::Traits<Derived>::SubCol SubCol;
    //typedef typename hidden::Traits<Derived>::SubVector SubVector;
    typedef typename hidden::Traits<Derived>::SubArray SubArray;

    enum
    {
      structure_ = hidden::Traits<Derived>::structure_,
      orient_    = hidden::Traits<Derived>::orient_,
      sizeRows_  = hidden::Traits<Derived>::sizeRows_,
      sizeCols_  = hidden::Traits<Derived>::sizeCols_,
      storage_   = hidden::Traits<Derived>::storage_
    };
    typedef hidden::CheckShift<Derived, structure_> CheckShift;
    /** Type of the Range for the rows */
    typedef TRange<sizeRows_> RowRange;
    /** Type of the Range for the columns */
    typedef TRange<sizeCols_> ColRange;

    using Base::range;
    using Base::begin;
    using Base::end;
    using Base::size;
    using Base::elt;
    using Base::setRange;

  protected:

    /** Default constructor. */
    ISparseArray1D();
    /** constructor with a specified Range.
      *  @param I range of the container
     **/
    ISparseArray1D( Range const& I);
    /** Misc constructor with first and last, initialization with a constant.
     *  @param I range of the container
     *  @param v initial value of the container
     **/
    ISparseArray1D( Range const& I, Type const& v);
    /** Copy constructor
     *  @param T the container to copy
     *  @param ref true if T is wrapped
     **/
    ISparseArray1D( const ISparseArray1D &T, bool ref =false);
    /** Copy constructor
     *  @param T the container to copy
     **/
    template<class OtherDerived>
    ISparseArray1D( ExprBase<OtherDerived> const& T);
    /** constructor by reference, ref_=1.
     *  @param T, I the container and the range of data to wrap
     **/
    ISparseArray1D( ISparseArray1D const& T, Range const& I);
    /** constructor by reference, ref_=1.
     *  @param T,I the container and the range of data to wrap
     **/
    template<class OtherDerived>
    ISparseArray1D( ISparseArray1D<OtherDerived> const& T, Range const& I);

    /** destructor: allocated memory is liberated by MemAllocator base class.*/
    ~ISparseArray1D() {}

  public:
    /** @return @c true if *this is reference container, @c false otherwise */
    inline bool isRef() const { return allocator_.isRef();}
    /** Modify the state of the container: this become a reference (if ref is
     *  @c true) or the owner of the data (if ref is @c false).
     *  @note To use with care in order to avoid memory leak
     *  @param ref : has top be false if this own its own data
     **/
    inline void setRef(bool ref) const { allocator_.setRef(ref);}

//    /** @return a constant pointer on the data set*/
//    inline Type* const& p_data() const { return allocator_.p_data();}
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
    int capacity() const { return isRef() ? 0 : allocator_.size();}

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
    void exchange(ISparseArray1D &T);
    /** overwrite @c this with @c src.
     *  @note If the size match, @c this is not resized, and in this case,
     *  the method take care of the possibility of overlapping.
     *  @param src the container to assign
     **/
    Derived& assign( ISparseArray1D const& src);

    /** set a value to this container.
     *  @param value the value to set
     **/
    Derived& setValue(Type const& value);
};

}

#endif /* STK_ISPARSEARRAY1D_H */
