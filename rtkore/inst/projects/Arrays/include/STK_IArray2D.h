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
 * Purpose:  Define the Interface for the Array classes.
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 *
 **/

/** @file STK_IArray2D.h
 *  @brief Interface base class for the Array2D classes, this is an internal
 *  header file, included by other containers library headers.
 *
 *  You should not attempt to use it directly but rather used one of the
 *  derived class like Array2D, except if you want to create your own
 *  Container Class.
 **/

#ifndef STK_IARRAY2D_H
#define STK_IARRAY2D_H

#include "STK_ArrayBase.h"
#include "STK_IContainer2D.h"
#include "STK_Array1D.h"

namespace STK
{

/** @ingroup Arrays
 *  @brief template interface base class for two-dimensional arrays.
 *
 * A IArray2D is an interface class for two-dimensional Arrays
 * stored in columns and having flexible dimensions. It is possible
 * to add, remove easily columns and rows in Derived class.
 *
 * Each column has a Range stored in the array @c rangeCols_.
 * It should be worth noting that we always have
 * @code
 *   (rangeCols_[j].size() <= capacityCol(j)) == true;
 *   (rangeCols_[j].isIn(this->rows()) == true;
 * @endcode
 *
 * Pseudo virtual function expected by this interface in derived classes are
 * @code
 *   void setValue1D(int i, TypeConst value); // for 1D arrays
 *   void resize1D(Range const& I); // for 1D arrays
 *   void shift1D(beg); // for 1D arrays*
 *
 *   void pushBack(int n=1);
 * @endcode
 *
 * @tparam Derived is the name of the class that implements @c IArray2D.
 **/
template < class Derived>
class IArray2D: protected IContainer2D<hidden::Traits<Derived>::sizeRows_, hidden::Traits<Derived>::sizeCols_>
              , public ArrayBase<Derived>
{
   // needed by merge
   template < class OtherDerived> friend class IArray2D;

  public:
    typedef typename hidden::Traits<Derived>::Type Type;
    typedef typename hidden::Traits<Derived>::TypeConst TypeConst;

    typedef typename hidden::Traits<Derived>::Row Row;
    typedef typename hidden::Traits<Derived>::Col Col;
    typedef typename hidden::Traits<Derived>::SubRow SubRow;
    typedef typename hidden::Traits<Derived>::SubCol SubCol;
    typedef typename hidden::Traits<Derived>::SubArray SubArray;
    // for 1D container
    typedef typename hidden::Traits<Derived>::SubVector SubVector;

    typedef typename hidden::Traits<Derived>::ColVector ColVector;
    typedef ColVector* PtrCol;
    typedef ColVector const* PtrColConst;

    enum
    {
      structure_ = hidden::Traits<Derived>::structure_,
      orient_    = hidden::Traits<Derived>::orient_,
      sizeRows_  = hidden::Traits<Derived>::sizeRows_,
      sizeCols_  = hidden::Traits<Derived>::sizeCols_,
      storage_   = hidden::Traits<Derived>::storage_
    };
    /** Type of the Range for the rows */
    typedef TRange<sizeRows_> RowRange;
    /** Type of the Range for the columns */
    typedef TRange<sizeCols_> ColRange;
    /** Type for the IContainer2D base Class. */
    typedef IContainer2D<sizeRows_, sizeCols_ > Base2D;
    /** Type for the Base Class. */
    typedef MemAllocator<PtrCol, sizeCols_> Allocator;
    /** type of the Base Container Class. */
    typedef ArrayBase<Derived> Base;

    using Base::elt;
    using Base2D::setCols;
    using Base2D::setRows;

  protected:
    /** Default constructor */
    IArray2D();
    /** constructor with specified ranges
     *  @param I,J rows and columns range
     **/
    IArray2D( Range const& I, Range const& J);
    /** Copy constructor If we want to wrap T, the main ptr will be wrapped
     *  in MemAllocator class. If we want to copy  T, Allocator is
     *  initialized to default values.
     *  @note bug correction, we have to use a copy of T.rangeCols_. in case
     *  we are using the code
     *  @code
     *  Array2DVector<TYPE> Dref(D.sub(J), true)
     *  @endcode
     *  we would get an error.
     *  @param T the container to copy
     *  @param ref true if we wrap T
     **/
    IArray2D( IArray2D const& T, bool ref =false);
    /** constructor by reference, ref_=1.
     *  @param T the container to wrap
     *  @param I,J rows and columns to wrap
     **/
    template<class OtherDerived>
    IArray2D( IArray2D< OtherDerived> const& T, Range const& I, Range const& J);
    /** destructor. Allocated horizontal memory (the array with the pointers
     *  on the columns) is liberated by the Allocator.
     **/
    ~IArray2D();

  public:
    /** clear the object.
     *  This will free all allocated memory and reset all range to Range().
     *  (while freeMem() does not modify rows range (rows_)
     **/
    void clear();
    /** move T to this.
     *  @note : T is not modified but just set as a reference of the data it was responsible.
     *  @param T the array to move.
     **/
    Derived& move(Derived const& T);
    /** exchange this container with T.
     *  @param T the container to exchange with this
     **/
    void exchange(IArray2D &T);

    // getters
    /**  @return @c true if the container is empty, @c false otherwise */
    inline bool isRef() const { return allocator_.isRef();}
    /** @return the Vertical range */
    inline RowRange const& rowsImpl() const { return Base2D::rows();}
    /**@return the Horizontal range */
    inline ColRange const& colsImpl() const { return Base2D::cols();}

    /** @return the Vertical range */
    inline RowRange const& rows() const { return Base2D::rows();}
    /** @return the index of the first row */
    inline int beginRows() const { return Base2D::beginRows();}
    /** @return the ending index of the rows */
    inline int endRows() const { return Base2D::endRows();}
    /** @return the number of rows */
    inline int sizeRows() const { return Base2D::sizeRows();}

    /**@return the Horizontal range */
    inline ColRange const& cols() const { return Base2D::cols();}
    /** @return the index of the first column */
    inline int beginCols() const { return Base2D::beginCols();}
    /**  @return the ending index of columns */
    inline int endCols() const { return Base2D::endCols();}
    /** @return the number of columns */
    inline int sizeCols() const { return Base2D::sizeCols();}

    /**  @return the index of the last column */
    inline int lastIdxCols() const { return Base2D::lastIdxCols();}
    /** @return the index of the last row */
    inline int lastIdxRows() const { return Base2D::lastIdxRows();}

    /**  @return @c true if the container is empty, @c false otherwise */
    inline bool empty() const { return Base2D::empty();}

    /** @return the allocator. */
    inline Allocator const& allocator() const { return allocator_;}
    /** @return a constant pointer on the j-th column of the container
     *  @param j the index of the column
     **/
    inline PtrColConst ptr(int j) const { return allocator_.elt(j);}
    /** @return the maximum possible number of columns without reallocation. */
    inline int availableCols() const { return allocator_.size();}
    /** @return the capacity of the column @c col.
     *  @param col index of the column we want the capacity
     **/
    inline int capacityCol(int col) const { return ptr(col) ? ptr(col)->capacity() : 0;}
    /** @return the range of each columns. */
    inline Array1D<Range, sizeCols_> const& rangeCols() const { return rangeCols_;}
    /** @return the range of a column.
     *  @param col index of the column we want the range
     **/
    inline Range const& rangeCol(int col) const { return rangeCols_[col];}

    /** implement setValue for vector/point/diagonal arrays*/
    inline void setValueImpl( int j, TypeConst v)
    {
      STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
      this->asDerived().setValue1D(j, v);
    }
    /** implement setValue for vector/point/diagonal arrays*/
    inline void setValueImpl( int i, int j, TypeConst v) { allocator_.elt(j)->setValue(i, v);}

    /** access to an element.
     *  @param i,j indexes of the element to get
     *  @return a reference on the (i,j) element
     **/
    inline Type& elt2Impl( int i, int j) { return allocator_.elt(j)->elt(i);}
    /** constant access to an element.
     *  @param i,j indexes of the element to get
     *  @return a constant reference on the (i,j) element
     **/
    inline TypeConst elt2Impl( int i, int j) const { return ptr(j)->elt(i);}

    // overloaded operators
    /** @return a constant reference on the element (i,j) of the 2D container.
     *  @param i,j row and column indexes
     **/
    inline TypeConst operator()(int i, int j) const
    {
#ifdef STK_BOUNDS_CHECK
       if (beginRows() > i) { STKOUT_OF_RANGE_2ARG(IArray::elt, i, j, beginRows() > i);}
       if (endRows() <= i)  { STKOUT_OF_RANGE_2ARG(IArray::elt, i, j, endRows() <= i);}
       if (beginCols() > j) { STKOUT_OF_RANGE_2ARG(IArray::elt, i, j, beginCols() > j);}
       if (endCols() <= j)  { STKOUT_OF_RANGE_2ARG(IArray::elt, i, j, endCols() <= j);}
#endif
      return elt(i,j);
    }
    /** @return a reference on the element (i,j) of the 2D container.
     *  @param i, j indexes of the element to get
     **/
    inline Type& operator()(int i, int j)
    {
#ifdef STK_BOUNDS_CHECK
       if (this->beginRows() > i) { STKOUT_OF_RANGE_2ARG(IArray2D::elt, i, j, beginRows() > i);}
       if (this->endRows() <= i)  { STKOUT_OF_RANGE_2ARG(IArray2D::elt, i, j, endRows() <= i);}
       if (this->beginCols() > j) { STKOUT_OF_RANGE_2ARG(IArray2D::elt, i, j, beginCols() > j);}
       if (this->endCols() <= j)  { STKOUT_OF_RANGE_2ARG(IArray2D::elt, i, j, endCols() <= j);}
#endif
      return elt(i,j);
    }
    /** @return the ith element
     *  @param i index of the element to get
     **/
    inline TypeConst operator[](int i) const
    {
      STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
#ifdef STK_BOUNDS_CHECK
      if (this->asDerived().begin() > i) { STKOUT_OF_RANGE_1ARG(IArray2D::elt, i, begin() > i);}
      if (this->asDerived().end() <= i)  { STKOUT_OF_RANGE_1ARG(IArray2D::elt, i, end() <= i);}
#endif
      return elt(i);
    }
    /** @return a reference on the ith element
     *  @param i index of the element to get
     **/
    inline Type& operator[](int i)
    {
      STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
#ifdef STK_BOUNDS_CHECK
      if (this->asDerived().begin() > i) { STKOUT_OF_RANGE_1ARG(IArray2D::elt, i, begin() > i);}
      if (this->asDerived().end() <= i)  { STKOUT_OF_RANGE_1ARG(IArray2D::elt, i, end() <= i);}
#endif
      return elt(i);
    }
    /** @return a constant reference on the number */
    inline TypeConst operator()() const { return elt();}
    /** @return the number */
    inline Type& operator()() { return elt();}

    //slicing
    /** access to a part of a column.
     *  @param j index of the column
     *  @return a reference in the range I of the column j of this
     **/
    Col col( int j) const;
    /** access to a part of a column.
     *  @param I range of the rows
     *  @param j index of the col
     *  @return a reference in the range I of the column j of this
     **/
    SubCol col(Range const& I, int j) const;
    /** access to many columns.
     *  @param J range of the index of the cols
     *  @return a 2D array containing the Container in the Horizontal range @c J
     **/
    SubArray col(Range const& J) const;
    /** access to a part of a row.
     *  @param i index of the row
     *  @return a reference of the row i.
     **/
    Row row( int i) const;
    /** access to a part of a row.
     *  @param i,J index of the row and range of the columns
     *  @return a reference of the row i.
     **/
    SubRow row(int i, Range const& J) const;
    /** access to many rows.
     *  @param I range of the index of the rows
     *  @return a 2D array containing the Container in the vertical range @c I
     **/
    SubArray row(Range const& I) const;
    /** @return  many elements.
     *  @param J Range of the elements
     **/
    SubVector sub(Range const& J) const;
    /** access to a sub-array.
     *  @param I,J range of the rows and of the columns
     **/
    SubArray sub(Range const& I, Range const& J) const;
    // overloaded operators for sub-arrays/vectors
    /** @return the sub-vector in given range
     *  @param I range to get
     **/
    SubVector operator[](Range const& I) const;
    /** @param I,j range of the rows and index of the column
     *  @return a Vertical container containing the column @c j of this
     *  in the range @c I
     **/
    SubCol operator()(Range const& I, int j) const;
    /** @param i,J index of the row and range of the columns
     *  @return an Horizontal container containing the row @c i of this
     *  in the range @c J
     **/
    SubRow operator()(int i, Range const& J) const;
    /** @param I,J range of the rows and of the columns
     *  @return a 2D container containing this in the range @c I, @c J
     **/
    SubArray operator()(Range const& I, Range const& J) const;
    /** @return the column j.
     *  @param j index of the column
     **/
    SubCol atCol(int j) const;
    /** @return the row i.
     *  @param i the index of the row
     **/
    Row atRow(int i) const;

    // modifiers
    /** function for reserving memory in all the columns
     *  @param sizeRows,sizeCols the size to reserve for the rows and columns
     **/
    void reserve(int sizeRows, int sizeCols);
    /** Reserve a certain amount of rows in all columns
     *  @param size the size to reserve
     **/
    void reserveRows(int size);
    /** Reserve a certain amount of columns
     *  @param sizeCols the size to reserve.
     **/
    void reserveCols(int sizeCols);

    /** @brief Set new beginning indexes to the array.
     *  @param rbeg, cbeg the indexes of the first row and first column to set
     **/
    void shift( int rbeg, int cbeg);
    /** New first index for the object (only for vectors/points/square/... arrays)
     *  @param beg the index of the first element to set
     **/
    void shift( int beg);
    /** New first index for the rows of the array.
     *  @param beg the index of the first row to set
     **/
    void shiftRows( int beg);
    /** New first index for the columns of the object.
     *  @param cbeg the index of the first column to set
     **/
    void shiftCols(int cbeg);

    /** resize the array.
     *  @note The implicit assumption made by this method is that it is easiest
     *  and faster to add column than add rows to the 2D array.
     *
     * @param I the new range for the rows of the array
     * @param J the new range for the columns of the array
     **/
    Derived& resize( Range const& I, Range const& J);
    /** @return the resized row/column/square array
     *  @param I the new range for the vector/point
     **/
    Derived& resize( Range const& I);

    // rows
    /** Insert n rows at position pos in the array
     *  If pos is outside the range of a column, then the method do nothing
     *  (useful for triangular/diagonal/... arrays).
     *  @param pos index where to insert rows
     *  @param n number of elements to insert (default is 1)
     **/
    void insertRows(int pos, int n =1);
    /** Delete n rows at the position pos
     *  @param pos index where to delete elements
     *  @param n number of rows to delete (default is 1)
    **/
    void eraseRows(int pos, int n=1);
    /** Insert n rows in front of the array.
     *  @param n number of elements to insert (default is 1)
     **/
    void pushFrontRows(int n =1);
    /** Add n rows to the array.
     *  @param n number of rows to add (default is 1)
     **/
    void pushBackRows( int n=1);
    /** Delete n first rows of the array.
     *  @param n number of rows to delete  (default is 1)
     **/
    void popFrontRows( int n = 1);
    /** Delete n latest rows of the array.
     *  @param n number of rows to delete  (default is 1)
     **/
    void popBackRows( int n = 1);
    /** set other at the beginning of this (concatenate). Perform a copy of the
     *  values stored in other to this.
     *  @param other the array to add
     *  @note the size and the type have to match
     **/
    template<class Other>
    Derived& pushFrontRows(ExprBase<Other> const& other);
    /** set other at the end of this (concatenate). Perform a copy of the
     *  values stored in other to this.
     *  @param other the array to add back
     *  @note the size and the type have to match
     **/
    template<class Other>
    Derived& pushBackRows(ExprBase<Other> const& other);
    // columns modifiers
    /** Insert n columns at the index pos to the array.
     *  @param pos position to insert columns
     *  @param n the number of column to insert (default is 1)
     **/
    void insertCols(int pos, int n =1);
    /** Delete n columns at the specified position of the array.
     *  @param pos the position of the deleted Columns
     *  @param n the number of column to delete (default is 1)
     **/
    void eraseCols(int pos, int n = 1);
    /** Insert n columns at the beginning of the array.
     *  @param n the number of column to insert (default is 1)
     **/
    void pushFrontCols(int n =1);
    /** Add n columns at the end of the array.
     *  @param n the number of Columns to add (default is 1)
     **/
    void pushBackCols(int n = 1);
    /** Delete first columns of the array
     *  @param n the number of Columns to delete (default is 1)
     **/
    void popFrontCols( int n =1);
    /** Delete last columns of the array
     *  @param n the number of Columns to delete (default is 1)
     **/
    void popBackCols( int n =1);
    /** merge (by value) the array other with this.
     *  @param other the array to merge to this
     **/
    template<class Other>
    Derived& pushFrontCols(ExprBase<Other> const& other);
    /** Specialization for Array1D. merge (by value) the array other with this
     *  @param other the column to add to this
     **/
    template<class Other>
    Derived& pushBackCols(IArray1D<Other> const& other);

    // modifiers, STL compatibility: for one dimension containers
    /** STL compatibility: push front an element.
     *  @param v value to push front
     **/
    void push_front(Type const& v);
    /** STL compatibility: append an element v.
     *  @param v value to append back
     **/
    void push_back(Type const& v);
    /** STL compatibility: insert element @c v in the range @c I of the Array.
     *  @param v,I value and range of indexes
     **/
    void insert( Range const& I, Type const& v);
    /**  STL compatibility:Delete n elements at the @c pos index from the container.
     *  @param pos index where to delete elements
     *  @param n number of elements to delete (default 1)
     **/
    void erase(int pos, int n=1);

     // Useful
     /** Swapping two columns.
      *  @param pos1, pos2 positions of the columns to swap
      **/
     void swapCols(int pos1, int pos2)
     {
 #ifdef STK_BOUNDS_CHECK
       if (this->beginCols() > pos1)
       { STKOUT_OF_RANGE_2ARG(IArray2D::swapCols,pos1, pos2,beginCols() >pos1);}
       if (this->endCols() <= pos1)
       { STKOUT_OF_RANGE_2ARG(IArray2D::swapCols,pos1, pos2,endCols() <= pos1);}
       if (this->beginCols() > pos2)
       { STKOUT_OF_RANGE_2ARG(IArray2D::swapCols,pos1, pos2,beginCols() >pos2);}
       if (this->endCols() <= pos2)
       { STKOUT_OF_RANGE_2ARG(IArray2D::swapCols,pos1, pos2,endCols() <=pos2);}
 #endif
       allocator_.swap(pos1, pos2);
       rangeCols_.swap(pos1,pos2);
     }
     /** swap two elements: only for vectors and points
      * @param i,j indexes of the elemet to swap
      **/
     void swap(int i, int  j)
     {
       STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
       std::swap(elt(i), elt(j));
     }
     /** Append the container @c other to @c this without copying the data
      *  explicitly. The column of @c other are appended to this.
      *
      *  Observe that the @c const keyword is not respected in this method:
      *  but it is useful to define this method even for constant objects.
      *
      *  @note data in itself are not altered nor duplicated, thus other and
      *  this possess the same columns.
      *  @param other the container to merge with this
      **/
     template<class OtherDerived>
     void merge(IArray2D< OtherDerived> const& other)
     {
       //checks
       if (isRef())
       { STKRUNTIME_ERROR_NO_ARG(IArray2D::merge(other),*this is a reference.);}
       if (other.isRef())
       { STKRUNTIME_ERROR_NO_ARG(IArray2D::merge(other),other is a reference.);}
       // if there is no columns, we can safely modify the vertical range
       if (this->sizeCols() <= 0) setRows(other.rows());
       if (this->rows() != other.rows())
       { STKRUNTIME_ERROR_NO_ARG(IArray2D::merge(other),this->rows() != other.rows());}

       // break const reference
       IArray2D< OtherDerived>& Tref = const_cast<IArray2D< OtherDerived>&>(other);
       // compute horizontal range of the container after insertion
       Range cols(this->cols());

       // save first index of the first column added before modification of cols
       const int first = cols.end();
       // reallocate memory for the columns
       cols.incLast(Tref.sizeCols());
       reallocCols(cols);

       // copy data from other
       Tref.shiftCols(first); // easiest like that
       for (int j=first; j< cols.end(); j++)
       { transferCol(Tref, j);}

       // delete and set view on the data
       Tref.allocator().free();
       Tref.allocator().setPtr(allocator_.p_data(), Tref.cols(), true);
     }
     /** Append the vector @c other to @c this without copying the data
      *  explicitly. @c other is appended to this and
      *  @c other will become a reference container. The data in itself are not
      *  altered, the Array1D become a reference on its own data.
      *  @param other the container to merge with this
      **/
     template<class OtherDerived>
     void merge(IArray1D<OtherDerived> const& other)
     {
       // checks
       if (isRef())
       { STKRUNTIME_ERROR_NO_ARG(IArray2D::merge(IArray1D),*this is a reference.);}
       if (other.isRef())
       { STKRUNTIME_ERROR_NO_ARG(IArray2D::merge(IArray1D),other is a reference.);}
       // if there is no columns, we can safely modify the vertical range
       if (this->sizeCols() <= 0) setRows(other.range());
       if (this->rows() != other.range())
       { STKRUNTIME_ERROR_NO_ARG(IArray2D::merge(IArray1D),this->rows() != other.range());}

       // compute horizontal range of the container after insertion
       Range cols(this->cols());

       // reallocate memory for the columns
       cols.incLast(1);
       reallocCols(cols);

       // set column
       allocator_.elt(cols.lastIdx()) = const_cast<OtherDerived*>(other.asPtrDerived());
       rangeCols_[cols.lastIdx()] = other.range();

       // set other as reference
       other.setRef(true);
     }
     /** @brief Update columns of the array at a specified range.
      *  @param J range of the columns to update
      **/
     void update(Range const& J)
     {
 #ifdef STK_BOUNDS_CHECK
       if (beginCols() > J.begin())
       { STKOUT_OF_RANGE_1ARG(IArray2D::update,J,beginCols() > J.begin());}
       if (endCols() < J.end())
       { STKOUT_OF_RANGE_1ARG(IArray2D::update,J,endCols() < J.end());}
 #endif
       for ( int icol = J.begin(); icol < J.end() ; ++icol)
       { update(icol);}
     }
     /** @brief Update column of the array at specified position.
      *  @param col index of the column to update
      **/
     void update(int col)
     {
 #ifdef STK_BOUNDS_CHECK
       if (beginCols() > col)
       { STKOUT_OF_RANGE_1ARG(IArray2D::update,col,beginCols() > col);}
       if (this->lastIdxCols() < col)
       { STKOUT_OF_RANGE_1ARG(IArray2D::update,col,lastIdxCols() < col);}
 #endif
       if (this->rangeRowsInCol(col) != rangeCols_[col])
       { resizeRowCol(col, this->rangeRowsInCol(col));}
     }

  protected:
    /** allocator of the column data set */
    Allocator allocator_;
    /** range of the index of the columns of the container. **/
    Array1D<Range, sizeCols_> rangeCols_;

    /** @return the allocator. */
    inline Allocator& allocator() { return allocator_;}

    /** Transfer the column pos2 to the column pos1 of this.
     *  @param pos1,pos2 indexes of the columns
     **/
    void transferCol( int pos1, int pos2)
    {
      // copy by address pointer on column pos2 of T in pos1 of this
      allocator_.elt(pos1) = allocator_.elt(pos2);
      rangeCols_[pos1] = rangeCols_[pos2];
    }
    /** Method for memory reallocation and initialization of the horizontal
     *  range of the container.
     *  The vertical range is not set in this method. If an
     *  error occur, we set the cols_ of the container to default.
     *  @param J horizontal range
     **/
    void reallocCols(Range const& J);

    /** Internal method for initializing to default values (null pointer and null range)
     *  to a range of columns.
     *  @param J horizontal range
     **/
    void nullCols(ColRange const& J);

  private:
    /** @brief Internal method for memory deallocation.
     *  This method clear all allocated memory and reset ranges to default value.
     *  Do nothing if this is a reference
     *  Range of the columns is set to default.
     *  Range of the rows (rows_) remains unmodified.
     **/
    void freeMem();
    /** Internal method for memory allocation and initialization of the horizontal
     *  range of the container. This method is called once at creation of the container.
     *
     *  The range of the column is not set in this method. Except if an error
     *  occur during memory allocation, cols_ will be set to default and an error
     *  thrown.
     *  @param J horizontal range
     **/
    void mallocCols(ColRange const& J);
    /** @brief Internal method for memory allocation and initialization of a range of columns.
     *  The capacity for the rows have to be set before calling this method.
     *  @param J vertical range of the Columns to initialize
     **/
    void initializeCols(Range const& J);
    /** @brief Internal method for releasing memory in a given range of columns.
     *  @param J range of columns to liberate.
     **/
    void freeCols(Range const& J);
    /** @brief Internal method for reserving memory in a range of columns.
     *  @param J range of the columns to initialize
     *  @param size the size to reserve
     **/
    void reserveRowsCols(Range const& J, int size);
    // single column methods
    /** copy column j of src into column j of this
     *  @param src source to copy
     *  @param j index of the columns to copy
     **/
    void copyCol(IArray2D const& src, int j);
    /** Internal method transferring column pos of container T to column
     *  pos of this. Set the column pos in T to zero.
     *  The column pos in this should not exists or should be deleted previously
     *  otherwise user will experiment a memory leak.
     *
     *  @param T the container with the column to transfer
     *  @param pos index of the column to transfer
     **/
    template<class OtherDerived>
    void transferCol( IArray2D< OtherDerived>& T, int pos)
    {
      allocator_.elt(pos) = T.allocator_.elt(pos);
      rangeCols_[pos]     = T.rangeCols_[pos];
      T.nullCol(pos);
    }
    /** Internal method setting default parameters and dimension to a column of the container.
     *  @param col the position of the column to initialize to a default value.
     *  @note if data is allocated, it will be lost
     **/
    void nullCol(int col);
    /** @brief Internal method for initializing a column.
     *  Method for the allocation of memory of the column col with given range I.
     *  @param col,I index and range of the column to initialize
     **/
    void initializeCol(int col, Range const& I);
    /** @brief Internal method for memory deallocation.
     *  @param col the number of the column to free
     **/
    void freeCol(int col);
    // modifiers
    /** @brief internal method for translating a column.
     *
     *  Method for the the allocation of memory of the column
     *  pos with the given range.
     *  @param col,beg index of the column and new begin of the column
     **/
    void shiftRowCol( int col, int beg);
    /** @brief Internal method for resizing a column with a specified range.
     *
     *  This method resize the column @c col to the desired range using:
     * - @c shiftRow
     * - either @c popBackRowsToCol or @c pushBackRowsToCol if needed.
     *  @param col index of the column
     *  @param I range to set to the column
     **/
    void resizeRowCol( int col, Range const& I);
    /** @brief Internal method for reserving rows to a specified column.
     *  reserve @c size memory place to the column @c col of the array
     *  @param col,size index of the column and size to reserve
     **/
    void reserveRowCol( int col, int size);
    /** @brief Internal method for inserting rows to a specified column.
     *
     *  Insert n rows at the position pos to the column column of the
     *  array. No check is done about the index.
     *  @param col,pos,n column position, row position and number of rows to insert
     **/
    void insertRowsCol( int col, int pos, int n);
    /** @brief Internal method for deleting rows from a specified column.
     *  Delete n rows at the position @c pos to the column @c col of the array.
     *
     *  @note It is possible to remove data outside the range of the column (but not
     *  outside the range of the array). In this case it is assumed
     *  that the data are zero and are not stored by the array
     *  (like for triangular or diagonal matrices).
     *
     *  @warning No check is done about indexes.
     *
     *  @param col,pos,n index of the column, row position and number of elements to delete
     **/
    void eraseRowsCol( int col, int pos, int n);
};

/* Default constructor */
template < class  Derived  >
IArray2D<Derived>::IArray2D(): Base2D(), Base()
                             , allocator_()
                             , rangeCols_()
{ mallocCols(this->cols());}
/** Constructor with specified ranges
 *  @param I,J range of the rows and columns
 **/
template < class  Derived  >
IArray2D<Derived>::IArray2D( Range const& I, Range const& J)
                           : Base2D(I,J), Base()
                           , allocator_()
                           , rangeCols_()
{  mallocCols(this->cols()); initializeCols(J);}
/* Copy constructor
 *  @param T the array to copy
 *  @param ref true if we wrap T
 **/
template < class  Derived  >
IArray2D<Derived>::IArray2D( IArray2D const& T, bool ref)
                           : Base2D(T), Base()
                           , allocator_(T.allocator_, ref)
                           , rangeCols_(T.rangeCols_, false) //  have to be created again, in case T is a temporary
{
  if (!ref)
  {
    initializeCols(T.cols()); // initialize the Columns
    for (int j=T.beginCols(); j<T.endCols(); j++)
    { copyCol(T, j);}
  }
}
/* constructor by reference, ref_=1.
 *  @param T the array to copy
 *  @param I,J ranges of the rows and columns to wrap
 **/
template < class  Derived  >
template<class OtherDerived>
IArray2D<Derived>::IArray2D( IArray2D<OtherDerived> const& T, Range const& I, Range const& J)
                           : Base2D(I,J), Base()
                           , allocator_(T.allocator_, J, true)  // a reference
                           , rangeCols_(T.rangeCols_, J, false) //  have to be created again, in case T is a temporary
                                          // T.rangeCols_ is itself a temporary that will be deleted
                                          // Tref(T.sub(J), true) for example
{
  for (int j=J.begin(); j<J.end(); j++)
  { rangeCols_[j] = inf(I, T.rangeCols()[j]);}
}
/* destructor.
 *  free the vertically allocated memory (the columns). The horizontally
 *  allocated memory is handled by the Allocator class.
 **/
template < class  Derived  >
IArray2D<Derived>::~IArray2D() { if (!isRef()) freeCols(cols());}

/* clear the object.
 *  This will free all allocated memory and reset all range to Range().
 *  (while freeMem() does not modify rows range (rows_)
 **/
template < class  Derived  >
void IArray2D<Derived>::clear()
{
  // Nothing to do for reference
  if (isRef()) return;
  freeMem();
  this->setRanges();
}
/* move T to this.
 *  @note : T is not modified but just set as a reference of the data it was responsible.
 *  @param T the array to move.
 **/
template < class  Derived  >
Derived& IArray2D<Derived>::move(Derived const& T)
{
  if (this->asPtrDerived() == &T) return this->asDerived();
  if (!isRef()) { freeCols(cols());}
  // move Base part
  allocator_.move(T.allocator_); // T become a reference
  rangeCols_.move(T.rangeCols_);
  // Set IContainer2D part
  setCols(T.cols());
  setRows(T.rows());
  return this->asDerived();
}
/* exchange this container with T.
 *  @param T the container to exchange with this
 **/
template < class  Derived  >
void IArray2D<Derived>::exchange(IArray2D &T)
{
  // swap MemAllocator part
  allocator_.exchange(T.allocator_);
  Base2D::exchange(T);
  rangeCols_.exchange(T.rangeCols_);
}

/* @brief Internal method for memory deallocation.
 *  This method clear all allocated memory and reset ranges to default value.
 *  Do nothing if this is a reference
 *  Range of the columns is set to default.
 *  Range of the rows (rows_) remains unmodified.
 **/
template< class Derived>
void IArray2D< Derived>::freeMem()
{
  if (isRef()) return;
  freeCols(cols());
  // free memory allocated in allocator.
  // For fixed size arrays, the size remain the same
  allocator_.free();
  setCols(allocator_.range());
  // clear arrays
  rangeCols_.resize(cols());
}
/* Method for memory allocation and initialization of the horizontal
 *  range of the container.
 *  The vertical range is not set in this method. If an
 *  error occur, we set the cols_ of the container to default.
 *  @param J horizontal range
 **/
template< class Derived>
void IArray2D< Derived>::mallocCols(ColRange const& J)
{
  // try to allocate memory
  try
  {
    if (J.size() > availableCols())
    {
      // compute the necessary size
      int size= Arrays::evalSizeCapacity(J.size());
      allocator_.malloc(Range(J.begin(), size)); // allocate memory for the columns
    }
    else
    { allocator_.shift(J.begin());}
    rangeCols_.resize(J);
  }
  catch (Exception const& error)   // if an error occur
  {
    Base2D::setCols();      // set default range
    rangeCols_.clear();     // clear this->rangeCols_
    allocator_.free();      // initialize with zero
    allocator_.setValue(0); // initialize with zero
    throw error;            // throw the error
  }
  allocator_.setValue(0); // initialize with zero
}
/* @brief Internal method for memory allocation and initialization of a range of columns.
 *  The capacity for the rows have to be set before calling this method.
 *  @param J vertical range of the Columns to initialize
 **/
template< class Derived>
void IArray2D< Derived>::initializeCols(Range const& J)
{
#ifdef STK_BOUNDS_CHECK
if (beginCols() > J.begin())
{ STKOUT_OF_RANGE_1ARG(IArray2D::initializeCols, J, beginCols() > J.begin());}
if (endCols() < J.end())
{ STKOUT_OF_RANGE_1ARG(IArray2D::initializeCols, J, endCols() < J.end());}
#endif
  for (int j=J.begin(); j<J.end(); j++)
  {
    try
    { initializeCol(j, this->rangeRowsInCol(j));}
    catch (Exception const& error) // if an error occur
    {
      // free each column allocated and throw exception
      for (int k=J.begin(); k<j; k++) freeCol(k);
      throw error;
    }
  }
}
/* @brief Internal method for releasing memory in a given range of columns.
 *  @param J range of columns to liberate.
 **/
template< class Derived>
void IArray2D< Derived>::freeCols(Range const& J)
{ for (int j=J.begin(); j<J.end(); j++) { freeCol(j);}}
/* copy column j of src into column j of this
 *  @param src source to copy
 *  @param j index of the columns to copy
 **/
template< class Derived>
void IArray2D< Derived>::copyCol(IArray2D const& src, int j)
{
  PtrCol dp =allocator_.elt(j), sp =src.allocator_.elt(j);
  dp->assign(*sp);
}

/* Method for memory reallocation and initialization of the horizontal
 *  range of the container.
 *  The vertical range is not set in this method. If an
 *  error occur, we set the cols_ of the container to default.
 *  @param J horizontal range
 **/
template< class Derived>
void IArray2D< Derived>::reallocCols(Range const& J)
{
  Range oldRange(cols());
  // try to allocate memory
  if (J.size() > availableCols())
  {
    int size= Arrays::evalSizeCapacity(J.size());
    Range newRange(J.begin(), size);
    allocator_.realloc( newRange ); // reallocate memory for the columns
  }
  rangeCols_.resize(J);        // initialize this->rangeCols_
  setCols(J);
}

/* Internal method for initializing to default values (null pointer and null range)
 *  to a range of columns.
 *  @param J horizontal range
 **/
template< class Derived>
void IArray2D< Derived>::nullCols(ColRange const& J)
{
#ifdef STK_BOUNDS_CHECK
  if (beginCols() > J.begin())
  { STKOUT_OF_RANGE_1ARG(IArray2D::nullCols, J, beginCols() > J.begin());}
  if (endCols() < J.end())
  { STKOUT_OF_RANGE_1ARG(IArray2D::nullCols, J, endCols() < J.end());}
#endif
  for (int j=J.begin(); j<J.end(); ++j) { nullCol(j);}
}
/* Internal method setting default parameters and dimension to a column of the container.
 *  @param col the position of the column to initialize to a default value.
 *  @note if data is allocated, it will be lost
 **/
template< class Derived>
void IArray2D< Derived>::nullCol(int col)
{
  allocator_.elt(col) = 0;
  rangeCols_[col] = Range();
}

/* @brief Internal method for initializing a column.
 *  Method for the the allocation of memory of the column col with range I.
 *  @param col,I index and range of the column to initialize
 **/
template< class Derived>
void IArray2D< Derived>::initializeCol(int col, Range const& I)
{
#ifdef STK_DEBUG_ARRAY2D
  stk_cout << _T("Entering initializeCol\n");
  stk_cout << _T("Initialize col=") << col <<_T(", I=") << I <<_T("\n");
#endif
  if (I.size() <=0)
  {
    allocator_.elt(col) = 0;
    rangeCols_[col] = I;
    return;
  }
  // try to allocate memory
  try
  { allocator_.elt(col) = new ColVector(I);}
  catch (std::bad_alloc & error)  // if an alloc error occur
  {
    allocator_.elt(col) = 0;
    rangeCols_[col] = Range();
    STKRUNTIME_ERROR_2ARG(IArray2D::initializeCol,col, I,memory allocation failed.);
  }
  rangeCols_[col] = I;
}
/* @brief Internal method for memory deallocation.
 *  @param col the number of the column to free
 **/
template< class Derived>
void IArray2D< Derived>::freeCol(int col)
{
#ifdef STK_DEBUG_ARRAY2D
  stk_cout << _T("Entering freeCol\n");
  stk_cout << _T("Deleting col=") << col <<_T(", rangeCols_[col]=") << rangeCols_[col] <<_T("\n");
  stk_cout << _T("allocator_.elt(col)=") << allocator_.elt(col) <<_T("\n");
#endif
  if (allocator_.elt(col)) // if there is a column at this position
  {
    delete allocator_.elt(col);
    allocator_.elt(col) =0;
    rangeCols_[col] = Range();
  }
}

} // namespace STK

#endif
// STK_IARRAY2D_H
