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

/** @file STK_IArray2DModifiers.h
 *  @brief In this file we implement the modifiers of the IArray2D
 **/

#ifndef STK_IARRAY2DMODIFIERS_H
#define STK_IARRAY2DMODIFIERS_H

namespace STK
{

/*@brief function for reserving memory in all the columns
 *  @param sizeRows,sizeCols the size to reserve for the rows and columns
 **/
template < class  Derived  >
void IArray2D< Derived>::reserve(int sizeRows, int sizeCols)
{
  reserveCols(sizeCols);
  reserveRows(sizeRows);
}
/*@brief function for reserving memory in all the columns
 *  @param size the size to reserve
 **/
template < class  Derived  >
void IArray2D< Derived>::reserveRows(int size)
{ reserveRowsCols(cols(), size);}
/*Reserve a certain amount of columns
 *  @param sizeCols the size to reserve.
 **/
template< class Derived>
void IArray2D< Derived>::reserveCols(int sizeCols)
{
  if (availableCols() >= sizeCols) return;
  // is this structure just a pointer?
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::reserveCols,sizeCols,cannot operate on references);}
  // try to allocate memory
  try
  {
    Range J(this->beginCols(), sizeCols);
    allocator_.realloc(J);
    rangeCols_.reserve(J.size());
  }
  catch (Exception const& error)   // if an error occur
  {
    STKRUNTIME_ERROR_1ARG(IArray2D::reserveCols,sizeCols,bad alloc error);
  }
}

/*@brief Internal method for reserving memory in a range of columns.
 *  @param J range of the columns to initialize
 *  @param size the size to reserve
 **/
template < class  Derived  >
void IArray2D< Derived>::reserveRowsCols(Range const& J, int size)
{
  for (int j=J.begin(); j<J.end(); j++)
  { reserveRowCol(j, size);}
}
/*@brief Internal method for reserving rows to a specified column.
 *  reserve @c size memory place to the column @c col of the array
 *  @param col,size index of the column and size to reserve
 **/
template < class  Derived  >
void IArray2D< Derived>::reserveRowCol( int col, int size)
{
  if (this->capacityCol(col) >= size) return;
  allocator_.elt(col)->reserve(size);
}


// shift
/*@brief Set new beginning indexes to the array.
 *  @param rbeg, cbeg the indexes of the first row and first column to set
 **/
template < class  Derived  >
void IArray2D< Derived>::shift( int rbeg, int cbeg)
{
  shiftCols(cbeg);
  shiftRows(rbeg);
}
/*New first index for the object.
 *  @param beg the index of the first element to set
 **/
template < class  Derived  >
void IArray2D< Derived>::shift( int beg)
{ this->asDerived().shift1D(beg);}
/*New first index for the rows of the array.
 *  @param beg the index of the first row to set
 **/
template < class  Derived  >
void IArray2D< Derived>::shiftRows( int beg)
{
  // compute increment
  int inc = beg - beginRows();
  if (inc == 0) return;
  // is this structure just a pointer?
  if (isRef()) { STKRUNTIME_ERROR_1ARG(IArray2D::shiftRows,beg,cannot operate on reference);}

  // translate rows
  Base2D::shiftRows(beg);
  // if there is rows, for all cols shift
  for (int j=beginCols(); j<endCols(); j++)
  { shiftRowCol(j, rangeCols_[j].begin()+inc);}
}
/*New beginning index for the columns of the object.
 *  @param cbeg the index of the first column to set
 **/
template < class  Derived  >
void IArray2D< Derived>::shiftCols(int cbeg)
{
  // if there is something to do
  if (cbeg == this->beginCols()) return;
  // is this structure just a pointer?
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::shiftCols,cbeg,cannot operate on references);}
  // shift beginCols()
  allocator_.shift(cbeg);     // translate data
  rangeCols_.shift(cbeg);     // translate rangeCols_
  Base2D::shiftCols(cbeg);    // adjust dimensions
}
/*@brief internal method for translating a column.
 *
 *  Method for the the allocation of memory of the column
 *  pos with the given range.
 *  @param col,beg index of the column and new begin of the column
 **/
template < class  Derived  >
void IArray2D< Derived>::shiftRowCol( int col, int beg)
{
  if (allocator_.elt(col)) { allocator_.elt(col)->shift(beg);}
  rangeCols_[col].shift(beg);
}
/*resize the array.
 *  @note The implicit assumption made by this method is that it is easiest
 *  and faster to add column than add rows to the 2D array.
 *
 * @param I the new range for the rows of the array
 * @param J the new range for the columns of the array
 **/
template < class  Derived  >
Derived& IArray2D< Derived>::resize( Range const& I, Range const& J)
{
  // check if there is something to do
  if ((this->rows() == I) && (this->cols() == J)) return this->asDerived();
  if (isRef())
  { STKRUNTIME_ERROR_2ARG(IArray2D::resize,I,J,cannot operate on reference);}
  //  translate and check again if there is something to do
  shift(I.begin(), J.begin());
  if ((this->rows() == I) && (this->cols() == J)) return this->asDerived();
  // just clear empty container
  if (I.size()<=0 || J.size() <= 0)
  { clear(); return this->asDerived();}

  // number of rows and columns to delete or add
  int rinc = I.end() - endRows();
  int cinc = J.end() - endCols();

  // work first on rows as we add columns
  if ((cinc >=0))
  {
    if (rinc < 0) { popBackRows(-rinc);} // less rows
    else          { pushBackRows(rinc);} // more rows
    pushBackCols(cinc); // add columns
  }
  else // work first on columns as we remove column
  {
    popBackCols(-cinc); // remove columns
    if (rinc < 0) { popBackRows(-rinc);} // less rows
    else          { pushBackRows(rinc);} // more rows
  }
  return this->asDerived();
}
/*@return the resized row/column/square array
 *  @param I the new range for the vector/point
 **/
template < class  Derived  >
Derived& IArray2D< Derived>::resize( Range const& I)
{ return this->asDerived().resize1D(I);}

// rows
/*Insert n rows in front of the array.
 *  @param n number of elements to insert (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::pushFrontRows(int n)
{
  if (n <= 0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::pushFrontRows,n,cannot operate on reference);}

  insertRows(beginRows(), n);
}
/*Add n rows to the array.
 *  @param n number of rows to add (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::pushBackRows( int n)
{
  if (n <= 0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::pushBackRows,n,cannot operate on reference);}

  insertRows(endRows(), n);
}
/*Delete n first rows of the array.
 *  @param n number of rows to delete  (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::popFrontRows( int n)
{
  if (n <= 0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::popFrontRows,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
  if (sizeRows() < n)
  { STKOUT_OF_RANGE_1ARG(IArray2D::popFrontRows,n,sizeRows() < n);}
#endif
  eraseRows(endRows()-n, n);
}
/*Delete n latest rows of the array.
 *  @param n number of rows to delete  (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::popBackRows( int n)
{
  if (n <= 0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::popBackRows,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
  if (sizeRows() < n)
  { STKOUT_OF_RANGE_1ARG(IArray2D::popBackRows,n,sizeRows() < n);}
#endif
  eraseRows(endRows()-n, n);
}
/*set other at the beginning of this (concatenate). Perform a copy of the
 *  values stored in other to this.
 *  @param other the array to add
 *  @note the size and the type have to match
 **/
template < class  Derived  >
template<class Other>
Derived& IArray2D< Derived>::pushFrontRows(ExprBase<Other> const& other)
{
  // check if the array is empty
  if (empty())
  {
    this->asDerived() = other.asDerived();
    return this->asDerived();
  }
  // not empty
  if (other.cols() != this->cols())
    STKRUNTIME_ERROR_NO_ARG(IArray2D::pushBackRows,range of the columns are different);
  // add nbRow to existing rows
  int nbRow = other.sizeRows();
  pushFrontRows(nbRow);
  for (int j=beginCols(); j< endCols(); ++j)
  {
    for (int i=beginRows(), iOther= other.beginRows(); iOther<other.endRows(); ++i, ++iOther)
    { setValue(i,j, other.elt(iOther,j));}
  }
  // return this
  return this->asDerived();
}
/*set other at the end of this (concatenate). Perform a copy of the
 *  values stored in other to this.
 *  @param other the array to add back
 *  @note the size and the type have to match
 **/
template < class  Derived  >
template<class Other>
Derived& IArray2D< Derived>::pushBackRows(ExprBase<Other> const& other)
{
  // check if the array is empty
  if (empty())
  {
    this->asDerived() = other.asDerived();
    return this->asDerived();
  }
  // not empty
  if (other.cols() != this->cols())
    STKRUNTIME_ERROR_NO_ARG(IArray2D::pushBackRows,range of the columns are different);
  // add nbRow to existing rows
  int nbRow = other.sizeRows();
  pushBackRows(nbRow);
  for (int j=beginCols(); j< endCols(); ++j)
  {
    // start from the end in order to avoid
    for (int i=this->lastIdxRows(), iOther= other.lastIdxRows(); iOther>=other.beginRows(); --i, --iOther)
    { elt(i,j) = other.elt(iOther,j);}
  }
  // return this
  return this->asDerived();
}

// columns
/*Insert n columns at the beginning of the array.
 *  @param n the number of column to insert (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::pushFrontCols(int n)
{
  if (n <= 0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::pushFrontCols,n,cannot operate on reference);}

  insertCols(beginCols(), n );
}
/*Add n columns at the end of the array.
 *  @param n the number of Columns to add (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::pushBackCols(int n)
{
  if (n <= 0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::pushBackCols,n,cannot operate on reference);}

  insertCols(endCols(), n );
}
/*Delete first columns of the array
 *  @param n the number of Columns to delete (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::popFrontCols( int n)
{
  if (n<=0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::popBackCols,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
  if (sizeCols() < n)
  { STKOUT_OF_RANGE_1ARG(IArray2D::popBackCols,n,sizeCol() < n);}
#endif

  eraseCols(beginCols(), n);
}
/*Delete last columns of the array
 *  @param n the number of Columns to delete (default is 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::popBackCols( int n)
{
  if (n<=0) return;
  if (isRef())
  { STKRUNTIME_ERROR_1ARG(IArray2D::popBackCols,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
  if (sizeCols() < n)
  { STKOUT_OF_RANGE_1ARG(IArray2D::popBackCols,n,sizeCol() < n);}
#endif

  eraseCols(endCols()-n, n);
}
/*merge (by value) the array other with this.
 *  @param other the array to merge to this
 **/
template < class  Derived  >
template<class Other>
Derived& IArray2D< Derived>::pushFrontCols(ExprBase<Other> const& other)
{
  // if the array is empty use operator=
  if (empty())
  {
    this->asDerived() = other.asDerived();
    return this->asDerived();
  }
  // this is not empty
  if (other.rows() != this->rows())
  { STKRUNTIME_ERROR_NO_ARG(IArray2D::pushFrontCols,range of the rows are different);}
  // if the array is not empty we add the column and copy other inside
  this->asDerived().insertCols(beginCols(), other.sizeCols());
  for (int j= beginCols(), j1= other.beginCols(); j < endCols(); ++j, ++j1)
  {
    *allocator_.elt(j) = other.col(j1);
  }
  // return this
  return this->asDerived();
}
/*Specialization for Array1D. merge (by value) the array other with this
 *  @param other the column to add to this
 **/
template < class  Derived  >
template<class Other>
Derived& IArray2D< Derived>::pushBackCols(IArray1D<Other> const& other)
{
  // check if the array is empty
  if (empty())
  {
    resize(other.rows(),1);
    for (int i=other.begin(); i<other.end(); i++)
      (*this)(i, lastIdxCols()) = other[i];
    return this->asDerived();
  }
  // not empty
  if (other.rows() != this->rows())
  { STKRUNTIME_ERROR_NO_ARG(IArray2D::pushBackCols(other),other.rows() != rows());}
  int last = endCols();
  pushBackCols();
  for (int i=other.begin(); i<other.end(); i++)
    (*this)(i, last) = other[i];
  // return this
  return this->asDerived();
}

 // columns
 /*Insert n columns at the index pos to the array.
  *  @param pos position to insert columns
  *  @param n the number of column to insert (default is 1)
  **/
template < class  Derived  >
 void IArray2D< Derived>::insertCols(int pos, int n)
 {
   if (n <= 0) return;

   if (isRef())
   { STKRUNTIME_ERROR_2ARG(IArray2D::insertCols,pos,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
   if (beginCols() > pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::insertCols,pos,n,beginCols() > pos);}
   if (endCols() < pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::insertCols,pos,n,endCols() < pos);}
#endif
   // compute column range of the array after insertion
   Range OldRange(cols()), NewRange(cols());
   NewRange.incLast(n);
   reallocCols(NewRange);
   // translate and copy last Columns from Taux to this
   for (int k=OldRange.lastIdx(); k>=pos; k--)
   { transferCol(k+n, k);}
   // initialize the rows in the range pos:pos+n-1
   nullCols( Range(pos, n) );
   initializeCols( Range(pos, n) );
 }
 /*Delete n columns at the specified position of the array.
  *  @param pos the position of the deleted Columns
  *  @param n the number of column to delete (default is 1)
  **/
template < class  Derived  >
 void IArray2D< Derived>::eraseCols(int pos, int n)
 {
   if (n<=0) return;
   if (isRef())
   { STKRUNTIME_ERROR_2ARG(IArray2D::eraseCols,pos,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
   if (beginCols() > pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::eraseCols,pos,n,beginCols() > pos);}
   if (endCols() <= pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::eraseCols,pos,n,endCols() <= pos);}
   if (endCols() < pos+n)
   { STKOUT_OF_RANGE_2ARG(IArray2D::eraseCols,pos,n,endCols() < pos+n);}
#endif
   // delete each col
   freeCols(Range(pos, n));
   // update cols_, rangeCols_
   this->decLastIdxCols(n);
   rangeCols_.erase(pos, n);
   allocator_.memmove(pos, Range(pos+n, endCols()-pos));
   // liberate memory if there is no more columns (don't use clear(), as we want to preserve rows_)
   if (sizeCols() == 0) { freeMem();}
 }

 // rows
 /*Insert n rows at position pos in the array
  *  If pos is outside the range of a column, then the method do nothing
  *  (useful for triangular/diagonal/... arrays).
  *  @param pos index where to insert rows
  *  @param n number of elements to insert (default is 1)
  **/
template < class  Derived  >
 void IArray2D< Derived>::insertRows(int pos, int n)
 {
   if (n <= 0) return;
   if (isRef())
   { STKRUNTIME_ERROR_2ARG(IArray2D::insertRows,pos,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
   if (beginRows() > pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::insertRows,pos,n,beginRows() > pos);}
   if (endRows() < pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::insertRows,pos,n,endRows() < pos);}
#endif

   this->incLastIdxRows(n);
   for (int j=beginCols(); j<endCols(); j++)
   {
      if (!allocator_.elt(j))
      { initializeCol(j, this->rangeRowsInCol(j));}
      else
      {
      if ( (pos >= rangeCols_[j].begin()) && (pos <= rangeCols_[j].end()))
      { insertRowsCol(j, pos, n);}
     }
   }
 }
 /*Delete n rows at the position pos
  *  @param pos index where to delete elements
  *  @param n number of rows to delete (default is 1)
 **/
template < class  Derived  >
 void IArray2D< Derived>::eraseRows(int pos, int n)
 {
   if (n<=0) return;
   // is this structure just a pointer?
   if (isRef())
   { STKRUNTIME_ERROR_2ARG(IArray2D::eraseRows,pos,n,cannot operate on reference);}
#ifdef STK_BOUNDS_CHECK
   if (beginRows() > pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::eraseRows,pos,n,beginRows() > pos);}
   if (endRows() <= pos)
   { STKOUT_OF_RANGE_2ARG(IArray2D::eraseRows,pos,n,endRows() <= pos);}
   if (endRows() < pos+n)
   { STKOUT_OF_RANGE_2ARG(IArray2D::eraseRows,pos,n,endRows() < pos+n);}
#endif

   for (int j=beginCols(); j<endCols(); j++) { eraseRowsCol(j, pos, n);}
   this->decLastIdxRows(n);
 }

// STL compatibility: for one dimension containers
/*STL compatibility: push front an element.
 *  @param v value to push front
 **/
template < class  Derived  >
void IArray2D< Derived>::push_front(Type const& v)
{ // push_front defined for vector, point and diagonal arrays
  STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
  insert(Range(this->begin(), 1), v);
}
/*STL compatibility: append an element v.
 *  @param v value to append back
 **/
template < class  Derived  >
void IArray2D< Derived>::push_back(Type const& v)
{ // push_back defined for vector, point and diagonal arrays
  STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
  this->asDerived().pushBack();
  this->back() = v;
}
/*STL compatibility: insert element @c v in the range @c I of the Array.
 *  @param v,I value and range of indexes
 **/
template < class  Derived  >
void IArray2D< Derived>::insert( Range const& I, Type const& v)
{ // insert defined for vector, point and diagonal arrays
  STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
  this->asDerived().insertElt(I.begin(), I.size());
  for (int i=I.begin(); i<I.end(); i++) { elt(i) = v;}
}

/*  STL compatibility:Delete n elements at the @c pos index from the container.
 *  @param pos index where to delete elements
 *  @param n number of elements to delete (default 1)
 **/
template < class  Derived  >
void IArray2D< Derived>::erase(int pos, int n)
{
  STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
  if (structure_ == Arrays::vector_) { eraseRows(pos, n); }
  else { eraseCols(pos, n); }

}


/*@brief Internal method for resizing a column with a specified range.
 *
 *  This method resize the column @c col to the desired range using:
 * - @c shiftRowCol
 * - either @c popBackRowsToCol or @c pushBackRowsToCol if needed.
 *  @param col index of the column
 *  @param I range to set to the column
**/
template < class  Derived  >
void IArray2D< Derived>::resizeRowCol( int col, Range const& I)
{
  if (rangeCols_[col] == I) return;
  shiftRowCol(col, I.begin());
  int inc = rangeCols_[col].size() - I.size();
  if (inc == 0) return;
  if (inc < 0)
  {
    allocator_.elt(col)->pushBack(-inc);
    rangeCols_[col].incLast(-inc);
  }
  else
  {
    rangeCols_[col].decLast(inc);
    if (rangeCols_[col].size()==0) freeCol(col);
  }
}
/*@brief Internal method for inserting rows to a specified column.
 *
 *  Insert n rows at the position pos to the column column of the
 *  array. No check is done about the index.
 *  @param col,pos,n column position, row position and number of rows to insert
 **/
template < class  Derived  >
void IArray2D< Derived>::insertRowsCol( int col, int pos, int n)
{
  allocator_.elt(col)->insertElt(pos, n);
  rangeCols_[col].incLast(n);
}
/*@brief Internal method for deleting rows from a specified column.
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
template < class  Derived  >
void IArray2D< Derived>::eraseRowsCol( int col, int pos, int n)
{
  // check trivial cases
  if (rangeCols_[col].lastIdx() < pos) return;
  if (rangeCols_[col].begin()> pos+n-1)
  { shiftRowCol( col, rangeCols_[col].begin() - n); return;}

  // find the existing rows to delete
  Range newRange(pos, n);
  newRange.inf(rangeCols_[col]);
  if (newRange == rangeCols_[col]) { freeCol(col); return;}

  // copy remaining rows
  allocator_.elt(col)->memmove(newRange.begin(), newRange.end(), rangeCols_[col].end() - newRange.end());
  rangeCols_[col].decLast(newRange.size());

  // shift if necessary [if pos<newRange.begin() there is a "padding" to take into account]
  if (pos < newRange.begin())
  { shiftRowCol( col, rangeCols_[col].begin() - (n-newRange.size()));}
}


} // namespace STK



#endif
// STK_IARRAY2DMODIFIERS_H
