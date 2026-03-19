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

/** @file STK_IArray2DSlicers.h
 *  @brief In this file we implement the slicing methods for IArray2D class.
 **/

#ifndef STK_IARRAY2DSLICERS_H
#define STK_IARRAY2DSLICERS_H

namespace STK
{

/*access to a part of a column.
 *  @param j index of the column
 *  @return a reference in the range I of the column j of this
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::Col IArray2D<Derived>::col( int j) const
{
#ifdef STK_BOUNDS_CHECK
       if (beginCols() > j) { STKOUT_OF_RANGE_1ARG(IArray::elt, j, beginCols() > j);}
       if (endCols() <= j)  { STKOUT_OF_RANGE_1ARG(IArray::elt, j, endCols() <= j);}
#endif
  return Col( this->asDerived(), this->rangeRowsInCol(j), j);
}
/*access to a part of a column.
 *  @param I range of the rows
 *  @param j index of the col
 *  @return a reference in the range I of the column j of this
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubCol IArray2D<Derived>::col(Range const& I, int j) const
{
#ifdef STK_BOUNDS_CHECK
  if (beginRows() > I.begin()) { STKOUT_OF_RANGE_2ARG(IArray::col, I, j, beginRows() > I.begin());}
  if (endRows() < I.end())    { STKOUT_OF_RANGE_2ARG(IArray::col, I, j, endRows() <= I.end());}
  if (beginCols() > j) { STKOUT_OF_RANGE_2ARG(IArray::col, I, j, beginCols() > j);}
  if (endCols() <= j)  { STKOUT_OF_RANGE_2ARG(IArray::col, I, j, endCols() <= j);}
#endif
  return SubCol( this->asDerived(), inf(I, this->rangeRowsInCol(j)), j);
}
/*access to many columns.
 *  @param J range of the index of the cols
 *  @return a 2D array containing the Container in the Horizontal range @c J
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubArray IArray2D<Derived>::col(Range const& J) const
{ return SubArray( this->asDerived(), this->rows(), J);}

/*access to a part of a row.
 *  @param i index of the row
 *  @return a reference of the row i.
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::Row IArray2D<Derived>::row( int i) const
{ return Row( this->asDerived(), this->rangeColsInRow(i), i);}
/*access to a part of a row.
 *  @param i index of the row
 *  @param J range of the columns
 *  @return a reference of the row i.
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubRow IArray2D<Derived>::row(int i, Range const& J) const
{ return SubRow( this->asDerived(), inf(J, this->rangeColsInRow(i)), i);}
/*access to many rows.
 *  @param I range of the index of the rows
 *  @return a 2D array containing the Container in the vertical range @c I
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubArray IArray2D<Derived>::row(Range const& I) const
{ return SubArray(this->asDerived(), I, cols());}

/*@return  many elements.
 *  @param J Range of the elements
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubVector IArray2D<Derived>::sub(Range const& J) const
{ return SubVector(this->asDerived(), J);}
/*access to a sub-array.
 *  @param I,J range of the rows and of the columns
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubArray IArray2D<Derived>::sub(Range const& I, Range const& J) const
{ return SubArray(this->asDerived(), I, J);}

// overloaded operators for sub-arrays/vectors
/*@return the sub-vector in given range
 *  @param I range to get
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubVector IArray2D<Derived>::operator[](Range const& I) const
{
  STK_STATIC_ASSERT_ONE_DIMENSION_ONLY(Derived);
  return sub(I);
}
/*@param I range of the index of the rows
 *  @param j index of the column
 *  @return a Vertical container containing the column @c j of this
 *  in the range @c I
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubCol IArray2D<Derived>::operator()(Range const& I, int j) const
{
#ifdef STK_BOUNDS_CHECK
  if (beginRows() > I.begin()) { STKOUT_OF_RANGE_2ARG(IArray::operator(), I, j, beginRows() > I.begin());}
  if (endRows() < I.end())    { STKOUT_OF_RANGE_2ARG(IArray::operator(), I, j, endRows() < I.end());}
  if (beginCols() > j) { STKOUT_OF_RANGE_2ARG(IArrayBase::operator(), I, j, beginCols() > j);}
  if (endCols() <= j)  { STKOUT_OF_RANGE_2ARG(IArrayBase::operator(), I, j, endCols() < j);}
#endif
  return col(I, j);
}
/*@param i index of the row
 *  @param J range of the columns
 *  @return an Horizontal container containing the row @c i of this
 *  in the range @c J
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>::SubRow IArray2D<Derived>::operator()(int i, Range const& J) const { return this->row(i, J);}
/*@param I,J range of the rows and of the columns
 *  @return a 2D container containing this in the range @c I, @c J
 **/
template < class  Derived  >
inline typename hidden::Traits<Derived>:: SubArray IArray2D<Derived>::operator()(Range const& I, Range const& J) const
{ return sub(I, J);}

/*@return the column j.
 *  @param j index of the column
 **/
template < class  Derived  >
typename hidden::Traits<Derived>::SubCol IArray2D<Derived>::atCol(int j) const
{
  if (this->beginCols() > j) { STKOUT_OF_RANGE_1ARG(IArray2D::atCol, j, beginCols() > j);}
  if (this->endCols() <= j)  { STKOUT_OF_RANGE_1ARG(IArray2D::atCol, j, endCols() <= j);}
  return col(j);
}
/*@return the row i.
 *  @param i the index of the row
 **/
template < class  Derived  >
typename hidden::Traits<Derived>::Row IArray2D<Derived>::atRow(int i) const
{
  if (this->beginRows() > i) { STKOUT_OF_RANGE_1ARG(IArray2D::atRow, i, beginRows() > i);}
  if (this->endRows() <= i)  { STKOUT_OF_RANGE_1ARG(IArray2D::at, i, lastIdxRows() < i);}
  return row(i);
}


} // namespace STK

#endif
// STK_IARRAY2DSLICERS_H
