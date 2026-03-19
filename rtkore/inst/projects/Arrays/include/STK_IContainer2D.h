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
 * created on: 10 août 2012
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_IContainer2D.h
 *  @brief In this file we define the IContainer2D interface classes.
 **/

#ifndef STK_ICONTAINER2D_H
#define STK_ICONTAINER2D_H

namespace STK
{

/** @ingroup Arrays
 *  @brief Interface base class for 2D containers.
 *
 *  The IContainer2D class is the base class for all two-dimensional containers
 *  storing data and which cannot be part of an expression.
 *  A two-dimensional container is defined by an horizontal range of index
 *  for the columns and a vertical range of index for the rows.
 *
 *  This Interface base class stores the ranges and allows to derived classes to
 *  manipulate these ranges.
 **/
template<int SizeRows_, int SizeCols_>
class IContainer2D
{
  public:
    /** Type of the Range for the rows */
    typedef TRange<SizeRows_> RowRange;
    /** Type of the Range for the columns */
    typedef TRange<SizeCols_> ColRange;

    /** Default constructor. cols_ = 1:0 and rows_ = 1:0. */
    IContainer2D(): rows_(), cols_() {}
    /** Constructor with specified ranges
     *  @param I,J vertical and horizontal range
     **/
    inline IContainer2D( RowRange const& I, ColRange const& J): rows_(I), cols_(J) {}
    /** Copy constructor
     *  @param T the container to copy
     **/
    inline IContainer2D( IContainer2D const& T): rows_(T.rows_), cols_(T.cols_) {}
    /** destructor. **/
    ~IContainer2D() {}

    /** @return the columns range */
    inline ColRange const& cols() const { return cols_;}
    /** @return the index of the first column */
    inline int beginCols() const { return cols_.begin();}
    /** @return the ending index of the columns */
    inline int endCols() const { return cols_.end();}
    /** @return the number of column */
    inline int sizeCols() const { return cols_.size();}

    /** @return the range of the rows */
    inline RowRange const& rows() const { return rows_;}
    /** @return the index of the first row */
    inline int beginRows() const { return rows_.begin();}
    /** @return the ending index of rows */
    inline int endRows() const { return rows_.end();}
    /** @return the number of rows */
    inline int sizeRows() const { return rows_.size();}

    /** @return the index of the last column */
    inline int lastIdxCols() const { return cols_.lastIdx();}
    /** @return the index of the last row */
    inline int lastIdxRows() const { return rows_.lastIdx();}

    /** @return @c true if the container is empty, @c false otherwise */
    bool empty() const { return (cols_.empty() || rows_.empty());}

  protected:
    /** Set the first index of the rows and columns.
     *  @param rbeg, cbeg the first index of the rows and columns
     **/
    void shift( int rbeg, int cbeg) { rows_.shift(rbeg); cols_.shift(cbeg);}
    /** Set the ranges of the container.
     *  @param I,J the vertical and horizontal range
     **/
    void setRanges(RowRange const& I = RowRange(), ColRange const& J = ColRange())
    { rows_ = I; cols_ =J;}
    // rows
    /** Set the range of the number of rows.
     *  @param I the range of the rows number
     **/
    void setRows( RowRange const& I = RowRange()) { rows_ = I;}
    /** Set the first index of the rows.
     *  @param beg the first index of the rows
     **/
    void shiftRows( int beg) { rows_.shift(beg);}
    /** Increment the range of the number of rows.
     *  @param inc the increment to apply
     **/
    void incRangeRows( int inc) { rows_.inc(inc);}
    /** Increment the first index of the number of rows.
     *  @param inc the increment to apply
     **/
    void incBeginRows( int inc) { rows_.incFirst(inc);}
    /** Decrement the first index of the number of rows.
     *  @param dec the decrement to apply
     **/
    void decBeginRows( int dec) { rows_.decFirst(dec);}
    /** Increment the end of the number of rows.
     *  @param inc the increment to apply
     **/
    void incEndRows( int inc) { rows_.incLast(inc);}
    /** Decrement the end of the number of rows.
     *  @param dec the decrement to apply
     **/
    void decEndRows( int dec) { rows_.decLast(dec);}
    // cols
    /** Set the columns range.
     * @param J the columns range
     **/
    void setCols( ColRange const& J = ColRange()) { cols_ = J;}
    /** Shift the columns first index to beg.
     *  @param beg the new first index
     **/
    void shiftCols( int beg) { cols_.shift(beg);}
    /** Increment the columns range.
     *  @param inc the increment to apply the range
     **/
    void incRangeCols( int inc) { cols_.inc(inc);}
    /** increment the first index of the columns.
     *  @param inc the increment to apply
     **/
    void incBeginCols( int inc) { cols_.incFirst(inc);}
    /** Decrement the columns first index.
     *  @param dec the decrement to apply
     **/
    void decBeginCols( int dec) { cols_.decFirst(dec);}
    /** Increment the last index of the columns.
     *  @param inc the increment to apply
     **/
    void incEndCols( int inc)  { cols_.incLast(inc);}
    /** Decrement the last index of the columns.
     *  @param dec the decrement to apply
     **/
    void decEndCols( int dec) { cols_.decLast(dec);}

    /** exchange this container with T
     *  @param T the container to exchange with this
     **/
     void exchange(IContainer2D& T)
     {
       std::swap(T.rows_, this->rows_ );
       std::swap(T.cols_, this->cols_ );
     }
     /** Increment the end of the number of rows.
      *  @param inc the increment to apply
      **/
     void incLastIdxRows( int inc) { rows_.incLast(inc);}
     /** Decrement the end of the number of rows.
      *  @param dec the decrement to apply
      **/
     void decLastIdxRows( int dec) { rows_.decLast(dec);}
     /** Increment the last index of the columns.
      *  @param inc the increment to apply
      **/
     void incLastIdxCols( int inc)  { cols_.incLast(inc);}
     /** Decrement the last index of the columns.
      *  @param dec the decrement to apply
      **/
     void decLastIdxCols( int dec) { cols_.decLast(dec);}

  private:
    /** Vertical range : Range of the indexes for the rows. */
    RowRange rows_;
    /** Horizontal range : Range of the indexes for the columns. */
    ColRange cols_;
};

} // namespace STK

#endif /* STK_ICONTAINER2D_H */
