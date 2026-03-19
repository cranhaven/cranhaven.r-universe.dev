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

/** @file STK_ITContainer2D.h
 *  @brief In this file we define the ITContainer2D interface class.
 **/

#ifndef STK_ITCONTAINER2D_H
#define STK_ITCONTAINER2D_H


namespace STK
{

/** @ingroup Arrays
 *  @brief Interface base class for homogeneous 2D containers like allocators.
 *
 * The ITContainer2D class is the template base class for all
 * homogeneous two-dimensional containers containing element of type @c Type
 * where Type is note necessarily a scalar. Some methods assume that derived class
 * is not part of an expression and is not constant, so that it can be
 * #- shifted,
 * #- resized
 * #- accessed in modification.
 *
 * Implement the curious recursive template paradigm : the template
 * parameter @c Derived is the name of the class that
 * implements @c ITContainer2D. For example
 * <code>
 * template<class Type>
 * class Derived: public ITContainer2D< Derived<Type> >
 * {...}
 * </code>
 *
 * Functions used in this interface and to implement in
 * derived class if necessary have the following definitions:
 * @code
 *   Type& elt2Impl(int i, int j);
 *   TypeConst elt2Impl(int i, int j) const;
 *   Type& elt1Impl(int pos);
 *   TypeConst elt1Impl(int pos) const;
 *   Type& elt0Impl()
 *   TypeConst elt0Impl(int pos) const;
 *   void shift1Impl(int beg);
 *   void shift2Impl(int beginRows, int beginCols);
 *   Derived& resize1Impl(int size);
 *   resize2Impl(sizeRows, sizeCols);
 * @endcode
 *
 *
 * @sa IContainer2D, ICArray
 *
 * @note The constant getter @c elt1Impl(pos) have to return a reference as we
 * are using derived classes for storing any kind of data.
 **/
template <class Derived>
class ITContainer2D: public IContainer2D< hidden::Traits<Derived>::sizeRows_
                                        , hidden::Traits<Derived>::sizeCols_
                                        >
                   , public IRecursiveTemplate<Derived>
{
  public:
    typedef typename hidden::Traits<Derived>::Type Type;
    typedef typename hidden::Traits<Derived>::TypeConst TypeConst;

    typedef typename hidden::Traits<Derived>::Row Row;
    typedef typename hidden::Traits<Derived>::Col Col;

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

    /** Type of the Base container */
    typedef IContainer2D<sizeRows_, sizeCols_ > Base2D;
    /** Type of the Base container */
    typedef IRecursiveTemplate<Derived> Base;

  protected:
    /** Default constructor.*/
    ITContainer2D(): Base2D(), Base() {}
    /** constructor with specified Range.
     *  @param I,J range of the rows and columns
     **/
    inline ITContainer2D( RowRange const& I, ColRange const& J): Base2D(I, J), Base() {}
    /** Copy constructor.
     *  @param T the container to copy
     **/
    inline ITContainer2D( ITContainer2D const& T): Base2D(T), Base() {}
    /** destructor. */
    ~ITContainer2D() {}

  public:
    /** @return the element (i,j) of the 2D container.
     *  @param i, j indexes of the element to get
     **/
    inline Type& elt(int i, int j)
    {
#ifdef STK_BOUNDS_CHECK
      if (this->beginRows() > i) { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, beginRows() > i);}
      if (this->endRows() <= i)  { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, endRows() <= i);}
      if (this->beginCols() > j) { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, beginCols() > j);}
      if (this->endCols() <= j)  { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, endCols() <= j);}
#endif
      return this->asDerived().elt2Impl(i,j);
    }
    /** @return a constant reference on element (i,j) of the 2D container
     *  @param i, j indexes of the element to get
     **/
    inline TypeConst elt(int i, int j) const
    {
#ifdef STK_BOUNDS_CHECK
      if (this->beginRows() > i) { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, beginRows() > i);}
      if (this->endRows() <= i)  { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, endRows() <= i);}
      if (this->beginCols() > j) { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, beginCols() > j);}
      if (this->endCols() <= j)  { STKOUT_OF_RANGE_2ARG(ITContainer2D::elt, i, j, endCols() <= j);}
#endif
      return this->asDerived().elt2Impl(i,j);
    }
    /** @return a reference on the ith element
     *  @param i index of the element to get
     **/
    inline Type& elt(int i) { return this->asDerived().elt1Impl(i);}
    /** @return the constant ith element
     *  @param i index of the element to get
     *  @note bounds check cannot be done there as number_ does not have
     *  begin() and end() implemented
     **/
    inline TypeConst elt(int i) const { return this->asDerived().elt1Impl(i);}
    /** @return a reference on the number */
    inline Type& elt() { return this->asDerived().elt0Impl();}
    /** @return a constant reference on the number */
    inline TypeConst elt() const  { return this->asDerived().elt0Impl();}

    /** resize the container
     *  @param sizeRows, sizeCols size of the rows and columns
     **/
    Derived& resize(int sizeRows, int sizeCols)
    { return this->asDerived().resize2Impl(sizeRows, sizeCols);}
    /** Resize 1D container
     *  @param size the size to set to the vector
     **/
    Derived& resize(int size) { return this->asDerived().resize1Impl(size);}

    /** shift the first indexes of the container
     *  @param firstRow, firstCol indexes of the first row and first column
     **/
    void shift( int firstRow, int firstCol) { this->asDerived().shift2Impl(firstRow, firstCol);}
    /** shift the first indexes of the 1D container
     *  @param beg the index of the first row or column
     **/
    void shift(int beg) { this->asDerived().shift1Impl(beg);}
};

} // namespace STK

#endif /* STK_ITCONTAINER2D_H */
