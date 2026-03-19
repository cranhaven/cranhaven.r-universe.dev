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
 * created on: 14 déc. 2011
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_ArraysTraits.h
 *  @brief In this file we define the main traits class we use for the
 *  STK++ Containers.
 **/


#ifndef STK_ARRAYSTRAITS_H
#define STK_ARRAYSTRAITS_H

namespace STK
{

namespace hidden
{
/** @ingroup hidden, Arrays
 *  @brief The traits struct Traits must be specialized for any
 *  container derived from the interface classes STK::ITContainer, STK::ITContainer1D
 *  and STK::ITContainer2D.
 *
 *  The enumerated values and type names defined in this trait struct are as
 *  follow (example taken from the STK::Array1D class)
 *  @code
 *  template<class Type_, int Size_>
 *  struct Traits< Array1D<Type_, Size_> >
 *  {
 *     typedef Array1D<Type_, 1> Row;
 *     typedef Array1D<Type_, Size_> Col;
 *     typedef Array1D<Type_, UnknownSize> SubRow;
 *     typedef Array1D<Type_, UnknownSize> SubCol;
 *     typedef Array1D<Type_, UnknownSize> SubArray;
 *     typedef Array1D<Type_, UnknownSize> SubVector;
 *
 *     typedef Type_ Type;
 *     typedef typename RemoveConst<Type>::Type const& TypeConst;
 *
 *     enum
 *     {
 *       structure_ = Arrays::vector_,
 *       orient_    = Arrays::by_col_,
 *       size_      = Size_,
 *       sizeCols_  = 1,
 *       sizeRows_  = Size_,
 *       storage_   = Arrays::dense_ // always dense
 *     };
 *
 *     // optional
 *     typedef DenseRandomIterator<Array1D<Type_, Size_> > Iterator;
 *     typedef ConstDenseRandomIterator<Array1D<Type_, Size_> > ConstIterator;
 *
 *     typedef std::reverse_iterator<Iterator> ReverseIterator;
 *     typedef std::reverse_iterator<ConstIterator> ConstReverseIterator;
 *  };
 *  @endcode
 *  @sa STK::Array1D, STK::Array2D, STK::CArray, STK::List1D
 */
template <typename Derived> struct Traits;

/** @ingroup hidden, Array
 *  @brief Traits class to get the correct returned Structure, Type, allocator,...
 *  of operator*. This traits struct is used by the functors classes operating
 *  on the STK::Array2D, STK::CArray,... classes.
 *
 *  @note Impossible products are tracked in ArrayByArrayProduct class.
 **/
template<typename Lhs, typename Rhs, int LStructure_, int RStructure_>
struct ProductTraits;

/** @ingroup hidden, Arrays
 *  Utility class that will select the type of operator to apply.
 *  The result can be either a number if the data are in a vector or a point,
 *  or a vector if the data are in a matrix
 **/
template<typename Derived, template<class> class Functor>
struct FunctorTraits;

/** @ingroup hidden, Arrays
 *  @brief The traits struct IteratorTraits must be specialized for any iterator
 *  derived from the base classes STK::IteratorBase and STK::InnerOperatorBase.
 *
 *  We use the type names defined by the STL for the iterator_traits class.
 *
 *  For example:
 *  @code
 *  template<typename Type>
 *  struct IteratorTraits
 *  {
 *    /// One of the iterator_tags types
 *    typedef std::random_access_iterator_tag  iterator_category;
 *    /// The type "pointed to" by the iterator.
 *    typedef Type        value_type;
 *    /// Distance between iterators is represented as this type.
 *    typedef int  difference_type;
 *    /// This type represents a pointer-to-value_type.
 *    typedef Type*   pointer;
 *    /// This type represents a reference-to-value_type.
 *    typedef Type& reference;
 *  };
 *  @endcode
 */
template <typename Derived> struct IteratorTraits;

} // namespace hidden

} // namespace STK

#endif /* STK_ARRAYSTRAITS_H */
