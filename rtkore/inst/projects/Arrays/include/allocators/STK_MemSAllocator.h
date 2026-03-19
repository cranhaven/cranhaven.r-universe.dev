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

/** @file STK_MemSAllocator.h
 *  @brief In this file we define and implement the MemSAllocator class
 **/



#ifndef STK_MEMSALLOCATOR_H
#define STK_MEMSALLOCATOR_H

#include "../STK_Array1D.h"


namespace STK
{
// forward declaration
template< typename Type_, int NzMax_ = UnknownSize, int Size_ = UnknownSize>
class MemSAllocator;

/** @ingroup Arrays
 *  @brief memory allocator for sparse Array classes.
 *  The data are stored either in the compressed sparse row (CSR) format
 *  or compressed sparse column (CSC) format. This Allocator does not assume
 *  any orientation and can be used for both kind of storage.
 *
 *  @tparam Type_ type of elements stored in this allocator
 *  @tparam NzMax_ maximal number of element in sparse matrix
 *  @tparam Size_ size (if known): number of rows (columns)
 *
 */
template< typename Type_, int NzMax_, int Size_>
class MemSAllocator: public IContainerRef
{
  public:
    enum
    {
      nzmax_ = NzMax_,
      size_  = (Size_< (UnknownSize-1)) ? Size_+1 : UnknownSize
    };

    typedef Type_  Type;
    typedef typename hidden::RemoveConst<Type_>::Type const& TypeConst;
    /** values stored by pair (row/column index, value) */
    typedef std::pair<int, Type> IndexedValue;

    using IContainerRef::isRef;
    using IContainerRef::setRef;

    /** Type of the base allocator allocating data. Can be of fixed size NzMax_ */
    typedef Array1D<IndexedValue, NzMax_> Allocator;
    /** Type of the base allocator allocating index pointer. Can be of fixed size Size_ */
    typedef Array1D<int, size_> PtrIdx;

    //typedef TRange<NzMax_> AllocatorRange;
    typedef TRange<Size_> AllocatorRange;

    /** default constructor */
    MemSAllocator(): IContainerRef(false)
                   , ptr_(incLast(Range()))
                   , idx_()
                   , zero_(0)
    {
      ptr_ = baseIdx;
      idx_ = IndexedValue(Arithmetic<int>::NA(), zero_);
    }
    /** constructor with specified dimension
     *  @param I range of the rows (or columns)
     **/
    MemSAllocator( AllocatorRange const& I): IContainerRef(false)
                                           , ptr_(incLast(I))
                                           , idx_()
                                           , zero_(0)
    {
      ptr_ = baseIdx;
      idx_ = IndexedValue(Arithmetic<int>::NA(), zero_);
    }
    /** copy constructor
     *  @param A allocator to copy
     *  @param ref @c true if this copy is just a reference, @c false otherwise
     **/
    MemSAllocator( MemSAllocator const& A, bool ref =false)
                 : IContainerRef(ref)
                 , ptr_(A.ptr_, ref)
                 , idx_(A.idx_, ref)
                 , zero_(A.zero_)
    {}
    /** reference constructor
     *  @param A allocator to copy
     *  @param I range of the rows/columns to reference
     **/
    MemSAllocator( MemSAllocator const& A, Range const& I)
                 : IContainerRef(true)
                 , ptr_(A.ptr_, I, true)
                 , idx_(A.idx_, true)
                 , zero_(A.zero_)
    {}

    // getters
    /** @return the first index of the data. */
    inline int begin() const { return ptr_.begin();}
    /**@return the ending index of the data */
    inline int end() const { return ptr_.end()-1;}
    /** @return the size of the data */
    inline int size() const { return ptr_.size()-1;}

    /** @return the vector with the pointers on rows (or columns) */
    inline PtrIdx const& ptr() const { return ptr_;}
    /** @return the vector with the (index,value) pairs columns (or rows) */
    inline Allocator const& idx() const { return idx_;}

    // setters
    /** @param ptr vector with the pointers on rows (or columns) */
    inline void setPtr(PtrIdx const& ptr) { ptr_ = ptr;}
    /** @param idx vector with the (index,value) pairs in columns (or rows) */
    inline void setIdx(Allocator const& idx) { idx_ = idx;}
    /** @param ptr vector with the pointers on rows (or columns)
     *  @param idx vector with the (index,value) pairs in columns (or rows)
     **/
    inline void set(PtrIdx const& ptr, Allocator const& idx)
    { ptr_ = ptr; idx_ = idx;}

    // manipulator
    /** This method allows to get the element (p_idx, s_idx)
     *  @param p_idx the index of the row (or column)
     *  @param s_idx the index of the column (or row)
     *  @return 0 if the element is not stored, the value of the element otherwise
     **/
    TypeConst getValue(int p_idx, int s_idx) const;
    /** This method allows to overwrite or insert an element to the position (p_idx,  s_idx)
     *  @param p_idx index of the row (respectively column)
     *  @param s_idx index of the column (respectively row)
     *  @param value value to set
     **/
    void addValue(int p_idx, int s_idx, Type const& value);

  protected:
    /** array of pointer */
    PtrIdx ptr_;
    /** array of pair(idx, value) */
    Allocator idx_;

  private:
    /** zero value */
    const Type zero_;
};


/* This method allows to get the element (p_idx, s_idx)
 *  @param p_idx the index of the row (or column)
 *  @param s_idx the index of the column (or row)
 *  @return 0 if the element is not stored, the value of the element otherwise
 **/
template< typename Type_, int NzMax_, int Size_>
typename MemSAllocator<Type_, NzMax_, Size_>::TypeConst MemSAllocator<Type_, NzMax_, Size_>::getValue(int p_idx, int s_idx) const
{
  for (int t=ptr_[p_idx]; t<ptr_[p_idx+1]; ++t)
  { if (idx_[t].first == s_idx) return idx_[t].second;}
  return zero_;
}

/* This method allows to overwrite or insert an element to the position (p_idx,  s_idx)
 *  @param p_idx index of the row (respectively column)
 *  @param s_idx index of the column (respectively row)
 *  @param value value to set
 **/
template< typename Type_, int NzMax_, int Size_>
void MemSAllocator<Type_, NzMax_, Size_>::addValue(int p_idx, int s_idx, Type const& value)
{
  // loop over already entries in this row/column
  for (int t=ptr_[p_idx]; t<ptr_[p_idx+1]; ++t)
  {
    if (idx_[t].first == s_idx) // there is an existing stored value for this entry
    {
      if (value != zero_) { idx_[t].second = value;}
      else // value to enter is zero
      {
        idx_.erase(t);
        for (int pt = p_idx+1; pt<ptr_.end(); ++pt) { --ptr_[pt];}
      }
      return;
    }
    else if (idx_[t].first > s_idx) // value is not yet an entry, so add it
    {
      if (value == zero_) return; // value is zero, there is nothing to do
      // Otherwise insert it
      idx_.insert(t, IndexedValue(s_idx, value));
      for (int pt = p_idx+1; pt<ptr_.end(); ++pt) { ++ptr_[pt];}
      return;
    }
  }
  // No entry in this row/column, if value is zero there is nothing to do
  if (value == zero_) return;
  // otherwise add it taking care of empty rows/columns
  idx_.insert( (ptr_[p_idx] == ptr_[p_idx+1]) ? ptr_[p_idx] : ptr_[p_idx]+1, IndexedValue(s_idx, value));
  for (int pt = p_idx+1; pt<ptr_.end(); ++pt) { ++ptr_[pt];}
}

} // namespace STK

#endif /* STK_MEMSALLOCATOR_H */
