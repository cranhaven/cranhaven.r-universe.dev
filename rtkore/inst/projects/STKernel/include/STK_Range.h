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

    Contact: S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
*/

/*
 * Project:  STKernel::Base
 * Purpose:  Define the Range class.
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_Range.h
 *  @brief In this file we define the class Range, a range of Index
 *  denoting a sub-vector region.
 **/

#ifndef STK_RANGE_H
#define STK_RANGE_H

#include <map>

/** @ingroup Sdk
 *  @brief base index of the containers created in STK++.
 *  This value means that the default range for a vector or the rows/columns of
 *  a matrix is the value given by this constant. **/
#if defined(STKBASEARRAYS)
const int firstIdx_ = STKBASEARRAYS;
#else
const int firstIdx_ = 0; // default is 0 based array
#endif


/** Utility macro that can be used in a similar way that first:last */
#define _R(first,last) Range(first, last, 0)

namespace STK
{
// forward declaration
template<int Size_> class TRange;
typedef TRange<UnknownSize> Range;

template<int Size_>
ostream& operator<< (ostream& s, TRange<Size_> const& I);

/** @ingroup STKernel
 *  @brief Base class for TRange class
 **/
template<class Derived>
class RangeBase: public IRecursiveTemplate<Derived>
{
  protected:
    /** Default constructor*/
    inline RangeBase(): begin_(firstIdx_) {}
    /** constructor.
     * @param begin beginning of range
     **/
    inline RangeBase( int begin): begin_(begin) {}
    /** Copy constructor.
     * @param I range to copy
     **/
    inline RangeBase( RangeBase const& I): begin_(I.begin_) {}
    /** Copy constructor.
     *  @param I range to copy
     **/
    template<class OtherDerived>
    inline RangeBase( RangeBase<OtherDerived> const& I): begin_(I.begin()) {}

  public:
    /** destructor. */
    inline ~RangeBase() {}
    /** get the first index of the TRange.
     *  @return the first index of the range
     **/
    inline int begin() const { return begin_;};

    /** check if this TRange in include in an other TRange
     *  @param I the index to compare
     *  @return @c true if this is include in @c I, @c false otherwise
     **/
    template<int OtherSize_>
    inline bool isIn(TRange<OtherSize_> const& I) const
    { return ((this->asDerived().begin() >= I.begin()) && (this->asDerived().end() <= I.end()));}
    /** check if the TRange I is include in the this TRange
     *  @param I the range to compare
     *  @return @c true if this contain I, @c false otherwise
     **/
    template<int OtherSize_>
    inline bool isContaining(TRange<OtherSize_> const& I) const
    { return ((begin_<= I.begin())&&(this->asDerived().end()>=I.end()));}
    /** Return true if i is in this TRange
     *  @param i the integer to compare
     *  @return @c true if i is in this, @c false otherwise
     **/
    inline bool isContaining(int i) const { return ((begin_<=i)&&(i<this->asDerived().end()));}

    /** shift range a:b becomes (a+i):(b+i)
     *  @param i shift to apply
     *  @return @c true if the range are equals, @c false otherwise
     **/
    inline Derived& operator+=(int i) { return this->asDerived().inc(i);}
    /** shift range a:b becomes (a+i):(b+i)
     *  @param i shift to apply
     *  @return @c true if the range are equals, @c false otherwise
     **/
    inline Derived& operator-=(int i) { return this->asDerived().dec(i);}

    /** compare this range with range @c I
     *  @param I the Index to compare
     *  @return @c true if the range are equals, @c false otherwise
     **/
    template<int OtherSize_>
    inline bool operator==(TRange<OtherSize_> const& I) const
    { return ((begin_ == I.begin()) && (this->asDerived().end() == I.end()));}
    /** compare this range with range @c I
     *  @param I the Index to compare
     *  @return @c true if the range are different, @c false otherwise
     **/
    template<int OtherSize_>
    inline bool operator!=(TRange<OtherSize_> const& I) const
    { return ((begin_ != I.begin()) || (this->asDerived().end() != I.end()));}

 protected:
    int begin_; ///< First index
};

/** @ingroup STKernel
 *  @brief Index sub-vector region with fixed size.
 *
 *  A TRange is an ordered pair [first,end) denoting a sub-vector
 *  region, similar to a Fortran 90 or Matlab colon notation.
 *  For example:
 *  @code
 *    Vector A(10), B(Range(0,20));
 *    Range I(2,4, false);
 *    A(I) = B(Range(0,2, false));
 *  @endcode
 *  overwrites the elements (2, 3, 4) of A by the elements (0, 1, 2) of B.
 *  There is no stride argument, only contiguous regions are allowed.
 **/
template<int Size_>
class TRange: public RangeBase< TRange<Size_> >
{
  public:
    typedef RangeBase< TRange<Size_> > Base;
    using Base::begin_;

    /** Default constructor. Give the size of the sub-region.
     *  @param size size of the sub-region (size is not used in fixed size range)
     **/
    inline TRange( int size = Size_): Base() {}
    /** Full constructor. Give the beginning and the size of the sub-region.
     *  @param first beginning of the sub-region
     **/
    inline TRange( int first, int ): Base(first) {}
    /** Complete constructor. Give the beginning and the last indexes of the sub-region.
     *  @param first first index of the sub-region
     **/
    inline TRange( int first, int , bool ): Base(first) {}
    /** copy constructor
     *  @param I Range to copy
     **/
    inline TRange(TRange const& I): Base(I) {}
    /** copy constructor
     *  @param I Range to copy
     **/
    template<int OtherSize_>
    inline TRange(TRange<OtherSize_> const& I): Base(I) {}
    /** destructor. */
    inline ~TRange() {}
    /** get the ending index of the TRange.
     *  @return the end index of the range
     **/
    inline int end() const { return begin_ + Size_;};
    /** get the size of the TRange (the number of elements).
     *  @return the size of the range
     **/
    inline int size() const { return Size_;};
    /** check if the range is empty or not.
     *  @return @c true if size <=0, @c false otherwise
     */
    inline bool empty() const { return Size_<=0;};

    // backward compatibility
    /** get the last index of the TRange.
     *  @return the last index of the range
     **/
    inline int lastIdx() const { return begin_ + Size_ -1;};

    /** Shift the TRange giving the first element.
     *  @param begin new value of the first element. */
    inline TRange& shift(int begin) { begin_= begin; return *this;}
    /** create the TRange [begin_+inc, end_+inc_].
     *  @param inc the increment to apply
     **/
    inline TRange& inc(int inc =1) { begin_ +=inc; return *this;}
    /** create the TRange [begin_-dec, end_-dec]
     *  @param dec the decrement to apply
     **/
    inline TRange& dec(int dec=1) { begin_ -=dec; return *this;}

    /** create the TRange [begin_+inc, end_].
     *  @param inc the increment to apply
     **/
    inline TRange& incFirst(int inc=1) { begin_ +=inc; return *this;}
    /** @brief create the TRange [begin_-dec, end_]
     * @param dec the decrement to apply
     **/
    inline TRange& decFirst(int dec=1) { begin_ -=dec;  return *this;}

    /** create the TRange [begin_, end_+inc)
     *  @param inc the increment to apply
     **/
    inline TRange& incEnd(int inc =1) { return *this;}
    /** create the TRange [begin_, end_-dec)
     *  @param dec the decrement to apply
     **/
    inline TRange& decEnd(int dec =1){ return *this;}

    // backward compatibility
    /** create the TRange [begin_, end_+inc)
     *  @param inc the increment to apply
     **/
    inline TRange& incLast(int inc =1) { return *this;}
    /** create the TRange [begin_, end_-dec)
     *  @param dec the decrement to apply
     **/
    inline TRange& decLast(int dec =1){ return *this;}
};

/** @ingroup STKernel
 *  @brief Index sub-vector region: Specialization when the size is unknown.
 *
 *  A Range is an ordered pair [first,last] denoting a sub-vector
 *  region, similar to a Fortran 90 or Matlab colon notation.
 *  For example :
 *  @code
 *  Vector A(10), B(Range(0,20));
 *  Range I(2,4, false);
 *  A(I) = B(Range(0,2, false));
 *  @endcode
 *  overwrite the elements (2, 3, 4) of A by the elements (0, 1, 2) of B.
 *  There is no stride argument, only contiguous regions are allowed.
 **/
template<>
class TRange<UnknownSize>: public RangeBase< TRange<UnknownSize> >
{
  public:
    typedef RangeBase<TRange<UnknownSize> > Base;
    using Base::begin_;

    /** constructor. By default the first index is defined by the firstIdx macro.
     *  @param size size of the sub-region
     **/
    inline TRange( int size =0): Base(), size_(size) {}
    /** Complete constructor. Give the beginning and the size of the sub-region.
     *  @param first, size beginning and size of the range
     **/
    inline TRange( int first, int size): Base(first), size_(size) {}
    /** Complete constructor. Give the first and last index of the sub-region.
     *  @param first, last first and last indexes of the sub-region
     *  @param junk allow to use the constructor (begin:last) rather than (begin,size)
     **/
    inline TRange( int first, int last, bool junk): Base(first), size_(last+1-first) {}
    /** @brief Copy constructor.
     *  Create a copy of an existing TRange.
     *  @param I range to copy
     **/
    inline TRange(TRange const& I): Base(I), size_(I.size_) {}
    /** @brief Copy constructor.
     *  Create a copy of an existing TRange.
     *  @param I range to copy
     **/
    template<int OtherSize_>
    inline TRange(TRange<OtherSize_> const& I): Base(I), size_(I.size()) {}
    /** destructor. */
    inline ~TRange() {}
    /** get the ending index of the TRange.
     *  @return the first index of the range
     **/
    inline int end()  const { return begin_+ size_;}
    /** get the size of the TRange (the number of elements).
     *  @return the size of the range
     **/
    inline int size()   const { return size_;}
    /** check if the range is empty or not.
     *  @return @c true if size <=0, @c false otherwise
     */
    inline bool empty() const { return size_<=0;}

    // backward compatibility
    /** get the last index of the TRange.
     *  @return the last index of the range
     **/
    inline int lastIdx() const { return(end()-1);}

    /** Shift the TRange giving the first element: the size is not modified.
     *  @param first new value of the first element. */
    inline TRange& shift(int first) { return inc(first - begin_);}
    /** create the TRange [begin_+inc, end_+inc_).
     *  @param inc the increment to apply
     **/
    inline TRange& inc(int inc =1){ begin_ +=inc; return *this;}
    /** create the TRange [begin_+inc, end_).
     *  @param dec the decrement to apply
     **/
    inline TRange& dec(int dec =1) { begin_ -=dec; return *this;}
    /** create the TRange [begin_-dec, end_-dec)
     *  @param inc the increment to apply to begin_
     **/

    inline TRange& incFirst(int inc =1) { begin_ +=inc; size_ -=inc; return *this;}
    /** @brief create the TRange [begin_-dec, end_)
     * @param dec the decrement to apply
     **/
    inline TRange& decFirst(int dec =1) { begin_ -=dec; size_ +=dec; return *this;}

    /** create the TRange [begin_, end_+inc)
     *  @param inc the increment to apply
     **/
    inline TRange& incEnd(int inc =1) { size_ +=inc; return *this;}
    /** create the TRange [begin_, end_-dec)
     *  @param dec the decrement to apply
     **/
    inline TRange& decEnd(int dec =1){ size_ -=dec; return *this;}

    // backward compatibility
    /** create the TRange [begin_, end_+inc)
     *  @param inc the increment to apply
     **/
    inline TRange& incLast(int inc =1) { size_ +=inc; return *this;}
    /** create the TRange [begin_, end_-dec)
     *  @param dec the decrement to apply
     **/
    inline TRange& decLast(int dec =1){ size_ -=dec; return *this;}

    /** Take the lowest value of begin_ and I.begin_ for begin_
     *  and the largest value of end_ and I.end_ for end_.
     *  @param I the index to apply
     **/
    template<int OtherSize_>
    inline TRange& sup(TRange<OtherSize_> const& I)
    {
      begin_  = std::min(begin_, I.begin());
      size_   = std::max(end(), I.end()) - begin_;
      return *this;
    }
    /** Take the largest value of begin_ and I.begin_ for begin_
     *  and the lowest value of end_ and I.end_ for end_.
     *  @param I the index to apply
     **/
    template<int OtherSize_>
    inline TRange& inf(TRange<OtherSize_> const& I)
    {
      begin_ = std::max(begin_, I.begin());
      size_  = std::min(end(), I.end()) - begin_;
      return *this;
    }

    /** @brief Read a Range in the form first:last (MATLAB-like form) from
     *  an input stream. The input stream can also be a number (say n).
     *  In this case the range will be n:n. If the range cannot be read the
     *  method return a NA value
     *  @param is the input stream
     *  @param I the range to set
     *  @return is stream
     **/
    friend istream& operator>> (istream& is, Range& I);

  private:
    int size_;     ///< theoretic Dimension size_ = end_- begin_

};

/** @ingroup Arithmetic
 *  @brief Partial Specialization for TRange.
 *  NA (not available) numbers is part of the TRange.
 */
template<int Size_>
struct Arithmetic< TRange<Size_> >: public std::numeric_limits< TRange<Size_> >
{
  /** Adding a Non Available (NA) special number. */
  static inline  TRange<Size_> NA() throw()
  { return TRange<Size_>(std::numeric_limits<int>::min(), std::numeric_limits<int>::min(), 0);}
  /** True if the type has a representation for a "Not Available". */
  static const bool hasNA = true;
  /** Test if x is a Non Available (NA) special number
   *  @param x the Binary number to test.
   **/
  static inline bool isNA(TRange<Size_> const& x) throw()
  { return (x.begin() == std::numeric_limits<int>::min());}
  /** test if x is  infinite.
   *  @param x the Binary number to test.
   **/
  static inline bool isInfinite(TRange<Size_> const& x) throw() { return false; }
  /** test if x is  finite.
   *  @param x the Binary number to test.
   **/
  static inline bool isFinite(TRange<Size_> const& x) throw() { return (!isNA(x) && !isInfinite(x));}
};

/** @ingroup RTTI
 *  @brief Partial Specialization of the IdTypeImpl for the Type TRange.
 **/
template<int Size_>
struct IdTypeImpl< TRange<Size_> >
{
  /** @return the IdType of the type TRange. */
  static inline Base::IdType returnType() { return(Base::range_);}
};


inline istream& operator>> (istream& is, Range& I)
{
  String str;
  std::getline(is, str, _T(':'));
  // get first number
  if (!stringToType(I.begin_, str))
  {
    I = Arithmetic<Range>::NA();
    is.clear(); is.setstate(std::ios::failbit);
    return is;
  }
  // check if the istream is exhausted
  if (is.eof())
  {
    I.size_ = 1;
    return is;
  }
  // skip the current char ":"
  is.peek();
  // get second number
  if ((is >> I.size_).fail())
  {
    I = Arithmetic<Range>::NA();
    is.clear(); is.setstate(std::ios::failbit);
    return is;
  }
  else { I.size_ -= I.begin_ ;}
  return is;
}

/** @ingroup STKernel
 *  @brief compute sup(I,J).
 *  Take the lowest value of I.begin() and J.begin() for begin
 *  and the largest value of I.end() and J.end() for end.
 *  @param I,J the first and second Range
*/
template<int SizeI_, int SizeJ_>
Range sup(TRange<SizeI_> const& I, TRange<SizeJ_> const& J)
{ return Range(std::min(I.begin(), J.begin()), std::max(I.lastIdx(), J.lastIdx()), 0);}
/** @ingroup STKernel
 *  @brief compute inf(I,J).
 *  Take the largest value of I.begin() and J.begin() for begin
 *  and the lowest value of I.end() and J.end() for end.
 *  @param I,J the first and second Range
 */
template<int SizeI_, int SizeJ_>
Range inf(TRange<SizeI_> const& I, TRange<SizeJ_> const& J)
{ return Range(std::max(I.begin(), J.begin()), std::min(I.lastIdx(), J.lastIdx()), 0);}
/** @ingroup STKernel
 *  @brief if I=a:b, return a+1:b
 *  @return range I with first index + 1
*/
template<int SizeI_>
Range incFirst(TRange<SizeI_> const& I)
{ return Range(I.begin()+1, I.lastIdx(), 0);}
/** @ingroup STKernel
 *  @brief if I=a:b, return a:b+1
 *  @return range I with first index + 1
*/
template<int SizeI_>
Range incLast(TRange<SizeI_> const& I)
{ return Range(I.begin(), I.lastIdx()+1, 0);}
/** @ingroup STKernel
 *  @brief if I=a:b, return a-1:b
 *  @return range I with first index + 1
*/
template<int SizeI_>
Range decFirst(TRange<SizeI_> const& I)
{ return Range(I.begin()-1, I.lastIdx(), 0);}
/** @ingroup STKernel
 *  @brief if I=a:b, return a:b-1
 *  @return range I with first index + 1
*/
template<int SizeI_>
Range decLast(TRange<SizeI_> const& I)
{ return Range(I.begin(), I.lastIdx()-1, 0);}

/** @ingroup Base
 *  @brief Write a TRange in the form first:last (MATLAB-like form) in an output stream.
 *  @param os output stream
 *  @param I Range to write
 *  @return output stream
 **/
template<int Size_>
ostream& operator<< (ostream& os, TRange<Size_> const& I)
{
  (Arithmetic< TRange<Size_> >::isNA(I)) ? os <<  stringNa
                                         : os <<  I.begin() << _T(":") << I.lastIdx();
  return os;
}

/** @ingroup Base
 *  @brief Convert a String to a Range.
 *  @param str the String we want to convert
 *  @return the Range represented by the String @c str. if the string
 *  does not match any known name, the NA value is returned.
 **/
inline Range stringToRange( String const& str)
{ return stringToType<Range>(str);}

/** @ingroup Base
 *  Convert a String to a Range using a map.
 *  @param str the String we want to convert
 *  @param mapping the mapping between the string and the TRange
 *  @return the TRange represented by the String @c str. if the string
 *  does not match any known name, the NA value is returned.
 **/
template<int Size_>
TRange<Size_> stringToRange( String const& str, std::map<String, TRange<Size_> > const& mapping)
{
  typename std::map<String, TRange<Size_> >::const_iterator it=mapping.find(str);
  return (it == mapping.end()) ? Arithmetic< TRange<Size_> >::NA() : it->second;
}

/** @ingroup Base
 *  Convert an Range to a String.
 *  @param value the Range we want to convert
 *  @param f format, by default write every number in decimal
 *  @return the string associated to this value.
 **/
template<int Size_>
String rangeToString( TRange<Size_> const& value, std::ios_base& (*f)(std::ios_base&) = std::dec)
{
  if (Arithmetic<TRange<Size_> >::isNA(value)) return stringNa;
  ostringstream os;
  os << f << value;
  return os.str();
}

/** @ingroup Base
 *  Convert a Range to a String.
 *  @param value the Range we want to convert
 *  @param mapping the mapping between Range and String
 *  @return the String associated to this value.
 **/
template<int Size_>
String rangeToString( TRange<Size_> const& value, std::map<TRange<Size_> , String> const& mapping)
{
  typename std::map<TRange<Size_> , String>::const_iterator it=mapping.find(value);
  if (it == mapping.end())  return stringNa;
  return it->second;
}

/** @ingroup Base
 *  @brief Specialization for Range
 *  @param t The Range to convert to String
 *  @param f format, by default write every number in decimal
 **/
template<>
inline String typeToString<Range >( Range const& t, std::ios_base& (*f)(std::ios_base&))
{ return rangeToString(t, f);}


} // namespace STK

#endif // STK_RANGE_H
