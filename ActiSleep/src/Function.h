/*
 *  Function.h
 *  Segments
 *
 *  Created by Michel Koskas on 06/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#ifndef _Function_h_
#define _Function_h_

#include "Sets.h"
#include "MyVector.h"

template <typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
class Function
{
public:
  // false by default in ALL constructors
  bool FirstElementSpecified;
  DataTypeName FirstElement;
  Function();
  ~Function();
  double virtual Min(SetTypeName &S) = 0;
  SetTypeName Roots(const SetTypeName &TheSet);  
  double operator()(const AtomicSetElementTypeName& mu)
  {
    if (FirstElementSpecified)
      return (*this)(FirstElement, mu);
    return (*this)(mu);
  }
  double operator()(const DataTypeName&, const AtomicSetElementTypeName &);
  double operator[](const AtomicSetElementTypeName& mu);  
  SetTypeName *IsLowerThan(SetTypeName &, double);
  void SpecializeMe(DataTypeName Y, int l);
};

template <typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
Function<AtomicSetElementTypeName, DataTypeName, SetTypeName>::Function()
{
  FirstElementSpecified = false;
}


template <typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
void Function<AtomicSetElementTypeName, DataTypeName, SetTypeName>::SpecializeMe(DataTypeName Y, int l)
{
  FirstElement = l* Y;
  FirstElementSpecified = true;
}

template <typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
Function<AtomicSetElementTypeName, DataTypeName, SetTypeName>::~Function()
{
}


template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
class SumOfFunctions
{
public:
  double TheConstant;
  MyVector<FunctionTypeName> MyFunctions;
  double Min(Set<SetTypeName, DataTypeName>);
  SumOfFunctions();
  ~SumOfFunctions();
  SumOfFunctions(FunctionTypeName &f);
  void operator+=(SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> &Other);
  void operator+=(const FunctionTypeName &Other); 
  void operator+=(const double &C);
  SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> & operator=(SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> &Other);
  SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> & operator=(const FunctionTypeName &Other); 
  SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> & operator=(const double &C);
  SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> *operator+(SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> Other);
  SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> *operator+(const FunctionTypeName &Other); 
  SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> *operator+(const double &C);
  double operator()(const AtomicSetElementTypeName &);
  double operator[](const AtomicSetElementTypeName &);
  SetTypeName *IsLowerThan(double);
  SetTypeName *IsLowerThan(double, SetTypeName &);
  
};

template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::SumOfFunctions()
{
  TheConstant= 0;
}

template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::~SumOfFunctions()
{
  // MyFunctions.clear();
}


template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
void SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::operator+=(SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> &Other)
{
  for(typename MyVector<FunctionTypeName>::iterator I = Other.MyFunctions.begin(); I != Other.MyFunctions.end(); I++)
    MyFunctions.push_back(*I);
  TheConstant += Other.TheConstant;  
}

template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> & SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::operator=(SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> &Other)
{
  if (this == &Other)
    return *this;
  MyFunctions.clear();
  for(typename MyVector<FunctionTypeName>::iterator I = Other.MyFunctions.begin(); I != Other.MyFunctions.end(); I++)
    MyFunctions.push_back(*I);
  TheConstant = Other.TheConstant;
  return *this;
}


template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
void SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::operator+=(const FunctionTypeName &Other)
{
  MyFunctions.push_back(Other);
}

template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> & SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::operator=(const FunctionTypeName &Other)
{
  MyFunctions.clear();
  TheConstant = 0;
  MyFunctions.push_back(Other);
  return *this;
}

template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
void SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::operator+=(const double &Other)
{
  TheConstant += Other;
}

template <typename FunctionTypeName, typename AtomicSetElementTypeName, typename DataTypeName, typename SetTypeName>
SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName> & SumOfFunctions<FunctionTypeName, AtomicSetElementTypeName, DataTypeName, SetTypeName>::operator=(const double &Other)
{
  TheConstant = Other;
  MyFunctions.clear();
  return *this;
}

#endif

