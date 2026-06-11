/*
 *  Sets.h
 *  Segments
 *
 *  Created by Michel Koskas on 13/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#ifndef _Sets_h_
#define _Sets_h_

#include "MyVector.h"


class AtomicSet
{
public:
  AtomicSet();
  virtual ~AtomicSet() = 0;
};

template<typename AtomicSetTypeName, typename DataTypeName>
class Set
{
private:
  MyVector<AtomicSetTypeName> Complementaires;
  MyVector<AtomicSetTypeName> IntersectionsSets;
//getter and setter
protected:
public:
  void PushInIntersectionsSets(AtomicSetTypeName &S)
  {
    IntersectionsSets.push_back(S);
  }
  void PushInComplementaires(AtomicSetTypeName &S)
  {
    Complementaires.push_back(S);
  }
  void Initialize(DataTypeName, DataTypeName);
  void SetComplementaires(MyVector<AtomicSetTypeName> &C){Complementaires = C;}
  void SetIntersectionsSets(MyVector<AtomicSetTypeName> &I){IntersectionsSets = I;}
  void SetIntersectionsSets(AtomicSetTypeName &I){IntersectionsSets.clear(); IntersectionsSets.push_back(I);}
  MyVector<AtomicSetTypeName> &GetComplementaires();
  MyVector<AtomicSetTypeName> &GetIntersectionsSets();
  bool Empty();
  Set();
  Set(bool EmptySet);
  Set<AtomicSetTypeName, DataTypeName> *Intersect(AtomicSetTypeName &);
  void SelfIntersect(AtomicSetTypeName &);
  Set<AtomicSetTypeName, DataTypeName> *Intersect(Set<AtomicSetTypeName, DataTypeName> &);
  void SelfIntersect(Set<AtomicSetTypeName, DataTypeName> &);
  Set(MyVector<AtomicSet>);
  Set(MyVector<AtomicSet>, MyVector<AtomicSet>);
  Set(Set &);
  // ~Set();
  void AddInComplementaires(AtomicSetTypeName &S)
  {
    Complementaires.push_back(S);
  }
  void AddInIntersectionsSets(AtomicSetTypeName &S)
  {
    IntersectionsSets.push_back(S);
  }
  Set<AtomicSetTypeName, DataTypeName> operator=(Set<AtomicSetTypeName, DataTypeName> &Other)
  {
    if (this != &Other)
    {
      SetComplementaires(Other.GetComplementaires());
      SetIntersectionsSets(Other.GetIntersectionsSets());
    }
    return *this;
  }
  void SetMe(Set<AtomicSetTypeName, DataTypeName> &S);
};

template <typename AtomicSetTypeName, typename DataTypeName>
void Set<AtomicSetTypeName, DataTypeName>::SetMe(Set<AtomicSetTypeName, DataTypeName> &S)
{
  *this = S;
}


template <typename AtomicSetTypeName, typename DataTypeName>
MyVector<AtomicSetTypeName> &Set<AtomicSetTypeName, DataTypeName>::GetComplementaires()
{
  return Complementaires;
}

template <typename AtomicSetTypeName, typename DataTypeName>
MyVector<AtomicSetTypeName> &Set<AtomicSetTypeName, DataTypeName>::GetIntersectionsSets()
{
  return IntersectionsSets;
}

template <typename AtomicSetTypeName, typename DataTypeName>
Set<AtomicSetTypeName, DataTypeName>::Set()
{
  Complementaires.clear();
  IntersectionsSets.clear();
}


template <typename AtomicSetTypeName, typename DataTypeName>
Set<AtomicSetTypeName, DataTypeName>::Set(Set &Other)
{
  Complementaires = Other.Complementaires;
  IntersectionsSets = Other.IntersectionsSets;  
}



template <typename AtomicSetTypeName, typename DataTypeName>
Set<AtomicSetTypeName, DataTypeName> *Set<AtomicSetTypeName, DataTypeName>::Intersect(AtomicSetTypeName &Other)
{
  Set<AtomicSetTypeName, DataTypeName> *Res = new Set<AtomicSetTypeName, DataTypeName>;
  *Res = *this;
  Res->IntersectionsSets.push_back(Other);
  return Res;
}

template <typename AtomicSetTypeName, typename DataTypeName>
void Set<AtomicSetTypeName, DataTypeName>::SelfIntersect(AtomicSetTypeName &Other)
{
  IntersectionsSets.push_back(Other);
}

template <typename AtomicSetTypeName, typename DataTypeName>
Set<AtomicSetTypeName, DataTypeName> *Set<AtomicSetTypeName, DataTypeName>::Intersect(Set<AtomicSetTypeName, DataTypeName> &Other)
{
  Set<AtomicSetTypeName, DataTypeName> *Res = new Set<AtomicSetTypeName, DataTypeName>;
  *Res = *this;
  for (typename MyVector<AtomicSetTypeName>::iterator I = Other.Complementaires.begin(); I != Other.Complementaires.end(); I++)
    Res->Complementaires.push_back(*I);
  for (typename MyVector<AtomicSetTypeName>::iterator I = Other.IntersectionsSets.begin(); I != Other.IntersectionsSets.end(); I++)
    Res->IntersectionsSets.push_back(*I);
  return Res;
}

template <typename AtomicSetTypeName, typename DataTypeName>
void Set<AtomicSetTypeName, DataTypeName>::SelfIntersect(Set<AtomicSetTypeName, DataTypeName> &Other)
{
  for (typename MyVector<AtomicSetTypeName>::iterator I = Other.Complementaires.begin(); I != Other.Complementaires.end(); I++)
    Complementaires.push_back(*I);
  for (typename MyVector<AtomicSetTypeName>::iterator I = Other.IntersectionsSets.begin(); I != Other.IntersectionsSets.end(); I++)
    IntersectionsSets.push_back(*I);
}


/*template <typename AtomicSetTypeName, typename DataTypeName>
Set<AtomicSetTypeName, DataTypeName>::~Set()
{
}*/


















#endif






