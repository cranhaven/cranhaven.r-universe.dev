/*
 *  Segment.h
 *  Segments
 *
 *  Created by Michel Koskas on 12/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#ifndef _Segment_h_
#define _Segment_h_

#include "Constants.h"
#include "MyVector.h"
//#include "Sets.h"
#include <iostream>

using namespace std;

class MultiSegment;

class Segment //: public AtomicSet
{
private:
	bool LeftBoundIncluded;
	bool RightBoundIncluded;
  double Min;
  double Max;
// setter and getter
public:
  //unsigned int sizeof(){return 2 * (sizeof(bool) + sizeof(double));}
  Segment();
  ~Segment();
  Segment(double, double, bool LBI = true, bool RBI = true);
  Segment *Intersect(const Segment &);
  void SelfIntersect(const Segment &);
  MultiSegment *IntersectWithComplementary(Segment &Other);
  void Initialize(double MI, double PI, bool LBI = true, bool RBI = true);
private:
// call by Intersect...
public:
  // TODO : Next funciton is public because of its use by "Pave". Maybe we should manage this (class friend?)
  bool CheckAndRepair();
  bool Empty();
  bool AlmostEmpty();
  bool Contains(double);
  double GetLeft() const;
  double GetRight() const;
  bool LeftBoundInside() const;
  bool RightBoundInside() const;
  Segment operator=(const Segment &S)
  {
    if (this != &S)
    {
       // TODO Use SetMe
			LeftBoundIncluded = S.LeftBoundIncluded;
			RightBoundIncluded = S.RightBoundIncluded;
      Min = S.Min;
      Max = S.Max;
    }
    return *this;
  }
  Segment(const Segment &S)
  {
		LeftBoundIncluded = S.LeftBoundIncluded;
		RightBoundIncluded = S.RightBoundIncluded;
    Min = S.Min;
    Max = S.Max;
  }
  bool operator==(const Segment &S);
private:
  std::ostream &operator<<(std::ostream &s);
public:
  void SetMe(double, double, bool LBI = true, bool RBI = true);
  void SetLeft(double, bool LBI = true);
  void SetRight(double, bool RBI = true);
private:
public:
  // TODO : Next funciton is public because of its use by "Pave". Maybe we should manage this (class friend?)
  bool IsSubset(const Segment &S);
};

class MultiSegment : public Segment
{
private:
  MyVector<Segment> MySegments;
public:
  //unsigned int sizeof(){return MySegments.sizeof();}
  bool Empty();
  bool AlmostEmpty();
  MultiSegment(bool EmptySet = false);
  MultiSegment(Segment &I);
  MultiSegment(MultiSegment &I);
  MultiSegment(MyVector<Segment> &VS);
  MultiSegment *Intersect(Segment &I);
  void SelfIntersect(Segment &I);
  MultiSegment *Intersect(MultiSegment *MS);
  void SelfIntersect(MultiSegment *MS);
  void FindNumberSegment(int &SegNum, bool &InSeg, double x, bool IsIncluded);
  int FindSegmentNumber(double x, bool IsIncluded);
  void SetMe(MultiSegment &MS);
  MultiSegment *FindMyComplementary();
public:
  MyVector<Segment> &GetMySegments();
  void AddInMySegments(Segment &S);
  void SelfIntersectWithComplementary(Segment &S);
  void SelfIntersectWithComplementary(MultiSegment *S);
  bool Contains(double);
  MultiSegment operator=(MultiSegment &M)
  {
    if (this != &M)
      {
        MySegments.clear();
        MySegments = M.MySegments;
      }
    return *this;
  }
};

#endif
