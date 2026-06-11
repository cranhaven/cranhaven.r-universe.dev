/*
 *  Segment.cpp
 *  Segments
 *
 *  Created by Michel Koskas on 12/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#include <algorithm>
#include <iostream>

#include "Segment.h"
#include "MyVector.h"

void Segment::Initialize(double MI, double PI, bool LBI, bool RBI)
{
	LeftBoundIncluded = LBI;
	RightBoundIncluded = RBI;
  Min = MI;
  Max = PI;
	CheckAndRepair();
}

Segment::Segment()
{
	LeftBoundIncluded = false;
	RightBoundIncluded = false;
  Min = MINUS_INFINITY;
  Max = PLUS_INFINITY;
}

Segment::~Segment()
{
}

bool Segment::LeftBoundInside() const
{
	return LeftBoundIncluded;
}

bool Segment::RightBoundInside() const
{
	return RightBoundIncluded;
}

std::ostream &operator<<(std::ostream &s, const Segment &S)
{
	if (S.LeftBoundInside())
		s << "[" ;
	else
		s << "]";
	s << S.GetLeft() << ", " << S.GetRight();
	if (S.RightBoundInside())
		s << "]";
	else
		s << "[";

  return s;
}

double Segment::GetLeft() const
{
  return Min;
}



double Segment::GetRight() const
{
  return Max;
}

std::ostream &operator<<(std::ostream &s, MultiSegment &S)
{
  s << " display of a multisegment : " << std::endl;
  for (MyVector<Segment>::iterator I = (S.GetMySegments()).begin(); I != (S.GetMySegments()).end(); I++)
    s << *I << " ";
  s << std::endl;
  s << "End of multisegment display." << std::endl << std::endl;
  return s;
}

void Segment::SetMe(double Left, double Right, bool LBI, bool RBI)
{
	LeftBoundIncluded = LBI;
	RightBoundIncluded = RBI;
  Min = Left;
  Max = Right;
  CheckAndRepair();
}


void Segment::SetLeft(double Left, bool LBI)
{
	LeftBoundIncluded = LBI;
	Min = Left;
}

void Segment::SetRight(double Right, bool RBI)
{
	RightBoundIncluded = RBI;
	Max = Right;
}

bool Segment::CheckAndRepair()
{
	if (Max > Min)
		return true;
	if ((Max == Min) && (LeftBoundIncluded) && (RightBoundIncluded))
		return true;
	// Now the set is empty.
	Max = MINUS_INFINITY;
	Min = PLUS_INFINITY;
	LeftBoundIncluded = false;
	RightBoundIncluded = false;
	return false;
}

Segment::Segment(double m, double M, bool LBI, bool RBI)
{
  SetMe(m, M, LBI, RBI);
}

Segment *Segment::Intersect(const Segment &Other)
{
  Segment *S = new Segment;
	if (Min > Other.GetLeft())
	{
		S->Min = Min;
		S->LeftBoundIncluded = LeftBoundIncluded;
	}
	else if (Min < Other.GetLeft())
	{
		S->Min = Other.Min;
		S->LeftBoundIncluded = Other.LeftBoundIncluded;
	}
	else
	{
		S->Min = Other.Min;
		S->LeftBoundIncluded = LeftBoundIncluded && Other.LeftBoundIncluded;
	}

	if (Max > Other.GetRight())
	{
		S->Max = Other.GetRight();
		S->RightBoundIncluded = Other.RightBoundIncluded;
	}
	else if (Max < Other.GetRight())
	{
		S->Max = Max;
		S->RightBoundIncluded = RightBoundIncluded;
	}
	else
	{
		S->Max = Max;
		S->RightBoundIncluded = RightBoundIncluded && Other.RightBoundIncluded;
	}
  S->CheckAndRepair();
  return S;
}

void Segment::SelfIntersect(const Segment &Other)
{
	if (Min < Other.GetLeft())
	{
		Min = Other.Min;
		LeftBoundIncluded = Other.LeftBoundIncluded;
	}
	else if (Min == Other.GetLeft())
		LeftBoundIncluded &= Other.LeftBoundIncluded;

	if (Max > Other.GetRight())
	{
		Max = Other.GetRight();
		RightBoundIncluded = Other.RightBoundIncluded;
	}
	else if (Max == Other.GetRight())
		RightBoundIncluded &= Other.RightBoundIncluded;
  CheckAndRepair();
}

bool Segment::IsSubset(const Segment &S)
{
	if (Min < S.GetLeft())
		return false;
	if (Min == S.GetLeft())
		if ((LeftBoundIncluded) && (!S.LeftBoundIncluded))
			return false;
	if (Max > S.GetRight())
		return false;
	if (Max == S.GetRight())
		if ((RightBoundIncluded) && (!S.RightBoundIncluded))
			return false;
  return true;
}



bool Segment::Empty()
{
  if (Min > Max)
    return true;
	if (Min == Max)
		return !(LeftBoundIncluded && RightBoundIncluded);
  return false;
}

bool Segment::AlmostEmpty()
{
  if (Min < Max)
    return false;
  return true;
}

bool Segment::Contains(double x)
{
  if (x < Min)
    return false;
  if (x > Max)
    return false;
	if (x == Min)
		return LeftBoundIncluded;
	if (x == Max)
		return RightBoundIncluded;
  return true;
}

bool Segment::operator==(const Segment &Other)
{
  if ((Min==Other.GetLeft()) && (Max==Other.GetRight()))
    return ((LeftBoundIncluded == Other.LeftBoundIncluded) && (RightBoundIncluded == Other.RightBoundIncluded));
  return false;
}

MultiSegment *Segment::IntersectWithComplementary(Segment &Other)
{
  MultiSegment *Res = new MultiSegment(true);
  Segment K(MINUS_INFINITY, MINUS_INFINITY, false, false);
  Segment L(PLUS_INFINITY, PLUS_INFINITY, false, false);

  if (Other.GetLeft() > MINUS_INFINITY)
	K.SetMe(MINUS_INFINITY, Other.GetLeft(), false, !Other.LeftBoundInside());
  if (Other.GetRight() < PLUS_INFINITY)
	L.SetMe(Other.GetRight(), PLUS_INFINITY, !Other.RightBoundInside(), false);

	Segment *T = (*this).Intersect(K);
	if (!T->Empty())
		(*Res).AddInMySegments(*T);
	Segment *R = (*this).Intersect(L);
	if (!R->Empty())
		(*Res).AddInMySegments(*R);
	delete R;
	delete T;
	return (Res);
}


bool MultiSegment::Empty()
{
  if (MySegments.empty())
    return true;
  else
    return false;
}

bool MultiSegment::AlmostEmpty()
{
  if (MySegments.empty())
    return true;
  else
    {
      for (MyVector<Segment>::iterator I = MySegments.begin(); I != MySegments.end(); I++)
	if (!(*I).AlmostEmpty())
	  return false;
    }
    return true;
}

MultiSegment::MultiSegment(bool EmptySet)
{
  if (EmptySet)
    {
			MySegments.clear();
			return;
    }
  Segment I;
  MySegments.push_back(I);
  //MySegments.Capacity = 10;
}

MultiSegment::MultiSegment(MultiSegment &S)
{
	for (MyVector<Segment>::iterator I = S.MySegments.begin(); I != S.MySegments.end(); I++)
		MySegments.push_back(*I);
}

MultiSegment::MultiSegment(Segment &S)
{
  S.CheckAndRepair();
  if (!S.Empty())
    MySegments.push_back(S);
}

MultiSegment::MultiSegment(MyVector<Segment> &VS)
{
  for (MyVector<Segment>::iterator I = VS.begin(); I != VS.end(); I++)
    {
			(*I).CheckAndRepair();
			if (!(*I).Empty())
				AddInMySegments(*I);
    }
}

void MultiSegment::SetMe(MultiSegment &MS)
{
  MySegments.clear();
  MySegments = MS.GetMySegments();
}

MyVector<Segment> &MultiSegment::GetMySegments()
{
  return MySegments;
}

void MultiSegment::AddInMySegments(Segment &S)
{
  S.CheckAndRepair();
  if (S.Empty())
    return;
  if (MySegments.size()==0)
    MySegments.push_back(S);
  else
  {
		int Smin=0;
		int Smax=0;
		bool MinIn=false;
		bool MaxIn=false;
		FindNumberSegment(Smin,MinIn,S.GetLeft(),S.LeftBoundInside());
		FindNumberSegment(Smax,MaxIn,S.GetRight(),S.RightBoundInside());
		MyVector<Segment> W;
		Segment Snew(MINUS_INFINITY, PLUS_INFINITY, false, false);
		for (int i = 0; i<Smin; i++)
			W.push_back(MySegments[i]);
		if (MinIn)
			Snew.SetLeft(MySegments[Smin].GetLeft(), MySegments[Smin].LeftBoundInside());
		else
			Snew.SetLeft(S.GetLeft(),S.LeftBoundInside());
		if (MaxIn)
		{
			Snew.SetRight(MySegments[Smax].GetRight(), MySegments[Smax].RightBoundInside());
			W.push_back(Snew);
			for (int i = (Smax + 1); i<MySegments.size(); i++)
				W.push_back(MySegments[i]);
		}
		else
		{
			Snew.SetRight(S.GetRight(), S.RightBoundInside());
			W.push_back(Snew);
			for (int i = Smax; i<MySegments.size(); i++)
				W.push_back(MySegments[i]);
		}
		MySegments = W;
  }
}

bool MultiSegment::Contains(double x)
{
	for (MyVector<Segment>::iterator I = MySegments.begin(); I != MySegments.end(); I++)
		if ((*I).Contains(x))
			return true;
		else
			if ((*I).GetRight() > x)
				return false;
	return false;
}

MultiSegment *MultiSegment::FindMyComplementary()
{
// TODO: do not use AddInMySegments (compute directly the full list instead)
  if (MySegments.size()==0)
  {
    Segment I;
    MultiSegment *Res = new MultiSegment(I);
    return Res;
  }
  MultiSegment *Res = new MultiSegment(true);
  Segment Smin(MINUS_INFINITY, MySegments[0].GetLeft(), false, !MySegments[0].LeftBoundInside());
	if (!Smin.Empty())
		(*Res).AddInMySegments(Smin);
	for (int i = 0; i < (MySegments.size()-1); i++)
	{
		Segment S (MySegments[i].GetRight(), MySegments[i+1].GetLeft(), !MySegments[i].RightBoundInside(), !MySegments[i+1].LeftBoundInside());
		(*Res).AddInMySegments(S);
	}
  Segment Smax(MySegments[MySegments.size()-1].GetRight(),PLUS_INFINITY, !MySegments[MySegments.size()-1].RightBoundInside(),false);
	if (!Smax.Empty())
		(*Res).AddInMySegments(Smax);
  return Res;
}

MultiSegment *MultiSegment::Intersect(Segment &S)
{
	MultiSegment *Res = new MultiSegment(true);
	if (S.Empty())
		return Res;
	int IndMin = FindSegmentNumber(S.GetLeft(), S.LeftBoundInside());
	int IndMax = FindSegmentNumber(S.GetRight(), S.RightBoundInside());
	if (IndMin >= MySegments.size())
		return Res;
	Segment *S1 = S.Intersect(MySegments[IndMin]);
	if (!S1->Empty())
		Res->MySegments.push_back(*S1);
	delete S1;
	if (IndMax > IndMin)
	{
		for (int i = IndMin + 1; i < IndMax; i++)
			Res->MySegments.push_back(MySegments[i]);
		if (IndMax < MySegments.size())
		{
			Segment *S2 = S.Intersect(MySegments[IndMax]);
			if (!S2->Empty())
				Res->MySegments.push_back(*S2);
			delete S2;
		}
	}
	return Res;
}

void MultiSegment::SelfIntersect(Segment &S)
{
	if (S.Empty())
	{
		MySegments.clear();
		return;
	}
	int IndMin = FindSegmentNumber(S.GetLeft(), S.LeftBoundInside());
	int IndMax = FindSegmentNumber(S.GetRight(), S.RightBoundInside());
	MySegments[IndMin].SelfIntersect(S);
	MySegments[IndMax].SelfIntersect(S);
	MySegments.erase(MySegments.begin() + IndMax + 1, MySegments.end());
	MySegments.erase(MySegments.begin(), MySegments.begin() + IndMin);
}

void MultiSegment::FindNumberSegment(int &SegNum, bool &InSeg, double x, bool IsIncluded)
{
  SegNum = MySegments.size();
  InSeg = false;
  if (MySegments.size() != 0)
	{
		// TODO: Use dichotomy
		for (int NS = 0; NS < MySegments.size(); NS++)
			if ( (x < MySegments[NS].GetRight()) ||( (x == MySegments[NS].GetRight()) && IsIncluded && MySegments[NS].RightBoundInside()) )
	    {
				SegNum = NS;
				InSeg = MySegments[NS].Contains(x);
				break;
	    }
	}
}


// TODO: REMOVE IsIncluded from arguments!
int MultiSegment::FindSegmentNumber(double x, bool IsIncluded)
{
	int Res;
	bool Aux;
	FindNumberSegment(Res, Aux, x, IsIncluded);
	return Res;
}

MultiSegment *MultiSegment::Intersect(MultiSegment *MS)
{
  MultiSegment *Res = new MultiSegment(true);
  if ((!Empty()) && (!MS->Empty()))
		for (int NS = 0; NS < MySegments.size(); NS++)
		{
			MultiSegment *Aux = (*MS).Intersect(MySegments[NS]);
			for (int i = 0; i < Aux->MySegments.size(); i++)
				Res->MySegments.push_back(Aux->MySegments[i]);
			Aux->MySegments.clear();
			delete Aux;
		}
	return Res;
}

void MultiSegment::SelfIntersect(MultiSegment *MS)
{
	MultiSegment *Aux = Intersect(MS);
	MySegments = Aux->MySegments;
	Aux->MySegments.clear();
	delete Aux;
}

void MultiSegment::SelfIntersectWithComplementary(MultiSegment *S)
{
  MultiSegment *Aux = (*S).FindMyComplementary();
  (*this).SelfIntersect(Aux);
  Aux->MySegments.clear();
  delete Aux;
}

void MultiSegment::SelfIntersectWithComplementary(Segment &S)
{
  MyVector<Segment> W;
  for (MyVector<Segment>::iterator I = MySegments.begin(); I != MySegments.end(); I++)
    {
			MultiSegment *MS = (*I).IntersectWithComplementary(S);
			for (int i=0; i < MS->MySegments.size(); i++)
				W.push_back(MS->GetMySegments()[i]);
			MS->MySegments.clear();
			delete MS;
    }
  MySegments = W;
}






