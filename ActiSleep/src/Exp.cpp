/*
 *  Exponential.cpp
 *  Segments
 *
 *  Created by Michel Koskas on 26/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#include "Exp.h"
#include "Constants.h"
#include <iostream>
#include <cmath>
#include <algorithm>
#include "MyVector.h"


Exponential::Exponential()
{
  ResetMe();
}

Exponential::Exponential(double a, double b, double s)
{
  ResetMe(a, b, s);
}

Exponential::Exponential(double y)
{
  ResetMe(y);
}

void Exponential::ResetMe()
{
  A = 0; B = 0; S = 0;
  FirstElement = 0;
  FirstElementSpecified = false;
}

void Exponential::ResetMe(double a, double b, double s)
{
  A = a; B = b; S = s;
  FirstElement = s;
  FirstElementSpecified = true;
}

void Exponential::ResetMe(double y)
{
  SpecializeMe(y);
}


void Exponential::SpecializeMe(double y)
{
  A=0;
  B = 1;
  S = y;
  FirstElement = y;
  FirstElementSpecified = true;
}


double Exponential::operator()(double y,  double mu)
{
  if (mu != 0)
    return A + y * mu - log(mu);
  return 0;
}

double Exponential::operator()(double mu)
{
    if (mu != 0)
      return A + S *mu - B* log(mu);
    return 0;
}


double Exponential::operator[](double mu)
{
    if (mu != 0)
      return S - B /mu;
    return 0;
}



double Exponential::Min(Segment &Q)
{
  double AuxRes = ArgMin(Q);
  return (*this)(AuxRes);
}


double Exponential::ArgMin(Segment &Q)
{
  Segment Tol(0,TOLERANCE);
  if(!Tol.Contains((*this).S))
  {
    if(!Tol.Contains((*this).B))
			if (Q.Contains((*this).B/(*this).S))
	      return B/S;
	    else if ((*this)(Q.GetRight()) > (*this)(Q.GetLeft()))
	    	return Q.GetLeft();
    if (S > 0)
	return Q.GetLeft();
    else
	return Q.GetRight();
  }
  if(!Tol.Contains((*this).B))
    return Q.GetRight();
  else
    return Q.GetLeft();
}

double Exponential::Min(MultiSegment &MS)
{
  if (MS.Empty())
    return PLUS_INFINITY;
  double Answer = PLUS_INFINITY;
  for (MyVector<Segment>::iterator I = MS.GetMySegments().begin(); I != MS.GetMySegments().end(); I++)
  {
    Answer = std::min(Answer, Min(*I));
  }
  return Answer;
}

double Exponential::ArgMin(MultiSegment &MS)
{
  if (MS.Empty())
    return PLUS_INFINITY;
  double Answer = PLUS_INFINITY;
  double min = PLUS_INFINITY;
  for (MyVector<Segment>::iterator I = MS.GetMySegments().begin(); I != MS.GetMySegments().end(); I++)
	if ((*this).Min(*I) < min)
	{
	  Answer = (*this).ArgMin(*I);
	  min = (*this).Min(*I);
	}

  return Answer;
}

double Exponential::Min()
{
  Segment Q(0,PLUS_INFINITY);
  return (*this).Min(Q);
}

double Exponential::ArgMin()
{
  Segment Q(0,PLUS_INFINITY);
  return (*this).ArgMin(Q);
}

MultiSegment *Exponential::LowerThanZero(MultiSegment &MS)
{
	Segment I(MINUS_INFINITY,PLUS_INFINITY);

  if ((*this).S==0)
	{
    if ((*this).B==0)
		{
			if ((*this).A<=0)
				I.SetMe(MINUS_INFINITY,PLUS_INFINITY,false,false);
			else
				I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
		}
		else
		{
			double R = exp((*this).A/(*this).B);
			if ((*this).B > 0)
        I.SetMe(R,PLUS_INFINITY,true,false);
			else
				I.SetMe(MINUS_INFINITY,R,false,true);
		}
    MultiSegment *SAux = MS.Intersect(I);
    return SAux;
	}
  else
	{
		if ((*this).B==0)
		{
			double R = -A/S;
			if ((*this).S>0)
				I.SetMe(MINUS_INFINITY, R, false, true);
			else
				I.SetMe(R,PLUS_INFINITY, true, false);
			MultiSegment *SAux = MS.Intersect(I);
			return SAux;
		}
		else
		{
      double TheMin = (*this)((*this).B/(*this).S);
			if (TheMin > EPSILON)
			{
				I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
				MultiSegment *SAux = MS.Intersect(I);
				return SAux;
			}
			if (abs(TheMin) < EPSILON)
			{
				I.SetMe((*this).B/(*this).S, (*this).B/(*this).S, true, true);
				MultiSegment *SAux = MS.Intersect(I);
				return SAux;
			}
      double FirstRoot, SecondRoot;
      // Computing first root
      double V = (*this).B/(*this).S, U = 0;
      while ((*this)(V) < 0)
	V /= 2;
      while (V - U >= EPSILON)
	{
	  U = V;
	  V = U - ((*this)(U)) / ((*this)[U]);
	}
      FirstRoot = V;
      // Computing second root 
      V = (*this).B/(*this).S;
      while ((*this)(V) < 0)
	V *= 2;
      U = V + 1;
      while (U - V >= EPSILON)
	{
	  U = V;
	  V = U - ((*this)(U)) / ((*this)[U]);
	}
      SecondRoot = V;
      // Done
      I.SetMe(FirstRoot, SecondRoot,true,true);
      MultiSegment *SAux = MS.Intersect(I);
      return SAux;
    }
	}
}

MultiSegment *Exponential::IsLowerThan(MultiSegment &MS, double C)
{
  A = A - C;
  MultiSegment *Answer = LowerThanZero(MS);
  A = A + C;
  return Answer;
}

MultiSegment *Exponential::IsLowerThan(double C)
{
	Segment Q(0,PLUS_INFINITY);
  MultiSegment MS(Q);
  A = A - C;
  MultiSegment *Answer = LowerThanZero(MS);
  A = A + C;
  return Answer;
}

void Exponential::operator+=(const Exponential &Other)
{
  A += Other.A;
  B += Other.B;
  S += Other.S;
  FirstElementSpecified=true;
}

void Exponential::operator+=(const double &C)
{
  A += C;
  FirstElementSpecified=true;
}

Exponential *Exponential::operator+(const Exponential &Other)
{
  Exponential *Res = new Exponential(Other.A, Other.B, Other.S);
  (*Res).A += (*this).A;
  (*Res).B += (*this).B;
  (*Res).S += (*this).S;
  (*Res).FirstElementSpecified=true;
  return Res;
}

Exponential *Exponential::operator+(const double &C)
{
  Exponential *Res = new Exponential((*this).A, (*this).B, (*this).S);
  (*Res).A += C;
  (*Res).FirstElementSpecified=true;
  return Res;
}

Exponential *Exponential::operator%(const int &C)
{
  Exponential *Res = new Exponential((*this).A, (*this).B, (*this).S);
  (*Res).A *= C;
  (*Res).B *= C;
  (*Res).S *= C;
  (*Res).FirstElementSpecified=true;
  return Res;
}

void Exponential::operator*=(int x)
{
  A *= x;
  S *= x;
  B *= x;
}


