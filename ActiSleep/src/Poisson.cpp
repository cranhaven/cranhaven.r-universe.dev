/*
 *  Poisson.cpp
 *  Segments
 *
 *  Created by Michel Koskas on 26/01/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#include "Poisson.h"
#include "Constants.h"
#include <iostream>
#include <cmath>
#include <algorithm>
#include "MyVector.h"


Poisson::Poisson()
{
  ResetMe();
}

Poisson::Poisson(double a, double b, double s)
{
  ResetMe(a, b, s);
}

Poisson::Poisson(int y)
{
  ResetMe(y);
}

void Poisson::ResetMe()
{
  A = 0; B = 0; S = 0;
  FirstElement = 0;
  FirstElementSpecified = false;
}

void Poisson::ResetMe(double a, double b, double s)
{
  A = a; B = b; S = s;
  FirstElement = s;
  FirstElementSpecified = true;
}

void Poisson::ResetMe(int y)
{
  SpecializeMe(y);
}


void Poisson::SpecializeMe(int y)
{
  A=0;
  B = 1;
  S = y;
  FirstElement = y;
  FirstElementSpecified = true;
}


double Poisson::operator()(int y,  double mu)
{
  if (mu != 0)
    return A + mu - y * log(mu);
  return 0;
}

double Poisson::operator()(double mu)
{
    if (mu != 0)
      return A + B*mu - S * log(mu);
    return 0;
}


double Poisson::operator[](double mu)
{
    if (mu != 0)
      return B - S /mu;
    return 0;
}



double Poisson::Min(Segment &Q)
{
  double Res = 0;
  if((*this).B != 0)
	{
		if ((*this).S !=0)
			if (Q.Contains((*this).S/(*this).B))
			{
	      Res = (*this)(S/B);
	      return Res;
			}
		Res = std::min(((*this).A +(*this).B *(Q.GetLeft())), ((*this).A +(*this).B *(Q.GetRight())));
		return Res;
	}
  if ((*this).S !=0)
	{
    Res = (*this)(Q.GetRight());
    return Res;
	}
  else
    return A;
}


double Poisson::ArgMin(Segment &Q)
{
  //double Res = 0;
  if((*this).B != 0)
  {
    if ((*this).S !=0)
	if (Q.Contains((*this).S/(*this).B))
	      return S/B;
    if (B > 0)
	return Q.GetLeft();
    else
	return Q.GetRight();
  }
  if ((*this).S !=0)
    return Q.GetRight();
  else
    return Q.GetLeft();
}

double Poisson::Min(MultiSegment &MS)
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

double Poisson::ArgMin(MultiSegment &MS)
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

double Poisson::Min()
{
  Segment Q;
  return (*this).Min(Q);
}

double Poisson::ArgMin()
{
  Segment Q;
  return (*this).ArgMin(Q);
}

MultiSegment *Poisson::LowerThanZero(MultiSegment &MS)
{
	Segment I(MINUS_INFINITY,PLUS_INFINITY);

  if ((*this).B==0)
	{
    if ((*this).S==0)
		{
			if ((*this).A<=0)
				I.SetMe(MINUS_INFINITY,PLUS_INFINITY,false,false);
			else
				I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
		}
		else
		{
			double R = exp((*this).A/(*this).S);
			if ((*this).S > 0)
        I.SetMe(R,PLUS_INFINITY,true,false);
			else
				I.SetMe(MINUS_INFINITY,R,false,true);
		}
    MultiSegment *SAux = MS.Intersect(I);
    return SAux;
	}
  else
	{
		if ((*this).S==0)
		{
			double R = -A/B;
			if ((*this).B>0)
				I.SetMe(MINUS_INFINITY, R, false, true);
			else
				I.SetMe(R,PLUS_INFINITY, true, false);
			MultiSegment *SAux = MS.Intersect(I);
			return SAux;
		}
		else
		{
      double TheMin = (*this)((*this).S/(*this).B);
			if (TheMin > EPSILON)
			{
				I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
				MultiSegment *SAux = MS.Intersect(I);
				return SAux;
			}
			if (abs(TheMin) < EPSILON)
			{
				I.SetMe((*this).S/(*this).B, (*this).S/(*this).B, true, true);
				MultiSegment *SAux = MS.Intersect(I);
				return SAux;
			}
      double FirstRoot, SecondRoot;
      // Computing first root
      double V = (*this).S/(*this).B, U = 0;
      while ((*this)(V) < 0)
	V /= 2;
      while (V - U >= EPSILON)
	{
	  U = V;
	  V = U - ((*this)(U)) / ((*this)[U]);
	}
      FirstRoot = V;
      // Computing second root 
      V = (*this).S/(*this).B;
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

MultiSegment *Poisson::IsLowerThan(MultiSegment &MS, double C)
{
  A = A - C;
  MultiSegment *Answer = LowerThanZero(MS);
  A = A + C;
  return Answer;
}

MultiSegment *Poisson::IsLowerThan(double C)
{
  MultiSegment MS;
  A = A - C;
  MultiSegment *Answer = LowerThanZero(MS);
  A = A + C;
  return Answer;
}

void Poisson::operator+=(const Poisson &Other)
{
  A += Other.A;
  B += Other.B;
  S += Other.S;
  FirstElementSpecified=true;
}

void Poisson::operator+=(const double &C)
{
  A += C;
  FirstElementSpecified=true;
}

Poisson *Poisson::operator+(const Poisson &Other)
{
  Poisson *Res = new Poisson(Other.A, Other.B, Other.S);
  (*Res).A += (*this).A;
  (*Res).B += (*this).B;
  (*Res).S += (*this).S;
  (*Res).FirstElementSpecified=true;
  return Res;
}

Poisson *Poisson::operator+(const double &C)
{
  Poisson *Res = new Poisson((*this).A, (*this).B, (*this).S);
  (*Res).A += C;
  (*Res).FirstElementSpecified=true;
  return Res;
}

Poisson *Poisson::operator%(const int &C)
{
  Poisson *Res = new Poisson((*this).A, (*this).B, (*this).S);
  (*Res).A *= C;
  (*Res).B *= C;
  (*Res).S *= C;
  (*Res).FirstElementSpecified=true;
  return Res;
}

void Poisson::operator*=(int x)
{
  A *= x;
  S *= x;
  B *= x;
}


