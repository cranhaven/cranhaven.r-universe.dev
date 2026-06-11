

#include "Trinome.h"
#include "Constants.h"
#include <math.h>
#include <algorithm>
#include <iostream>

Trinome::Trinome()
{
  ResetMe();
}

Trinome::Trinome(double y)
{
  ResetMe(y);
}

Trinome::Trinome(double a, double b, double c)
{
  ResetMe(a, b, c);
}

void Trinome::ResetMe()
{
  FirstElement = 0;
  FirstElementSpecified = false;
  a2=0;
  a1=0;
  a0=0;
}

void Trinome::ResetMe(double y)
{
  SpecializeMe(y);
}

void Trinome::ResetMe(double a, double b, double c)
{
  a2 = a;
  a1 = b;
  a0 = c;
  FirstElementSpecified = true;
}

Trinome::~Trinome()
{
}

void Trinome::SpecializeMe(double Y)
{
  FirstElement = Y;
  FirstElementSpecified = true;
  a2 = 1;
  a1 = -2 * Y;
  a0 = Y * Y;
}

double Trinome::operator()(double y,  double mu)
{
  return (y-mu) * (y - mu);
}

double Trinome::operator()(double mu)
{
     return ((a2*mu+a1)*mu + a0);
}

double Trinome::operator[](double mu)
{
     return (2*a2*mu+a1);
}

double Trinome::Min(Segment &S)
{
  if (S.Empty())
    return PLUS_INFINITY;
  double m = -a1 / (2 * a2);
  if ((a2 > 0) && (S.Contains (m)))
    return (*this)(m);
  return std::min((*this)(S.GetLeft()), (*this)(S.GetRight()));
}

double Trinome::ArgMin(Segment &S)
{
  if (S.Empty())
    return MINUS_INFINITY;
  double m = -a1 / (2 * a2);
  if ((a2 > 0) && (S.Contains (m)))
    return m;
  return S.GetLeft();
}

double Trinome::ArgMin()
{
  Segment S;
  return ArgMin(S);
}

double Trinome::Min()
{
  Segment S;
  return Min(S);
}

double Trinome::Min(MultiSegment &S)
{
  if (S.Empty())
    return PLUS_INFINITY;
  double Answer = PLUS_INFINITY;
  for (MyVector<Segment>::iterator I = (S.GetMySegments()).begin(); I != (S.GetMySegments()).end(); I++)
  {
    Answer = std::min(Answer, Min(*I));
  }
  return Answer;
}

double Trinome::ArgMin(MultiSegment &MS)
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

void Trinome::operator*=(double x)
{
  a0 *= x;
  a1 *= x;
  a2 *= x;
}

void Trinome::operator*=(int x)
{
  a0 *= x;
  a1 *= x;
  a2 *= x;
}

MultiSegment *Trinome::LowerThanZero(MultiSegment &S)
{
	if (a2 != 0)
	{
		double TheMin = (*this)(-a1/(2*a2));
		if (TheMin > 0)
		{
			MultiSegment *Sp = new MultiSegment(true);
			return Sp;
		}
		double Delta, FirstRoot, SecondRoot;
		Delta= pow(a1,2)-4*a2*a0;
		FirstRoot=(-a1-sqrt(Delta))/(2*a2);
		SecondRoot=(-a1+sqrt(Delta))/(2*a2);

		Segment AuxS(FirstRoot, SecondRoot);
		MultiSegment *Answer = new MultiSegment(AuxS);
		Answer->SelfIntersect(&S);
		return Answer;
	}
	if (a1 != 0)
	{
		double r = -a0/a1;
		if (S.Contains(r))
		{
			Segment I;
			if (a1>0)
			  I.SetMe(S.GetLeft(), r,S.LeftBoundInside(),true);
			else
			  I.SetMe(r, S.GetRight(), true, S.RightBoundInside());
			MultiSegment *Res = new MultiSegment(I);
			return Res;
		}
		if (a1>0)
		{
			MultiSegment *Res = new MultiSegment;
			if (r>S.GetRight())
			  (*Res).AddInMySegments(S);
			return Res;
		}
		else
		{
			MultiSegment *Res = new MultiSegment;
			if (r<S.GetLeft())
			  (*Res).AddInMySegments(S);
			return Res;
		}
	}
	MultiSegment *Res = new MultiSegment;
	if (a0 <= 0)
	  (*Res).AddInMySegments(S);
	return Res;
}


MultiSegment *Trinome::IsLowerThan(MultiSegment &S, double C)
{
  a0 = a0-C;
  MultiSegment *Answer = LowerThanZero(S);
  a0 = a0 + C;
  return Answer;

}


MultiSegment *Trinome::IsLowerThan(double C)
{
  MultiSegment *MS = new MultiSegment(false);
  a0 -= C;
  MultiSegment *Res = LowerThanZero(*MS);
  a0 += C;
  delete MS;
  return Res;
}

void Trinome::operator+=(Trinome &Other)
{
  a2 += Other.a2;
  a1 += Other.a1;
  a0 += Other.a0;
  FirstElementSpecified = true;
}

void Trinome::operator+=(const double C)
{
  a0 += C;
  FirstElementSpecified = true;
}

Trinome *Trinome::operator+(Trinome &Other)
{
  Trinome *Res = new Trinome(0);
  (*Res).a0 = (*this).a0 + Other.a0;
  (*Res).a1 = a1 + Other.a1;
  (*Res).a2 = a2 + Other.a2;
  (*Res).FirstElementSpecified = true;
  return Res;
}

Trinome *Trinome::operator+(const double &C)
{
  Trinome *Res = new Trinome(0);
  (*Res).a0 = a0 + C;
  (*Res).a1 = a1;
  (*Res).a2 = a2;
  (*Res).FirstElementSpecified = true;
    return Res;
}

Trinome *Trinome::operator*(const int &C)
{
  Trinome *Res = new Trinome(0);
  (*Res).a0 = a0 * C;
  (*Res).a1 = a1 * C;
  (*Res).a2 = a2 * C;
  (*Res).FirstElementSpecified = true;
    return Res;
}

