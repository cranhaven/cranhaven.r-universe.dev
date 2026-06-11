
#include "Variance.h"
#include "Segment.h"
#include "Constants.h"
#include <iostream>
#include <math.h>
#include <algorithm>
#include <cmath>
#include "GeneralFunctionsDeclarations.h"



  // Variance(sigma)= A + T.ln(sigma) + S/2 . 1/sigma^2
  // Variance(sigma^2)= A + T.ln(sigma)/2 + S/(2*sigma)
  // S = (y-mu)^2

Variance::Variance()
{
  ResetMe();
}

Variance::Variance(double nmu)
{
  ResetMe(nmu);
}


Variance::Variance(int t, double s, double nmu)
{
  ResetMe(t, s, nmu);
}

Variance::Variance(double a, int t, double s, double nmu)
{
  ResetMe(a, t, s, nmu);
}

void Variance::ResetMe()
{
  A = 0;
  T = 0;
  S = 0;
  mu = 0;
  FirstElement = 0;
  FirstElementSpecified = true;
}

void Variance::ResetMe(double m)
{
  A = 0;
  T = 0;
  S = 0;
  mu = m;
  FirstElement = 0;
  FirstElementSpecified = true;
}


void Variance::ResetMe(int t, double s, double m)
{
  A = 0;
  T = t;
  S = s;
  mu = m;
  FirstElement = s;
  FirstElementSpecified = true;
}

void Variance::ResetMe(double a, int t, double s, double m)
{
  A = a;
  T = t;
  S = s;
  mu = m;
  FirstElement = s;
  FirstElementSpecified = true;
}

Variance *Variance::operator+(Variance &Other)
{
  Variance *Res = new Variance;
  Res->A = (*this).A + Other.A;
  Res->S = (*this).S + Other.S;
  Res->T = (*this).T + Other.T;
  Res->mu = ((*this).mu + Other.mu)/2;
  return Res;
}

Variance *Variance::operator+(const double &C)
{
  Variance *Res = new Variance;
  Res->A = (*this).A + C ;
  Res->S = (*this).S;
  Res->T = (*this).T;
  Res->mu = (*this).mu;
  (*Res).FirstElementSpecified = true;
  return Res;
}

Variance *Variance::operator*(const int &C)
{
  Variance *Res = new Variance;
  Res->A = (*this).A * C ;
  Res->S = (*this).S * C;
  Res->T = (*this).T * C;
  Res->mu = (*this).mu ;
  (*Res).FirstElementSpecified = true;
  return Res;
}

void Variance::operator+=(const double &C)
{
  A += C;
  FirstElementSpecified = true;
}

void Variance::operator+=(Variance &Other)
{
  mu = (mu + Other.mu)/2;
  A += Other.A;
  S += Other.S;
  T += Other.T;
  FirstElementSpecified = true;
}


void Variance::SpecializeMe(double Y)
{
  A = 0;
  T = 1;
  S = (Y-mu)*(Y-mu);
  FirstElementSpecified = true;
}


double Variance::operator()(double y, double sigma)
{
  return log(sigma)/2 + (y-mu)*(y-mu)/(2*sigma);
}


double Variance::operator()(double sigma)
{
    return A + T * log(sigma)/2 + S/(2*sigma);
}

double Variance::operator[](double sigma)
{

    return (T / (2*sigma) - S /(2*sigma*sigma) );
}

double Variance::Min(Segment &LS)
{
  double xmin = MINUS_INFINITY;
  if (T>0)
    xmin = S/T;
  if (LS.Contains(xmin))
    return (*this)(xmin);
  return std::min((*this)(LS.GetLeft()), (*this)(LS.GetRight()));
}

double Variance::ArgMin(Segment &LS)
{
  double xmin = MINUS_INFINITY;
  if (T>0)
    xmin = S/T;
  if (LS.Contains(xmin))
    return xmin;
  if ((*this)(LS.GetLeft()) < (*this)(LS.GetRight()))
    return LS.GetLeft();
  return LS.GetRight();
}

double Variance::Min()
{
  Segment LS;
  return Min(LS);
}

double Variance::ArgMin()
{
  Segment LS;
  return ArgMin(LS);
}


double Variance::Min(MultiSegment &MS)
{
  if (MS.Empty())
    return PLUS_INFINITY;
  double Answer = PLUS_INFINITY;
  for (MyVector<Segment>::iterator I = MS.GetMySegments().begin(); I != MS.GetMySegments().end(); I++)
  {
    Answer = std::min(Answer, (*this).Min(*I));
  }
  return Answer;
}

double Variance::ArgMin(MultiSegment &MS)
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


// Commentary: in this function A is not necessary linked to Y as it should usually be.
MultiSegment *Variance::LowerThanZero(MultiSegment &MS)
{
  Segment I(MINUS_INFINITY,PLUS_INFINITY);
  if (S==0)
  {
    if (T==0)
    {
	if (A<=0)
  	  I.SetMe(MINUS_INFINITY,PLUS_INFINITY,false,false);
	else
	  I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
    }
    else
    {
	double R = exp(-2*A/T);
	I.SetMe(MINUS_INFINITY,R,false,true);
    }
  }
  else
  {
    if ((*this).T==0)
    {
	if (A>=0)
	  I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
	else
	{
	  double R = -S/(2*A);
  	  I.SetMe(R, PLUS_INFINITY, true, false);
	}
    }
    else // Solving A+T/2 ln(x) +S/(2x)=0 is equivalent to solving 2Ax+Txln(x)=0 which is convex
    {
	double xmin = S/T;
	double TheMin = (*this)(xmin);
	if (abs(TheMin) < EPSILON)
	  I.SetMe(xmin,xmin,true,true);
	else if (TheMin > 0)
	  I.SetMe(PLUS_INFINITY,MINUS_INFINITY,false,false);
	else
	{
	  double FirstRoot, SecondRoot;

	  // Now computing the first root (near 0)
	  double V = xmin, U;
	  while ((*this)(V) < 0)
	    V /= 2;
	  U = 2 * V;
	  while (abs(V - U) > EPSILON)
	  {
		U = V;
		//V = U - (2*A*U + S + T*U*log(U)) / (T*log(U) + T + 2*A);
		V = U - ((*this)(U)) / ((*this)[U]);

	  }
	  FirstRoot = V;

	  // Now computing the second root (near +infty)
	  //double Ri = PLUS
	  if ((*this)(MS.GetMySegments()[MS.GetMySegments().size()-1].GetRight())<0)
	    SecondRoot = PLUS_INFINITY;
	  else
	  {
	    V = xmin;
	    while ((*this)(V) < 0)
	      V *= 2;
	    U = V / 2;
	    while (abs(V - U) > EPSILON)
	    {
		U = V;
		//V = U - (2*A*U + S + T*U*log(U)) / (T*log(U) + T + 2*A);
		V = U - ((*this)(U)) / ((*this)[U]);

	    }
 	    SecondRoot = V;
	  }
	  I.SetMe(FirstRoot, SecondRoot,true, true);
	}
      }
  }
  return MS.Intersect(I);

}


MultiSegment *Variance::IsLowerThan(MultiSegment &LS, double C)
{
  A = A - C;
  MultiSegment *Answer = LowerThanZero(LS);
  A = A + C;
  return Answer;
}

void Variance::operator*=(int x)
{
  A *= x;
  S *= x;
  T *= x;
}




