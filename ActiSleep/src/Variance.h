

#ifndef _Variance_h_
#define _Variance_h_

#include "Constants.h"
#include "Function.h"
#include "Segment.h"



class Variance : public Function<Segment, int, MultiSegment>
{
  // Variance(sigma)= A + T.ln(sigma) + S/2 . 1/sigma^2
  // S = (y-mu)^2

public:
  double A;
  int T;
  double S;
  double mu;
  Variance();
  Variance(double mu);
  Variance(int t, double s, double mu);
  Variance(double a, int t, double s, double mu);
  void ResetMe();
  void ResetMe(double);
  void ResetMe(int, double, double);
  void ResetMe(double a, int t, double s, double mu);


  void SpecializeMe(double);
  double operator()(double);
  double operator()(double,double);
  double operator[](double);
  void operator*=(int x);
  MultiSegment *LowerThanZero(MultiSegment &);
  Variance operator=(const Variance &Model)
  {
    FirstElementSpecified = Model.FirstElementSpecified;
    FirstElement = Model.FirstElement;
    A = Model.A;
    T = Model.T;
    S = Model.S;
    mu = Model.mu;
    return *this;
  }
  Variance operator=(const double &C)
  {
    FirstElementSpecified = true;
    FirstElement = 0;
    A = C;
    T = 0;
    S = 0;
    mu = 0;
    return *this;
  }
  Variance  *operator+(Variance &Other);
  Variance  *operator+(const double &C);
  Variance  *operator*(const int &C);
  void operator+=(Variance &Other);
  void operator+=(const double &C);
  double Min();
  double Min(Segment &S);
  double Min(MultiSegment &S);
  double ArgMin();
  double ArgMin(Segment &S);
  double ArgMin(MultiSegment &S);
  MultiSegment *IsLowerThan( MultiSegment &, double);
};



#endif
