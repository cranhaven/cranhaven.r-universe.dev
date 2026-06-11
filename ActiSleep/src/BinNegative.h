#ifndef _BinNegative_h_
#define _BinNegative_h_

#include "Constants.h"
#include "Function.h"
#include "Segment.h"



class BinNegative : public Function<Segment, int, MultiSegment>
{
  // Negative Binomial(mu)= A - S.ln(mu) - T.ln(1-mu)

public:
  double A;
  double T;
  double S;
  BinNegative();
  BinNegative(double t, int y);
  BinNegative(double a, double s, double t, bool b=true);
  void ResetMe();
  void ResetMe(double t, int y);
  void ResetMe(double a, double s, double t, bool b);


  void SpecializeMe(int);
  double operator()(double);
  double operator()(int,double);
  double operator[](double);
  MultiSegment *LowerThanZero(MultiSegment &);
  BinNegative operator=(const BinNegative &Model)
  {
    FirstElementSpecified = Model.FirstElementSpecified;
    FirstElement = Model.FirstElement;
    A = Model.A;
    T = Model.T;
    S = Model.S;
    return *this;
  }
  BinNegative operator=(const double &C)
  {
    FirstElementSpecified = true;
    FirstElement = 0;
    A = C;
    T = 0;
    S = 0;
    return *this;
  }
  BinNegative  *operator+(BinNegative &Other);
  BinNegative  *operator+(const double &C);
  BinNegative  *operator*(const int &C);
  void operator+=(BinNegative &Other);
  void operator+=(const double &C);
  void operator*=(int x);
  double Min();
  double Min(Segment &S);
  double Min(MultiSegment &S);
  double ArgMin();
  double ArgMin(Segment &S);
  double ArgMin(MultiSegment &S);
  MultiSegment *IsLowerThan( MultiSegment &, double);
};



#endif
