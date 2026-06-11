


#ifndef _Exponential_h_
#define _Exponential_h_


#include "Function.h"
#include "Segment.h"

class Exponential:public Function<Segment, int, MultiSegment>
{
public:
  double A;
  double B;
  double S;
 // unsigned int sizeof(){return 3 * sizeof(double);}
  Exponential();
  Exponential(double, double, double);
  // The following constructor sets values for A and FirstElement
  Exponential(double);
  // Exponential(mu) = Y.mu -ln(mu)
  // Exponential(mu) = S.mu -B.ln(mu) + A (as we sum and compare over time)

  void ResetMe();
  void ResetMe(double, double, double);
  void ResetMe(double);

  void SpecializeMe(double);
  double operator()(double,  double);
  double operator()(double);
  double operator[](double);
  void operator*=(int x);
  Exponential operator=(const Exponential &Model)
  {
    FirstElementSpecified = Model.FirstElementSpecified;
    FirstElement = Model.FirstElement;
    A = Model.A;
    B = Model.B;
    S = Model.S;
    return *this;
  }
  Exponential operator=(const double &C)
  {
    FirstElementSpecified = true;
    FirstElement = 0;
    A = C;
    B = 0;
    S = 0;
    return *this;
  }
  void operator+=(const Exponential &Other);
  void operator+=(const double &C);
  Exponential *operator+(const Exponential &Other);
  Exponential *operator+(const double &C);
  Exponential *operator%(const int &C);
  double Min();
  double Min(Segment &S);
  double Min(MultiSegment &S);
  double ArgMin();
  double ArgMin(Segment &S);
  double ArgMin(MultiSegment &S);
  MultiSegment *LowerThanZero(MultiSegment &S);
  MultiSegment *IsLowerThan(MultiSegment &, double);
  MultiSegment *IsLowerThan(double);
};




#endif
