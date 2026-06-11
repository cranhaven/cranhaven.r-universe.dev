


#ifndef _Poisson_h_
#define _Poisson_h_


#include "Function.h"
#include "Segment.h"

class Poisson:public Function<Segment, int, MultiSegment>
{
public:
  double A;
  double B;
  double S;
 // unsigned int sizeof(){return 3 * sizeof(double);}
  Poisson();
  Poisson(double, double, double);
  // The following constructor sets values for A and FirstElement
  Poisson(int);
  // Poisson(mu) = A + mu - Y.ln(mu)

  void ResetMe();
  void ResetMe(double, double, double);
  void ResetMe(int);

  void SpecializeMe(int);
  double operator()(int,  double);
  double operator()(double);
  double operator[](double);
  void operator*=(int x);
  Poisson operator=(const Poisson &Model)
  {
    FirstElementSpecified = Model.FirstElementSpecified;
    FirstElement = Model.FirstElement;
    A = Model.A;
    B = Model.B;
    S = Model.S;
    return *this;
  }
  Poisson operator=(const double &C)
  {
    FirstElementSpecified = true;
    FirstElement = 0;
    A = C;
    B = 0;
    S = 0;
    return *this;
  }
  void operator+=(const Poisson &Other);
  void operator+=(const double &C);
  Poisson *operator+(const Poisson &Other);
  Poisson *operator+(const double &C);
  Poisson *operator%(const int &C);
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
