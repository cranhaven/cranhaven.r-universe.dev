

#ifndef _Trinome_h_
#define _Trinome_h_

#include "Constants.h"
#include "Function.h"
#include "Segment.h"



class Trinome : public Function<Segment, int, MultiSegment>
{
  // Trinome(mu)=(Y-mu)²
public:
  Trinome();
  Trinome(double);
  Trinome(double a, double b, double c);
  void ResetMe();
  void ResetMe(double);
  void ResetMe(double a, double b, double c);

  ~Trinome();
private:
  double a0, a1, a2;
  // getter and setter for a0, a1, a2
  // ax^2 + bx + c
public:
  //unsigned int sizeof(){return 3 * sizeof(double);}
  double Geta0();
  double Geta1();
  double Geta2();

  void SpecializeMe(double);
  double operator()(double);
  double operator()(double,double);
// derivative function
  double operator[](double);
  MultiSegment *LowerThanZero(MultiSegment &);
  Trinome operator=(const Trinome &Model)
  {
    FirstElementSpecified = Model.FirstElementSpecified;
    FirstElement = Model.FirstElement;
    a0 = Model.a0;
    a1 = Model.a1;
    a2 = Model.a2;
    return *this;
  }

  Trinome operator=(const double &C)
  {
    FirstElementSpecified = true;
    FirstElement = 0;
    a0 = C;
    a1 = 0;
    a2 = 0;
    return *this;
  }

  double Min(Segment &S);
  double Min();
  double Min(MultiSegment &S);
  double ArgMin();
  double ArgMin(Segment &S);
  double ArgMin(MultiSegment &S);
  MultiSegment *IsLowerThan( MultiSegment &, double);
  MultiSegment *IsLowerThan(double);
  void operator*=(double x);
  void operator*=(int x);
  void operator+=(Trinome &Other);
  void operator+=(const double C);
  Trinome *operator+(Trinome &Other);
  Trinome *operator+(const double &C);
  Trinome *operator*(const int &C);
};




#endif
