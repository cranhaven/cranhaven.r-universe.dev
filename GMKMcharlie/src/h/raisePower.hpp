# pragma once


inline double raisePower(double x, int p)
{
  double r = x;
  for(int i = 1; i < p; ++i) x *= r;
  return x;
}


