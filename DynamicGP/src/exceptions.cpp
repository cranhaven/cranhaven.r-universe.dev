#include<iostream>
#include<exceptions.hpp>
std::ostream& operator<<(std::ostream& os, cholException& rhs)
{
  os<<rhs.file<<":"<<rhs.line<<" error in Cholesky, info="
    <<rhs.info<<", g="<<rhs.g<<"\n";
  os<<"d=("<<rhs.d[0];
  for(int i = 1; i < rhs.dim; ++i)
    os<<","<<rhs.d[i];
  os<<")\n";
  return os;
}

std::ostream& operator<<(std::ostream& os, optException& rhs)
{
  os<<rhs.file<<":"<<rhs.line<<" error in optimization of nugget, min="
    <<rhs.min<<" max="<<rhs.max<<"\n";
  return os;
}

std::ostream& operator<<(std::ostream& os, svdException& rhs)
{
  os<<rhs.file<<":"<<rhs.line<<" error in SVD, info="<<rhs.info<<"\n";
  return os;
}
