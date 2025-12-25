// // [[Rcpp::plugins(cpp11)]]
// # include <Rcpp.h>
// using namespace Rcpp;
// # define vec std::vector
# include "macros.hpp"
# include "raisePower.hpp"




template<typename indtype, typename valtype>
struct E
{
  indtype size, *region;
  valtype weight, l2norm, mag, *loss;
  E(){size = 0; weight = 1; l2norm = 0; mag = 0; region = nullptr; loss = nullptr;}
};


namespace distances {


// ee == 0: x and y are both dense events.
// ee == 1: x is sparse and y is dense.
// ee == 2: x is dense and y is sparse.
// ee == 3: x and y are both sparse events.
// X and Y are derived classes of E.
template<typename X, typename Y, int ee,
         typename indtype, typename valtype, bool takeRoot>
inline valtype eucD(X &x, Y &y)
{
  valtype S = 0;


  if(ee == 0) // dense-dense
  {
    for(indtype i = 0; i < x.size; ++i)
    {
      valtype tmp = x.loss[i] - y.loss[i];
      S += tmp * tmp;
    }
  }
  else if(ee == 3) // sparse-sparse
  {
    indtype i = 0, j = 0, iend = x.size, jend = y.size;
    for( ; i < iend and j < jend; )
    {
      if(x.region[i] < y.region[j])
      {
        S += x.loss[i] * x.loss[i];
        ++i;
      }
      else if(x.region[i] > y.region[j])
      {
        S += y.loss[j] * y.loss[j];
        ++j;
      }
      else
      {
        valtype tmp = x.loss[i] - y.loss[j];
        S += tmp * tmp;
        ++i; ++j;
      }
    }
    for(; j < jend; ++j) S += y.loss[j] * y.loss[j];
    for(; i < iend; ++i) S += x.loss[i] * x.loss[i];
  }
  else if(ee == 1) // sparse-dense
  {
    for(indtype i = 0, iend = x.size; i < iend; ++i)
    {
      indtype k = x.region[i];
      S += x.loss[i] * (x.loss[i] - 2 * y.loss[k]);
    }
    S += y.mag;
  }
  else
  {
    for(indtype i = 0, iend = y.size; i < iend; ++i)
    {
      indtype k = y.region[i];
      S += y.loss[i] * (y.loss[i] - 2 * x.loss[k]);
    }
    S += x.mag;
  }


  S *= x.weight * y.weight;
  if(takeRoot) S = std::sqrt(S);
  return S;
}




template<typename X, typename Y, int ee, typename indtype, typename valtype>
inline valtype l1D(X &x, Y &y)
{
  valtype S = 0;


  if(ee == 0)
  {
    for(indtype i = 0; i < x.size; ++i)
      S += std::abs(x.loss[i] - y.loss[i]);
  }
  else if(ee == 3)
  {
    indtype i = 0, j = 0, iend = x.size, jend = y.size;
    for( ; i < iend and j < jend; )
    {
      if(x.region[i] < y.region[j])
      {
        S += std::abs(x.loss[i]);
        ++i;
      }
      else if(x.region[i] > y.region[j])
      {
        S += std::abs(y.loss[j]);
        ++j;
      }
      else
      {
        S += std::abs(x.loss[i] - y.loss[j]);
        ++i; ++j;
      }
    }
    for(; j < jend; ++j) S += std::abs(y.loss[j]);
    for(; i < iend; ++i) S += std::abs(x.loss[i]);
  }
  else if(ee == 1)
  {
    for(indtype i = 0, iend = x.size; i < iend; ++i)
    {
      indtype k = x.region[i];
      S += std::abs(x.loss[i] - y.loss[k]) - std::abs(y.loss[k]);
    }
    S += y.mag;
  }
  else
  {
    for(indtype i = 0, iend = y.size; i < iend; ++i)
    {
      indtype k = y.region[i];
      S += std::abs(y.loss[i] - x.loss[k]) - std::abs(x.loss[k]);
    }
    S += x.mag;
  }


  S *= x.weight * y.weight;
  return S;
}




template<typename X, typename Y, int ee, typename indtype, typename valtype>
inline valtype maxD(X &x, Y &y)
{
  valtype S = 0;
  if(ee == 0)
  {
    for(indtype i = 0; i < x.size; ++i)
      S = std::max(S, std::abs(x.loss[i] - y.loss[i]));
  }
  else if(ee == 3)
  {
    indtype i = 0, j = 0, iend = x.size, jend = y.size;
    for(; i < x.size and j < y.size; )
    {
      if(x.region[i] < y.region[j])
      {
        S = std::max(S, std::abs(x.loss[i]));
        ++i;
      }
      else if(x.region[i] > y.region[j])
      {
        S = std::max(S, std::abs(y.loss[j]));
        ++j;
      }
      else
      {
        S = std::max(S, std::abs(x.loss[i] - y.loss[j]));
        ++i; ++j;
      }
    }
    for(; j < jend; ++j) S = std::max(S, std::abs(y.loss[j]));
    for(; i < iend; ++i) S = std::max(S, std::abs(x.loss[i]));
  }
  else if(ee == 1)
  {
    indtype j = 0;
    for(indtype i = 0, iend = y.size; i < iend; ++i)
    {
      if(j >= x.size or i < x.region[j])
        S = std::max<valtype> (S, std::abs(y.loss[i]));
      else
      {
        S = std::max<valtype> (S, std::abs(y.loss[i] - x.loss[j]));
        ++j;
      }
    }
  }
  else
  {

    indtype j = 0;
    for(indtype i = 0, iend = x.size; i < iend; ++i)
    {
      if(j >= y.size or i < y.region[j])
        S = std::max<valtype> (S, std::abs(x.loss[i]));
      else
      {
        S = std::max<valtype> (S, std::abs(x.loss[i] - y.loss[j]));
        ++j;
      }
    }

  }



  S *= x.weight * y.weight;
  return S;
}




template<typename X, typename Y, int ee, typename indtype, typename valtype>
inline valtype cosineD(X &x, Y &y)
{
  valtype S = 0;
  if(x.l2norm == 0) x.l2norm = std::sqrt(std::inner_product(x.loss, x.loss + x.size, x.loss, 0.0));
  if(y.l2norm == 0) y.l2norm = std::sqrt(std::inner_product(y.loss, y.loss + y.size, y.loss, 0.0));


  if(ee == 0)
  {
    S = 1 - std::inner_product(
      x.loss, x.loss + x.size, y.loss, 0.0) / (x.l2norm * y.l2norm);
  }
  else if(ee == 3)
  {
    indtype i = 0, j = 0;
    for( ; i < x.size and j < y.size; )
    {
      if(x.region[i] < y.region[j]) ++i;
      else if(x.region[i] > y.region[j]) ++j;
      else
      {
        S += x.loss[i] * y.loss[j];
        ++i; ++j;
      }
    }
    S = 1 - S / (x.l2norm * y.l2norm);
  }
  else if(ee == 1)
  {
    for(indtype i = 0; i < x.size; ++i)
    {
      indtype k = x.region[i];
      S += x.loss[i] * y.loss[k];
    }
    S = 1 - S / (x.l2norm * y.l2norm);
  }
  else
  {
    for(indtype i = 0; i < y.size; ++i)
    {
      indtype k = y.region[i];
      S += y.loss[i] * x.loss[k];
    }
    S = 1 - S / (y.l2norm * x.l2norm);
  }


  S *= x.weight * y.weight;
  return S;
}




template<typename X, typename Y, int ee, typename indtype, typename valtype, bool takeRoot>
inline valtype minkDint(X &x, Y &y, valtype p)
{
  valtype S = 0;
  if(ee == 0)
  {
    for(indtype i = 0; i < x.size; ++i)
      S += raisePower(std::abs(x.loss[i] - y.loss[i]), p);
  }
  else if(ee == 3)
  {
    indtype i = 0, j = 0, iend = x.size, jend = y.size;
    for( ; i < iend and j < jend; )
    {
      if(x.region[i] < y.region[j])
      {
        S += raisePower(std::abs(x.loss[i]), p);
        ++i;
      }
      else if(x.region[i] > y.region[j])
      {
        S += raisePower(std::abs(y.loss[j]), p);
        ++j;
      }
      else
      {
        S += raisePower(std::abs(x.loss[i] - y.loss[j]), p);
        ++i; ++j;
      }
    }
    for(; j < jend; ++j) S += raisePower(std::abs(y.loss[j]), p);
    for(; i < iend; ++i) S += raisePower(std::abs(x.loss[i]), p);
  }
  else if(ee == 1)
  {
    for(indtype i = 0, iend = x.size; i < iend; ++i)
    {
      indtype k = x.region[i];
      S += raisePower(std::abs(x.loss[i] - y.loss[k]), p) - raisePower(std::abs(y.loss[k]), p);
    }
    S += y.mag;
  }
  else
  {
    for(indtype i = 0, iend = y.size; i < iend; ++i)
    {
      indtype k = y.region[i];
      S += raisePower(std::abs(y.loss[i] - x.loss[k]), p) - raisePower(std::abs(x.loss[k]), p);
    }
    S += x.mag;
  }


  S *= x.weight * y.weight;
  if(takeRoot) S = std::pow(S, 1.0 / p);
  return S;
}




template<typename X, typename Y, int ee, typename indtype, typename valtype, bool takeRoot>
inline valtype minkDfloat(X &x, Y &y, valtype p) // minkD() where p is not integer.
{
  valtype S = 0;
  if(ee == 0)
  {
    for(indtype i = 0; i < x.size; ++i)
    {
      valtype tmp = std::abs(x.loss[i] - y.loss[i]);
      S += std::pow(tmp, p);
    }
  }
  else if(ee == 3)
  {
    indtype i = 0, j = 0, iend = x.size, jend = y.size;
    for( ; i < iend and j < jend; )
    {
      if(x.region[i] < y.region[j])
      {
        S += std::pow(std::abs(x.loss[i]), p);
        ++i;
      }
      else if(x.region[i] > y.region[j])
      {
        S += std::pow(std::abs(y.loss[j]), p);
        ++j;
      }
      else
      {
        S += std::pow(std::abs(x.loss[i] - y.loss[j]), p);
        ++i; ++j;
      }
    }
    for(; j < jend; ++j) S += std::pow(std::abs(y.loss[j]), p);
    for(; i < iend; ++i) S += std::pow(std::abs(x.loss[i]), p);
  }
  else if(ee == 1)
  {
    for(indtype i = 0, iend = x.size; i < iend; ++i)
    {
      indtype k = x.region[i];
      S += std::pow(std::abs(x.loss[i] - y.loss[k]), p) - std::pow(std::abs(y.loss[k]), p);
    }
    S += y.mag;
  }
  else
  {
    for(indtype i = 0, iend = y.size; i < iend; ++i)
    {
      indtype k = y.region[i];
      S += std::pow(std::abs(y.loss[i] - x.loss[k]), p) - std::pow(std::abs(x.loss[k]), p);
    }
    S += x.mag;
  }


  S *= x.weight * y.weight;
  if(takeRoot) S = std::pow(S, 1.0 / p);
  return S;
}


}




// beta == -1: cosine dissimilarity.
// beta == 0: max norm.
// beta == 1: l1 norm.
// beta == 2: Euc norm.
// beta == 3: p is integer and 3 <= p <= 35.
// beta == 4: p is real or p >= 36.
// ee == 0: dense-dense.
// ee == 1: sparse-dense.
// ee == 2: dense-sparse.
// ee == 3: sparse-sparse.
template<typename X, typename Y, int ee, typename indtype, typename valtype, int beta, bool takeRoot>
inline valtype minkD(X &x, Y &y, valtype p = 0)
{
  if(beta == -1)  return distances::cosineD  <X, Y, ee, indtype, valtype>           (x, y);
  if(beta ==  0)  return distances::maxD     <X, Y, ee, indtype, valtype>           (x, y);
  if(beta ==  1)  return distances::l1D      <X, Y, ee, indtype, valtype>           (x, y);
  if(beta ==  2)  return distances::eucD     <X, Y, ee, indtype, valtype, takeRoot> (x, y);
  if(beta ==  3)  return distances::minkDint <X, Y, ee, indtype, valtype, takeRoot> (x, y, p);
  return distances::minkDfloat               <X, Y, ee, indtype, valtype, takeRoot> (x, y, p);
}































