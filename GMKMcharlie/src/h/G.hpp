# pragma once
# include <Rcpp.h>
# include "macros.hpp"


template<typename indtype, typename valtype>
struct G
{
  bool updateAlpha, updateMean, updateSigma; // valtype piConstMul;
  valtype alpha; // Weight on this kernel.
  valtype sqrtOfDet; // Square root of the covariance matrix's determinant.
  valtype logSqrtOfDet;
  vec<valtype> mu; // Kernel's center.
  vec<valtype> cholU; // Upper triangle matrix from Cholesky decomposition of the covariance matrix.
  vec<valtype> ptr; // Densities this kernel projects at a set of points in space. Could be an empty vector.


  inline void computeCholUandSqrtOfDet(vec<valtype> &sigma);
  inline void computeCholUandSqrtOfDet(valtype *sigma);
  inline void computeCholUandLogSqrtOfDet(vec<valtype> &sigma);
  inline void computeCholUandLogSqrtOfDet(valtype *sigma);


  inline void shallowCopy(G &x) // Copy parameters of the kernel but not the densities it projects.
  {
    updateAlpha = x.updateAlpha;
    updateMean = x.updateMean;
    updateSigma = x.updateSigma;
    alpha = x.alpha;
    sqrtOfDet = x.sqrtOfDet;
    logSqrtOfDet = x.logSqrtOfDet;
    mu = x.mu;
    cholU = x.cholU;
  }


  // For debugging
  inline void print(bool convertToSigma = 1)
  {
    Rcpp::Rcout << alpha << "    ";
    for(indtype i = 0, iend = mu.size(); i < iend;++i)
    {
      Rcpp::Rcout << mu[i] <<" ";
    }

    Rcpp::Rcout << "    ";
    if(convertToSigma)
    {
      vec<valtype> tmp(mu.size() * mu.size());
      vec<valtype*> tmp2(mu.size());
      triCholToFullSigma(&cholU[0], &tmp[0], mu.size(), &tmp2[0]);
      for(indtype i = 0, iend = tmp.size(); i < iend; ++i)
      {
        Rcpp::Rcout << tmp[i] << " ";
      }
    }
    else
    {
      for(indtype i = 0, iend = cholU.size(); i < iend; ++i)
      {
        Rcpp::Rcout << cholU[i] <<" ";
      }
    }
    Rcpp::Rcout << "\n";
    Rcpp::Rcout << "density container: ";
    for(indtype i = 0, iend = ptr.size(); i < iend; ++i)
    {
      Rcpp::Rcout << ptr[i] << " ";
    }
    Rcpp::Rcout << "\n\n";
  }


  // M is a tempoarary vector of size d, the dimensionality of a data point
  // inline valtype densityEval(valtype *x, indtype d, valtype *M, indtype withAlphaWithPi = 3)
  inline valtype densityEval(valtype *x, indtype d, valtype *M, valtype piConstMul)
  {
    // if(sqrtOfDet <= 0) return 0;


    valtype *mean = &*mu.begin();
    valtype *cl = &*cholU.begin();


    // Mahalanobis distance computation
    valtype *Mst = M;
    valtype *Mbegin = Mst;
    valtype *Mend = M + d;


    *Mst = (*x - *mean) / *cl;
    valtype mahanaD = *Mst * *Mst;
    indtype rowLen = 1;
    while(true)
    {
      ++Mst;
      if(Mst >= Mend) break;
      ++x;
      ++mean;
      cl += rowLen;
      ++rowLen;
      valtype numerator = *x - *mean - std::inner_product(Mbegin, Mst, cl, 0.0);
      valtype denominator = *(cl + (Mst - Mbegin));
      *Mst = numerator / denominator;
      mahanaD += *Mst * *Mst;
    }


    valtype rst = std::exp(-mahanaD / 2) / sqrtOfDet;
    return rst * alpha * piConstMul;
  }


  inline valtype logdensityEval(valtype *x, indtype d, valtype *M, valtype logPiConstMul)
  {
    valtype *mean = &*mu.begin();
    valtype *cl = &*cholU.begin();


    // Mahalanobis distance computation
    valtype *Mst = M;
    valtype *Mbegin = Mst;
    valtype *Mend = M + d;


    *Mst = (*x - *mean) / *cl;
    valtype mahanaD = *Mst * *Mst;
    indtype rowLen = 1;
    while(true)
    {
      ++Mst;
      if(Mst >= Mend) break;
      ++x;
      ++mean;
      cl += rowLen;
      ++rowLen;
      valtype numerator = *x - *mean - std::inner_product(Mbegin, Mst, cl, 0.0);
      valtype denominator = *(cl + (Mst - Mbegin));
      if(numerator == 0 and denominator == 0)
      {
        logSqrtOfDet = std::numeric_limits<valtype>::lowest();
        return -1;
      }
      *Mst = numerator / denominator;
      mahanaD += *Mst * *Mst;
    }
    return -mahanaD / 2 + std::log(alpha) + logPiConstMul - logSqrtOfDet;
  }


};













