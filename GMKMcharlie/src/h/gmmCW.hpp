# pragma once
# include <RcppParallel.h>
# include "macros.hpp"
# include "G.hpp"
# include "dnyTasking.hpp"
using namespace RcppParallel;




template<typename indtype, typename valtype>
struct paraWeight: public Worker
{
  indtype Xsize;
  indtype phase;
  valtype sum;
  valtype *Xdensity;
  valtype *pointW;
  valtype *rowSum;
  valtype *S;
  valtype *W;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 1024)) break;
      // ========
      for(indtype i = objI, iend = std::min<indtype> (objI + 1024, dT->NofAtom);
          i < iend; ++i)
      {
        if(phase == 0)
        {
          W[i] = 0;
          if(rowSum[i] > 0) W[i] = Xdensity[i] / rowSum[i];
          W[i] *= pointW[i];
          S[st] += W[i];
        }
        else
        {
          W[i] /= sum;
        }
      }
      // ========
    }
  }


  paraWeight(valtype &alphaNew, indtype Xsize, valtype *Xdensity,
             valtype *pointW, valtype *rowSum, valtype *W, int maxCore):
    Xsize(Xsize), Xdensity(Xdensity), pointW(pointW),
    rowSum(rowSum), W(W)
  {
    phase = 0;
    vec<valtype> Scontainer(maxCore, 0);
    S = &Scontainer.front();
    dynamicTasking dt(maxCore, Xsize); dT = &dt;
    parallelFor(0, maxCore, *this);
    sum = std::accumulate(S, S + maxCore, 0.0);
    alphaNew = sum / Xsize;
    phase = 1;
    dT->reset(maxCore, Xsize);
    parallelFor(0, maxCore, *this);
  }
};




template<typename indtype, typename valtype>
struct paraWeightMean: public Worker
{
  valtype *X;
  valtype *W;
  indtype d;
  indtype Xsize;
  valtype *rst;
  valtype **S;
  dynamicTasking *dT;


  void sumOneCPU(indtype cpuI, indtype i, indtype d,
                 valtype **S, valtype *W, valtype *X)
  {
    valtype *mval = X + i * d;
    valtype *a = S[cpuI];
    for(indtype k = 0; k < d; ++k)
    {
      a[k] += W[i] * mval[k];
    }
  }


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 512)) break;
      {
        for(indtype i = objI, iend = std::min<indtype> (i + 512, dT->NofAtom);
            i < iend; ++i)
        {
          sumOneCPU(st, i, d, S, W, X);
        }
      }
    }
  }


  paraWeightMean(valtype *X, valtype *W, indtype d, indtype Xsize, valtype *rst,
                 indtype Ncore): X(X), W(W), d(d), Xsize(Xsize), rst(rst)
  {
    vec<valtype> sumContainer(Ncore * d, 0);
    vec<valtype*> sptrContainer(Ncore);
    S = &sptrContainer.front();
    for(indtype i = 0, k = 0; i < Ncore; ++i, k += d)
    {
      S[i] = &sumContainer[k];
    }
    dynamicTasking dt(Ncore, Xsize); dT = &dt;
    parallelFor(0, Ncore, *this);
    std::fill(rst, rst + d, 0);
    for(indtype i = 0; i < Ncore; ++i)
    {
      valtype *cpu = S[i];
      for(indtype k = 0; k < d;++k)
      {
        rst[k] += cpu[k];
      }
    }
  }
};




template<typename indtype, typename valtype>
struct paraWeightSigma: public Worker
{
  indtype d;
  indtype Xsize;
  valtype *X;
  valtype *W;
  valtype *rst;
  valtype **S;
  valtype *mu;
  vec<valtype> *tmpCntr; // each of size at least d
  dynamicTasking *dT;


  void sumOneCPU(indtype cpuI, indtype i, indtype d, valtype *W, valtype *X, valtype *mu)
  {
    valtype *mval = X + i * d;
    valtype &w = W[i];
    valtype *x_mu = &tmpCntr[cpuI][0];
    for(indtype k = 0; k < d; ++k)
    {
      x_mu[k] = mval[k] - mu[k];
    }
    valtype *a = S[cpuI];
    for(indtype p = 0; p < d; ++p)
    {
      for(indtype q = 0; q <= p; ++q)
      {
        *a += w * x_mu[p] * x_mu[q];
        ++a;
      }
    }
  }


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      sumOneCPU(st, objI, d, W, X, mu);
    }
  }


  paraWeightSigma(indtype d, indtype Xsize, valtype *X, valtype *W, valtype *rst,
                  valtype *mu, indtype Ncore):
    d(d), Xsize(Xsize), X(X), W(W), rst(rst), mu(mu)
  {
    indtype sigmaSize = std::size_t(d) * (d + 1) / 2;
    vec<valtype> sumContainer(sigmaSize * Ncore, 0);
    vec<valtype*> sptrContainer(Ncore);
    S = &sptrContainer.front();
    for(indtype i = 0, k = 0; i < Ncore; ++i, k += sigmaSize)
    {
      S[i] = &sumContainer[k];
    }
    vec<vec<valtype> > tmpContainer(Ncore, vec<valtype>(d, 0));
    tmpCntr = &tmpContainer[0];
    dynamicTasking dt(Ncore, Xsize); dT = &dt;
    parallelFor(0, Ncore, *this);


    std::fill(rst, rst + sigmaSize, 0);
    for(indtype i = 0; i < Ncore; ++i)
    {
      valtype *cpu = S[i];
      for(indtype k = 0; k < sigmaSize; ++k)
      {
        rst[k] += cpu[k];
      }
    }
  }
};




template<typename indtype, typename valtype>
inline valtype oneRowSum(G<indtype, valtype> *gv, indtype gvSize, indtype row)
{
  valtype S = 0;
  for(indtype i = 0; i < gvSize; ++i)
  {
    S += gv[i].ptr[row];
  }
  return S;
}




template<typename indtype, typename valtype>
struct paraSingleGdensityUpdateRowSum: public Worker
{
  indtype d;
  indtype Xsize;
  valtype pi_;
  valtype *X;
  valtype *rowSum;
  G<indtype, valtype> &gaussian;
  vec<valtype> *tmpCntr; // each elemental vector is of size d
  G<indtype, valtype> *gv;
  indtype gvSize; // gv and gvSize re-sum the densities in case numeric issue kicks in
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      {
        valtype tmp = gaussian.densityEval(X + objI * d, d, &tmpCntr[st][0], pi_);
        rowSum[objI] += tmp - gaussian.ptr[objI];
        gaussian.ptr[objI] = tmp;
        if(rowSum[objI] < 0) // numeric issue kicks in
        {
          rowSum[objI] = oneRowSum<indtype, valtype> (gv, gvSize, objI);
        }
      }
    }
  }


  paraSingleGdensityUpdateRowSum(indtype d, indtype Xsize, valtype *X,
                                 valtype *rowSum, G<indtype, valtype> &gaussian,
                                 G<indtype, valtype> *gv, indtype gvSize, indtype NofCPU):
    d(d), Xsize(Xsize), X(X), rowSum(rowSum), gaussian(gaussian), gv(gv), gvSize(gvSize)
  {
    pi_ = std::pow(2.0 * M_PI, d * (-0.5));
    dynamicTasking dt(NofCPU, Xsize);
    dT = &dt;
    vec<vec<valtype> > tmpContainer(NofCPU, vec<valtype> (d, 0));
    tmpCntr = &tmpContainer[0];
    parallelFor(0, NofCPU, *this);
  }
};




template<typename indtype, typename valtype>
struct paraCalDenWithAlphaOneGaussian: public Worker
{
  indtype d;
  indtype Xsize;
  valtype *X;
  valtype pi_;
  G<indtype, valtype> &gau;
  vec<valtype> *tmpCntr;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      gau.ptr[objI] = gau.densityEval(X + objI * d, d, &tmpCntr[st][0], pi_);
    }
  }
  paraCalDenWithAlphaOneGaussian(
    indtype d, indtype Xsize, valtype *X, G<indtype, valtype> &gau, indtype maxCore):
    d(d), Xsize(Xsize), X(X), gau(gau)
  {
    pi_ = std::pow(2.0 * M_PI, d * (-0.5));
    dynamicTasking dt(maxCore, Xsize); dT = &dt;
    vec<vec<valtype> > tmpContainer(maxCore, vec<valtype> (d, 0));
    tmpCntr = &tmpContainer[0];
    parallelFor(0, maxCore, *this);
  }
};









