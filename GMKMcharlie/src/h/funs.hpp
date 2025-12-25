# pragma once
// # include <Rcpp.h>
# include <RcppParallel.h>
# include <RcppArmadillo.h>
# include "macros.hpp"
# include "G.hpp"
# include "dnyTasking.hpp"
using namespace Rcpp;
using namespace RcppParallel;


// Cholesky decomposition to the same memory block. M is half of a
// symmetric matrix. This function Cholesky-decomposes the latent full M of
// dimensionality d, and stores the upper-triangle component in M. Such design
// intends to avoid using extra memory and to enhance cache locality.
// template<typename indtype, typename valtype>
// inline bool cholTriSelf(valtype *M, indtype d)
// {
//   if(*M <= 0) return 0;
//   valtype *&CL = M;
//   *CL = std::sqrt(*M);
//
//
//   valtype *currentRowBegin = M + 1;
//   indtype rowLen = 2;
//
//
//   while(rowLen <= d)
//   {
//     *currentRowBegin = *currentRowBegin / *CL;
//     valtype *priorRowBegin = CL + 1;
//     indtype priorRowLen = 2;
//     indtype i = 1;
//
//
//     for(indtype iend = rowLen - 1; i < iend; ++i)
//     {
//       currentRowBegin[i] = (currentRowBegin[i] - std::inner_product(
//         priorRowBegin, priorRowBegin + priorRowLen - 1, currentRowBegin, 0.0)) /
//           priorRowBegin[priorRowLen - 1];
//       priorRowBegin += priorRowLen;
//       ++priorRowLen;
//     }
//
//
//     // Reach the diagnol
//     valtype tmp = currentRowBegin[i] - std::inner_product(
//       priorRowBegin, priorRowBegin + priorRowLen - 1, currentRowBegin, 0.0);
//     if(tmp <= 0) return 0;
//     currentRowBegin[i] = std::sqrt(tmp);
//
//
//     // Go to next row
//     currentRowBegin += rowLen;
//     ++rowLen;
//   }
//
//
//   return 1;
// }




template<typename indtype, typename valtype>
inline void cholTriSelf(valtype *M, indtype d)
{
  valtype *&CL = M;
  *CL = std::sqrt(*M);


  valtype *currentRowBegin = M + 1;
  indtype rowLen = 2;


  while(rowLen <= d)
  {
    *currentRowBegin = *currentRowBegin / *CL;
    valtype *priorRowBegin = CL + 1;
    indtype priorRowLen = 2;
    indtype i = 1;


    for(indtype iend = rowLen - 1; i < iend; ++i)
    {
      currentRowBegin[i] = (currentRowBegin[i] - std::inner_product(
        priorRowBegin, priorRowBegin + priorRowLen - 1, currentRowBegin, 0.0)) /
          priorRowBegin[priorRowLen - 1];
      priorRowBegin += priorRowLen;
      ++priorRowLen;
    }


    // Reach the diagnol
    valtype tmp = currentRowBegin[i] - std::inner_product(
      priorRowBegin, priorRowBegin + priorRowLen - 1, currentRowBegin, 0.0);
    currentRowBegin[i] = std::sqrt(tmp);


    // Go to next row
    currentRowBegin += rowLen;
    ++rowLen;
  }
}




template<typename indtype, typename valtype>
inline void G<indtype, valtype>::computeCholUandSqrtOfDet(vec<valtype> &sigma)
  // sigma is triangular, half of the full covariance matrix.
{
  indtype d = mu.size();
  cholTriSelf(&sigma.front(), d);


  if(&sigma != &cholU) std::swap(sigma, cholU);


  sqrtOfDet = 1;
  valtype *begin = &cholU.front();
  for(indtype rowLen = 1, offset = 0; rowLen <= d; ++rowLen)
  {
    offset += rowLen;
    sqrtOfDet *= begin[offset - 1];
  }
}




template<typename indtype, typename valtype>
inline void G<indtype, valtype>::computeCholUandSqrtOfDet(valtype *sigma)
  // sigma is triangular, half of the full covariance matrix.
  // Swap sigma and cholU first
  // After this function, sigma contains the old covariance matrix.
{
  for(indtype i = 0, iend = cholU.size(); i < iend; ++i)
  {
    std::swap(sigma[i], cholU[i]);
  }


  indtype d = mu.size();
  cholTriSelf(&cholU[0], d);


  sqrtOfDet = 1;
  valtype *begin = &cholU.front();
  for(indtype rowLen = 1, offset = 0; rowLen <= d; ++rowLen)
  {
    offset += rowLen;
    sqrtOfDet *= begin[offset - 1];
  }
}




template<typename indtype, typename valtype>
inline valtype logSqrtOfDetCholU(valtype *cholU, indtype dim)
{
  valtype logSqrtOfDet = 0;
  valtype negInf = std::numeric_limits<valtype>::lowest();
  for(indtype rowLen = 1, offset = 0; rowLen <= dim; ++rowLen)
  {
    offset += rowLen;
    valtype val = cholU[offset - 1];
    if(val <= 0) return(negInf);
    logSqrtOfDet += std::log(val);
  }
  return logSqrtOfDet;
}




template<typename indtype, typename valtype>
inline void G<indtype, valtype>::computeCholUandLogSqrtOfDet(vec<valtype> &sigma)
  // sigma is triangular, half of the full covariance matrix.
{
  indtype d = mu.size();
  cholTriSelf(&sigma.front(), d);
  if(&sigma != &cholU) std::swap(sigma, cholU);
  logSqrtOfDet = logSqrtOfDetCholU(&cholU[0], d);
}




template<typename indtype, typename valtype>
inline void G<indtype, valtype>::computeCholUandLogSqrtOfDet(valtype *sigma)
  // sigma is triangular, half of the full covariance matrix.
  // Swap sigma and cholU first
  // After this function, sigma contains the old covariance matrix.
{
  for(indtype i = 0, iend = cholU.size(); i < iend; ++i)
    std::swap(sigma[i], cholU[i]);
  indtype d = mu.size();
  cholTriSelf(&cholU[0], d);
  logSqrtOfDet = logSqrtOfDetCholU(&cholU[0], d);
}




template<typename indtype, typename valtype>
inline void fullSigmaToTriSigma(valtype *full, valtype *tri, indtype d)
{
  indtype endCol = 1;
  for(valtype *fullEnd = full + d * d; full < fullEnd; full += d)
  {
    std::copy(full, full + endCol, tri);
    tri += endCol;
    ++endCol;
  }
}




// CL is of length (d + 1) * d / 2, M is of length d ^ 2
template<typename indtype, typename valtype>
inline void triCholToFullSigma(valtype *CL, valtype *M, indtype d, valtype **rowPtr)
  // rowPtr is of size d
{
  rowPtr[0] = CL;
  indtype rowLen = 1;
  for(indtype i = 1, &iend = d; i < iend; ++i)
  {
    rowPtr[i] = rowPtr[i - 1] + rowLen;
    ++rowLen;
  }


  // Decomposed triangle matrix multiplication
  valtype *MrowI = M;
  indtype whichrow = 0;
  while(whichrow < d)
  {
    for(indtype i = 0; i <= whichrow; ++i)
    {
      MrowI[i] = std::inner_product(rowPtr[whichrow], rowPtr[whichrow] + i + 1, rowPtr[i], 0.0);
    }
    MrowI += d;
    ++whichrow;
  }


  // fill the other half of M
  for(indtype col = 0, colEnd = d - 1; col < colEnd; ++col)
  {
    valtype *colStart = M + col * d;
    valtype *colOffset = M + col;
    for(indtype row = col + 1, &rowEnd = d, rowTimesD = row * d;
        row < rowEnd; ++row, rowTimesD += d)
    {
      colStart[row] = colOffset[rowTimesD];
    }
  }
}




template<typename indtype, typename valtype>
inline void shallowCopy(vec<G<indtype, valtype> > &x, vec<G<indtype, valtype> > &dest)
{
  vec<G<indtype, valtype> > tmp(x.size());
  for(indtype i = 0, iend = x.size(); i < iend; ++i)
    tmp[i].shallowCopy(x[i]);
  dest.swap(tmp);
}




// std::swap() won't shortcut for STL container within class
template<typename indtype, typename valtype>
inline void swapG(G<indtype, valtype> &x, G<indtype, valtype> &y)
{
  std::swap(x.updateAlpha, y.updateAlpha);
  std::swap(x.updateMean, y.updateMean);
  std::swap(x.updateSigma, y.updateSigma);
  std::swap(x.alpha, y.alpha);
  std::swap(x.sqrtOfDet, y.sqrtOfDet);
  std::swap(x.logSqrtOfDet, y.logSqrtOfDet);
  std::swap(x.mu, y.mu);
  std::swap(x.cholU, y.cholU);
  std::swap(x.ptr, y.ptr);
}




// given a gaussian mixture, annihilate components with alpha < annihilationEPS
template<typename indtype, typename valtype>
inline void annihilateGinVec(vec<G<indtype, valtype> > &gv, valtype annihilationEPS)
{
  if(annihilationEPS <= 0) return;
  vec<G<indtype, valtype> > rst(gv.size());
  indtype j = 0;
  for(indtype i = 0, iend = gv.size(); i < iend; ++i)
  {
    if(gv[i].alpha > annihilationEPS)
    {
      swapG(gv[i], rst[j]);
      ++j;
    }
  }
  rst.resize(j);
  std::swap(gv, rst);
}


template<typename indtype, typename valtype>
inline void earseCollapsedGau(vec<G<indtype, valtype> > &gv)
{
  vec<G<indtype, valtype> > rst(gv.size());
  indtype j = 0;
  valtype negInf = std::numeric_limits<valtype>::lowest();
  for(indtype i = 0, iend = gv.size(); i < iend; ++i)
  {
    if(gv[i].logSqrtOfDet != negInf)
    {
      swapG(gv[i], rst[j]);
      ++j;
    }
  }
  rst.resize(j);
  std::swap(gv, rst);
}




template<typename indtype, typename valtype>
inline void eraseComponent(vec<G<indtype, valtype> > &gv, indtype I)
{
  vec<valtype>(0).swap(gv[I].mu);
  vec<valtype>(0).swap(gv[I].cholU);
  vec<valtype>(0).swap(gv[I].ptr);
  for(indtype i = I, iend = gv.size() - 1; i < iend; ++i)
    swapG(gv[i], gv[i + 1]);
  gv.resize(gv.size() - 1);
}




// If ind[i] is false, gv[i] will be erased
template<typename indtype, typename valtype>
inline void eraseComponent(vec<G<indtype, valtype> > &gv, bool *ind)
{
  indtype siz = std::accumulate(ind, ind + gv.size(), indtype(0));
  if(siz - gv.size() == 0) return;
  vec<G<indtype, valtype> > rst(siz);
  indtype k = 0;
  for(indtype i = 0, iend = gv.size(); i < iend; ++i)
  {
    if(ind[i])
    {
      swapG(gv[i], rst[k]);
      ++k;
    }
  }
  rst.swap(gv);
}




template<typename indtype, typename valtype>
struct cmptDensity: public Worker
{
  indtype d;
  indtype Xsize;
  indtype gmodelSize;
  valtype pi_;
  valtype *X;
  G<indtype, valtype> *gmodel;
  vec<valtype> *tmpCtnr;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 500)) break;
      for(std::size_t I = objI,
          Iend = std::min<std::size_t> (dT->NofAtom, I + 500);
          I < Iend; ++I)
      {
        std::size_t whichModel = I / Xsize, offset = I % Xsize;
        G<indtype, valtype> &gaussian = gmodel[whichModel];
        gaussian.ptr[offset] = gaussian.densityEval(X + offset * d, d, &tmpCtnr[st][0], pi_);
      }
    }
  }


  cmptDensity(indtype d, indtype Xsize, indtype gmodelSize, valtype *X,
              G<indtype, valtype> *gmodel, indtype NofCPU):
    d(d), Xsize(Xsize), gmodelSize(gmodelSize), X(X), gmodel(gmodel)
  {
    pi_ =std::pow(2.0 * M_PI, d * (-0.5));
    vec<vec<valtype> > tmpContainer(NofCPU, vec<valtype> (d, 0));
    tmpCtnr = &tmpContainer[0];
    dynamicTasking dt(NofCPU, std::size_t(gmodelSize) * Xsize);
    dT = &dt;
    parallelFor(0, NofCPU, *this);
  }
};


template<typename indtype, typename valtype>
struct cmptRowSum: public Worker
{
  indtype Xsize; // Number of rows.
  // indtype gmodelSize; // Number of columns.
  G<indtype, valtype> *gmodel;
  valtype **auxC;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      for(indtype i = 0; i < Xsize; ++i)
        auxC[st][i] += gmodel[objI].ptr[i];
    }
  }


  cmptRowSum(indtype Xsize, indtype gmodelSize,
             G<indtype, valtype> *gmodel, valtype *rowSum,
             vec<valtype> &auxCntr_, indtype NofCPU):
    Xsize(Xsize), // gmodelSize(gmodelSize),
    gmodel(gmodel)
  {
    auxCntr_.assign(std::size_t(Xsize) * (NofCPU - 1), 0);
    std::fill(rowSum, rowSum + Xsize, 0);
    vec<valtype*> auxCntr(NofCPU);
    auxC = &auxCntr[0];
    auxC[0] = rowSum;
    for(indtype i = 1; i < NofCPU; ++i)
      auxC[i] = &auxCntr_[0] + std::size_t(i - 1) * Xsize;
    dynamicTasking dt(NofCPU, gmodelSize); dT = &dt;
    parallelFor(0, NofCPU, *this);
    for(indtype i = 1; i < NofCPU; ++i)
    {
      valtype *x = auxC[i];
      for(indtype j = 0; j < Xsize; ++j)
        rowSum[j] += x[j];
    }
  }
};


template<typename indtype, typename valtype>
struct cmptLogDensity: public Worker
{
  indtype d;
  indtype Xsize;
  indtype gmodelSize;
  valtype logPi_;
  valtype *X;
  G<indtype, valtype> *gmodel;
  vec<valtype> *tmpCtnr;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 512)) break;
      for(std::size_t I = objI,
          Iend = std::min<std::size_t> (dT->NofAtom, I + 512);
          I < Iend; ++I)
      {
        std::size_t whichModel = I / Xsize, offset = I % Xsize;
        G<indtype, valtype> &gaussian = gmodel[whichModel];
        gaussian.ptr[offset] = gaussian.logdensityEval(X + offset * d, d, &tmpCtnr[st][0], logPi_);
      }
    }
  }


  cmptLogDensity(indtype d, indtype Xsize, indtype gmodelSize, valtype *X,
                 G<indtype, valtype> *gmodel, indtype NofCPU):
    d(d), Xsize(Xsize), gmodelSize(gmodelSize), X(X), gmodel(gmodel)
  {
    // pi_ = std::pow(2.0 * M_PI, d * (-0.5));
    logPi_ = std::log(2.0 * M_PI) * d * (-0.5);
    vec<vec<valtype> > tmpContainer(NofCPU, vec<valtype> (d, 0));
    tmpCtnr = &tmpContainer[0];
    dynamicTasking dt(NofCPU, std::size_t(gmodelSize) * Xsize);
    dT = &dt;
    parallelFor(0, NofCPU, *this);
  }
};




template<typename indtype, typename valtype>
struct cmptDensityGivenLogDenistyAndRowSum: public Worker
{
  indtype gmodelSize;
  G<indtype, valtype> *gmodel;
  valtype *rowSum, *logRowMax;
  dynamicTasking *dT;


  void vecMax(valtype *max, valtype *x, indtype size)
  {
    for(indtype i = 0; i < size; ++i)
      max[i] = std::max(max[i], x[i]);
  }


  void compden(indtype rowSt, indtype rowEnd)
  {
    std::copy(&gmodel[0].ptr[rowSt], &gmodel[0].ptr[rowEnd], logRowMax + rowSt);
    for(indtype i = 1; i < gmodelSize; ++i)
      vecMax(logRowMax + rowSt, &gmodel[i].ptr[rowSt], rowEnd - rowSt);
    std::fill(rowSum + rowSt, rowSum + rowEnd, 0);
    for(indtype i = 0; i < gmodelSize; ++i)
    {
      for(indtype j = rowSt; j < rowEnd; ++j)
      {
        gmodel[i].ptr[j] = std::exp(gmodel[i].ptr[j] - logRowMax[j]);
        rowSum[j] += gmodel[i].ptr[j];
      }
    }
    // Rcout << " ================================== \n";
    // Rcout << *std::min_element(logRowMax + rowSt, logRowMax + rowEnd) << ", ";
    // Rcout << *std::max_element(logRowMax + rowSt, logRowMax + rowEnd) << ", ";
    // Rcout << *std::min_element(rowSum + rowSt, rowSum + rowEnd) << ", ";
    // Rcout << *std::max_element(rowSum + rowSt, rowSum + rowEnd) << ", ";
    // Rcout << "\n================================== \n";
  }


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 64)) break;
      compden(objI, std::min<std::size_t> (dT->NofAtom, objI + 64));
    }
  }


  cmptDensityGivenLogDenistyAndRowSum(
    G<indtype, valtype> *gmodel, indtype Ngau,
    valtype *rowSum, valtype *logRowMax,
    indtype Ndata, indtype maxCore):
    gmodelSize(Ngau), gmodel(gmodel), rowSum(rowSum), logRowMax(logRowMax)
  {
    dynamicTasking dt(maxCore, Ndata); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};




template<typename valtype>
inline valtype relaErr(valtype &current, valtype &previous)
{
  if(previous == 0)
  {
    if(current == 0) return 0;
    return std::numeric_limits<valtype>::max();
  }
  return std::abs(current / previous - 1);
}


template<typename indtype, typename valtype>
inline valtype relaErrSum(valtype *current, valtype *previous, indtype d)
{
  valtype S = 0;
  for(valtype *end = current + d; current < end; ++current, ++previous)
  {
    valtype tmp = relaErr<valtype> (*current, *previous);
    if(tmp == std::numeric_limits<valtype>::max())
      return std::numeric_limits<valtype>::max();
    S += tmp;
  }
  return S;
}




template<typename indtype, typename valtype>
inline bool meetEigenRatio(
    valtype *cholBegin, indtype d, valtype ratio,
    arma::Mat<valtype> &sigma,
    arma::Col<valtype> &e,
    valtype **tmpCntr)
  // sigma is a temporary container of size d x d
  // e is a temporary container of size d.
{
  triCholToFullSigma(cholBegin, &sigma[0], d, tmpCntr);
  arma::eig_sym(e, sigma);
  if((e[0] <= 0) or (ratio > 0 and e[d - 1] / e[0] > ratio)) return 0;
  return 1;
}




template<typename indtype, typename valtype>
struct checkEigenRatios: public Worker
{
  indtype d;
  valtype ratioThreshold;
  G<indtype, valtype> *gv;
  bool *trueOrFalse;
  arma::Mat<valtype> *fullSigmaCntr;
  arma::Col<valtype> *eCntr;
  vec<valtype*> *ptrCntr;
  dynamicTasking *dT;
  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      {
        trueOrFalse[objI] = meetEigenRatio(
          &gv[objI].cholU[0], d, ratioThreshold,
          fullSigmaCntr[st], eCntr[st], &ptrCntr[st][0]);
      }
    }
  }
  checkEigenRatios(
    indtype d, valtype ratioThreshold, G<indtype, valtype> *gv, indtype gvSize,
    bool *trueOrFalse, int maxCore):
    d(d), ratioThreshold(ratioThreshold), gv(gv), trueOrFalse(trueOrFalse)
  {
    dynamicTasking dt(maxCore, gvSize); dT = &dt;
    vec<arma::Mat<valtype> > fullSigmaContainer(maxCore, arma::Mat<valtype> (d, d));
    fullSigmaCntr = &fullSigmaContainer[0];
    vec<arma::Col<valtype> > eContainers(maxCore, arma::Col<valtype> (d));
    eCntr = &eContainers[0];
    vec<vec<valtype*> > ptrCntr_(maxCore, vec<valtype*> (d));
    ptrCntr = &ptrCntr_[0];
    parallelFor(0, maxCore, *this);
  }
};




template<typename indtype, typename valtype>
inline void cleanGaussianKernelNotMeetingEigenRatio(
    vec<G<indtype, valtype> > &gv, indtype d, valtype ratio, int maxCore)
{
  if(ratio <= 0) return;
  vec<unsigned char> ind(gv.size());
  checkEigenRatios<indtype, valtype> (
      d, ratio, &gv[0], gv.size(), (bool*)&ind[0], maxCore);
  eraseComponent<indtype, valtype> (gv, (bool*)&ind[0]);
}




template<typename indtype, typename valtype>
inline void normalizeAlpha(vec<G<indtype, valtype> > &gv)
{
  valtype alphaS = 0;
  indtype gvsize = gv.size();
  for(indtype i = 0; i < gvsize; ++i) alphaS += gv[i].alpha;
  alphaS = 1.0 / alphaS;
  for(indtype i = 0; i < gvsize; ++i) gv[i].alpha *= alphaS;
}




// ============================================================================
// This section implements the dense-version of Kmeans++.
// It is used in function findSpreadedMean() in gmmClassic.cpp.
// ============================================================================
template<typename indtype, typename valtype>
struct nextSelection: public Worker
{
  indtype d;
  valtype *chosen, *x, *minD;
  valtype **max;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 16)) break;
      {
        for(indtype i = objI, iend = std::min<indtype> (i + 16, dT->NofAtom);
            i < iend; ++i)
        {
          double dist = 0, *xi = x + std::size_t(i) * d;
          for(int k = 0; k < d; ++k)
          {
            double tmp = chosen[k] - xi[k];
            dist += tmp * tmp;
          }
          if(dist < minD[i]) minD[i] = dist;
          if(*max[st] < minD[i])
          {
            max[st] = minD + i;
          }
        }
      }
    }
  }


  nextSelection(indtype &newselection, indtype selected, indtype d, valtype *x,
                indtype N, valtype *minD, indtype maxCore):
    d(d), x(x), minD(minD)
  {
    chosen = x + selected * d;
    dynamicTasking dt(maxCore, N); dT = &dt;
    vec<valtype*> maxPtr(maxCore);
    valtype dummy = std::numeric_limits<valtype>::lowest();
    for(indtype i = 0; i < maxCore; ++i) maxPtr[i] = &dummy;
    max = &maxPtr[0];
    parallelFor(0, maxCore, *this);
    valtype *final = maxPtr[0];
    for(indtype i = 1; i < maxCore; ++i)
    {
      if(*final < *maxPtr[i]) final = maxPtr[i];
    }
    newselection = final - minD;
  }
};




inline NumericMatrix findSpreadedMean(NumericMatrix X, int K, int maxCore = 7)
{
  int d = X.nrow();
  int N = X.ncol();
  double *x = &X[0];
  NumericMatrix rst(d, K);
  double currentMax = -1;
  int currentSelection = 0;
  // Select the observation of the largest magnitude as the initial point.
  for(int i = 0; i < N; ++i)
  {
    double tmp = std::inner_product(x + i * d, x + i * d + d, x + i * d, 0.0);
    if(tmp > currentMax)
    {
      currentMax = tmp;
      currentSelection = i;
    }
  }
  std::copy(x + currentSelection * d, x + currentSelection * d + d, &rst[0]);
  vec<double> minD(N, std::numeric_limits<double>::max());
  for(int i = 1; i < K; ++i)
  {
    int newselection;
    nextSelection<int, double> (
        newselection, currentSelection, d, x, N, &minD[0], maxCore);
    currentSelection = newselection;
    std::copy(x + currentSelection * d, x + currentSelection * d + d, &rst[0] + i * d);
  }
  return rst;
}




inline NumericMatrix makeCovariances01(NumericMatrix X, int K)
{
  int d = X.nrow(), N = X.ncol();
  double *x = &X[0];
  vec<double> min(d, std::numeric_limits<double>::max());
  vec<double> max(d, std::numeric_limits<double>::lowest());
  for(int i = 0; i < N; ++i)
  {
    double *y = x + i * d;
    for(int j = 0; j < d; ++j)
    {
      if(y[j] > max[j]) max[j] = y[j];
      if(y[j] < min[j]) min[j] = y[j];
    }
  }
  double *var = &min[0];
  for(int i = 0; i < d; ++i)
  {
    var[i] = max[i] - min[i];
    var[i] *= var[i];
  }
  NumericMatrix rst(d * d, K);
  for(int i = 0; i < K; ++i)
  {
    double *y = &rst[0] + i * d * d;
    for(int j = 0; j < d; ++j)
      y[j * (d + 1)] = var[j];
  }
  return rst;
}




inline NumericMatrix makeCovariances02(NumericMatrix X, int K)
{
  int d = X.nrow(), N = X.ncol();
  double *x = &X[0];
  vec<double> S(d, 0.0), SS(d, 0.0);
  for(int i = 0; i < N; ++i)
  {
    double *col = x + i * d;
    for(int j = 0; j < d; ++j)
    {
      S[j] += col[j];
      SS[j] += col[j] * col[j];
    }
  }
  double *vars = &S[0];
  double scaler = std::pow(K, -1.0 / d);
  for(int i = 0; i < d; ++i)
  {
    vars[i] = (SS[i] / N - std::pow(S[i] / N, 2.0)) * scaler;
  }
  NumericMatrix rst(d * d, K);
  for(int i = 0; i < K; ++i)
  {
    double *y = &rst[0] + i * d * d;
    for(int j = 0; j < d; ++j)
      y[j * (d + 1)] = vars[j];
  }
  return rst;
}
















































