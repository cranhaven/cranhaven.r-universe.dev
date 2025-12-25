// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <RcppArmadillo.h>
# include <RcppParallel.h>
# include "h/dnyTasking.hpp"
# include "h/macros.hpp"
# include "h/G.hpp"
# include "h/funs.hpp"
# include "h/gmmCW.hpp"
using namespace RcppParallel;
using namespace Rcpp;




/*
// Compute column sum and update
template<typename indtype, typename valtype>
struct comptColSum: public Worker
{
  indtype gsize;
  indtype d;
  indtype Xsize;
  valtype N_2;
  G<indtype, valtype> *gvec;
  valtype *rowSum;
  valtype *colSum;
  valtype *pointW;
  dynamicTasking *dT;


  inline void cmptColSumOneG(indtype i)
  {
    valtype &S = colSum[i];
    S = 0;
    valtype *ptr = &gvec[i].ptr[0];
    for(indtype k = 0; k < Xsize; ++k)
    {
      valtype tmp = 0;
      // It is possible that ptr[k] and rowSum[k] are both 0.
      if(ptr[k] > 0) tmp = ptr[k] / rowSum[k];
      S += tmp * pointW[k];
    }
    S = std::max(S - N_2, 0.0);
  }


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      cmptColSumOneG(objI);
    }
  }


  comptColSum(indtype gsize, indtype d, indtype Xsize,
              G<indtype, valtype> *gvec, valtype *rowSum,
              valtype *colSum, valtype *pointW, indtype Ncore):
    gsize(gsize), d(d), Xsize(Xsize), gvec(gvec), rowSum(rowSum),
    colSum(colSum), pointW(pointW)
  {
    N_2 = (d + std::size_t(d) * (d + 1) / 2) / 2.0;
    dynamicTasking dt(Ncore, gsize);
    dT = &dt;
    parallelFor(0, Ncore, *this);
  }
};
*/




/*
template<typename indtype, typename valtype>
inline void cmptColSumOneG(indtype whichG, G<indtype, valtype> *gvec,
                           indtype d, indtype Xsize, valtype *rowSum,
                           valtype *colSum, valtype *pointW)
{
  valtype N_2 = (d + std::size_t(d) * (d + 1) / 2) / 2.0;
  valtype &S = colSum[whichG];
  S = 0;
  valtype *ptr = &gvec[whichG].ptr[0];
  for(indtype k = 0; k < Xsize; ++k)
  {
    valtype tmp = ptr[k] / rowSum[k];
    if(!std::isfinite(tmp)) tmp = 0;
    if(pointW == nullptr) S += tmp;
    else S += tmp * pointW[k];
  }
  S = std::max(S - N_2, 0.0);
}
*/




template<typename indtype, typename valtype>
struct updateDensityMatAndRowSumDueToAlphaChange: public Worker
{
  // indtype gsize;
  indtype Xsize;
  indtype J;
  valtype nonJmultiplier;
  valtype Jmultiplier;
  valtype **auxC;
  G<indtype, valtype> *gmodel;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      valtype r = nonJmultiplier;
      if(objI - J == 0) r = Jmultiplier;
      for(indtype i = 0; i < Xsize; ++i)
      {
        gmodel[objI].ptr[i] *= r;
        auxC[st][i] += gmodel[objI].ptr[i];
      }
    }
  }


  updateDensityMatAndRowSumDueToAlphaChange(
    indtype Xsize, indtype J,
    valtype nonJmultiplier, valtype Jmultiplier,
    indtype gmodelSize, G<indtype, valtype> *gmodel,
    valtype *rowSum, vec<valtype> &auxCntr_, indtype NofCPU):
    Xsize(Xsize), J(J),
    nonJmultiplier(nonJmultiplier), Jmultiplier(Jmultiplier),
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




// The log-likelihood
template<typename indtype, typename valtype>
struct cmptLogLoss: public Worker
{
  valtype *rowSum;
  valtype *pointW;
  valtype *S;
  dynamicTasking *dT;


  void operator()(std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 256)) break;
      {
        for(indtype i = objI, iend = std::min<indtype> (i + 256, dT->NofAtom);
            i < iend; ++i)
        {
          valtype tmp = std::log(std::max<valtype> (
            rowSum[i], std::numeric_limits<valtype>::min()));
          S[st] += tmp * pointW[i];
        }
      }
    }
  }


  cmptLogLoss(valtype &loss, valtype *rowSum, valtype *pointW,
              indtype Xsize, unsigned Ncore): rowSum(rowSum), pointW(pointW)
  {
    vec<valtype> sum(Ncore, 0);
    S = &sum[0];
    dynamicTasking dt(Ncore, Xsize); dT = &dt;
    parallelFor(0, Ncore, *this);
    loss = std::accumulate(sum.begin(), sum.end(), 0.0);
  }
};




template<typename indtype, typename valtype>
struct clusterLabeling: public Worker
{
  indtype gsize, Xsize, d;
  valtype *X;
  indtype *Xbelong;
  G<indtype, valtype> *best;
  vec<valtype> *M;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 128)) break;
      {
        for(indtype i = objI, iend = std::min<indtype> (i + 128, dT->NofAtom);
            i < iend; ++i)
        {
          valtype maxDen = 0;
          indtype whichMax = 0;
          for(indtype k = 0; k < gsize; ++k)
          {
            valtype tmpDen = best[k].densityEval(X + i * std::size_t(d), d, &M[st][0], 1);
            if(tmpDen <= maxDen) continue;
            maxDen = tmpDen;
            whichMax = k;
          }
          Xbelong[i] = whichMax;
        }
      }
    }
  }


  clusterLabeling(indtype gsize, indtype Xsize, indtype d, valtype *X,
                  indtype *Xbelong, G<indtype, valtype> *best,
                  indtype maxCore):
    gsize(gsize), Xsize(Xsize), d(d), X(X), Xbelong(Xbelong), best(best)
  {
    vec<vec<valtype> > auxContainer(maxCore, vec<valtype> (d));
    M = &auxContainer[0];
    dynamicTasking dt(maxCore, Xsize); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};




/*
template<typename indtype, typename valtype>
valtype cmptLoss(indtype d, indtype Xsize, indtype gsize,
                 G<indtype, valtype> *gvec, valtype *rowSum,
                 valtype *pointW, unsigned Ncore)
{
  valtype N = d + std::size_t(d) * (d + 1) / 2;
  indtype &n = Xsize;
  valtype alphaS = 0;
  valtype loss = 0;


  for(indtype i = 0; i < gsize; ++i)
  {
    if(gvec[i].alpha > 0)
    {
      alphaS += gvec[i].alpha;
      loss += std::log(gvec[i].alpha);
    }
  }


  valtype tmp = std::log(n / 12.0);
  // valtype logAlphaS = std::log(std::max<valtype> (alphaS, invInf));
  valtype logAlphaS = std::log(alphaS);
  loss = N / 2.0 * (loss + (tmp - logAlphaS) * gsize);
  loss += gsize / 2.0 * (tmp + N + 1) + n * logAlphaS;


  valtype S = 0;
  cmptLogLoss<indtype, valtype> (S, rowSum, pointW, Xsize, Ncore);


  return loss - S;
}
*/




/*
template<typename indtype, typename valtype>
struct rowSumSubtract: public Worker
{
  indtype Xsize;
  valtype *rowSum;
  valtype *density;
  G<indtype, valtype> *gv;
  indtype gvSize; // gv and gvSize are here in case numeric issue kicks in.
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, 256)) break;
      for(indtype i = objI,
          iend = std::min<indtype> (i + 256, dT->NofAtom);
          i < iend; ++i)
      {
        rowSum[i] -= density[i];
        if(rowSum[i] < 0) // When numeric issue kicks in, re-summation
        {
          rowSum[i] = 0;
          for(indtype k = 0; k < gvSize; ++k)
          {
            valtype *ptr = &gv[k].ptr[0];
            if(ptr == density) continue;
            rowSum[i] += ptr[i];
          }
        }
      }
    }
  }


  rowSumSubtract(indtype Xsize, valtype *rowSum, valtype *density,
                 G<indtype, valtype> *gv, indtype gvSize, indtype Ncore):
    Xsize(Xsize), rowSum(rowSum), density(density), gv(gv), gvSize(gvSize)
  {
    dynamicTasking dt(Ncore, Xsize); dT = &dt;
    parallelFor(0, Ncore, *this);
  }
};
*/




List paraGmmFJfullInitial(
    NumericMatrix dat, NumericVector alpha,
    NumericMatrix mu, NumericMatrix sigma,
    NumericVector xweight, int Nthreads = 7, int maxit = 1000L,
    int kmin = 2, double eps = 1e-5, double annihilationEPS = 1e-16,
    double tlimit = 3600, int verb = 10)
{
  kmin = std::max<int> (kmin, 2);
  if(kmin > alpha.size())
  {
    Rcout << "Initial components less than minimum. Abort.\n";
    return List::create();
  }
  int d = dat.nrow(), Xsize = dat.ncol();
  double *X = &dat[0];


  // gaussian kernel initialization
  vec<G<int, double> > Gvec(alpha.size());
  {
    for(int i = 0, iend = Gvec.size(); i < iend; ++i)
    {
      Gvec[i].alpha = alpha[i];
      double *tmpmu = &mu[0] + i * d;
      Gvec[i].mu.assign(tmpmu, tmpmu + d);
      // extract tri-sigma, perform cholesky decomposition
      {
        double *fullSigma = &sigma[0] + std::size_t(d) * d;
        vec<double> &triSigma = Gvec[i].cholU;
        triSigma.resize(std::size_t(d) * (d + 1) / 2);
        fullSigmaToTriSigma(fullSigma, &triSigma[0], d);
        Gvec[i].computeCholUandSqrtOfDet(triSigma);
      }
    }


    // annihilate if alpha < annihilationEPS
    annihilateGinVec(Gvec, annihilationEPS);


    // density container initialization
    for(int i = 0, iend = Gvec.size(); i < iend; ++i)
    {
      Gvec[i].ptr.resize(Xsize);
    }
  }


  // EM
  vec<G<int, double> > best;
  NumericVector rowSum(Xsize);
  NumericVector rowSumBest(Xsize);
  double minLoss = std::numeric_limits<double>::max();
  {
    double *xw = &xweight[0];


    cmptDensity<int, double> (d, Xsize, Gvec.size(), X, &Gvec[0], Nthreads);
    vec<double> auxCntr;
    cmptRowSum<int, double> (Xsize, Gvec.size(), &Gvec.front(),
                             &rowSum[0], auxCntr, Nthreads);


    // cmptLogDensity<int, double> (d, Xsize, Gvec.size(), &dat[0], &Gvec.front(), Nthreads);
    // cmptDensityGivenLogDenistyAndRowSum<int, double> (
    //     &Gvec.front(), Gvec.size(), &rowSum[0], Xsize, Nthreads);


    // double priorLoss = std::numeric_limits<double>::max() * 0.5;
    double loss = std::numeric_limits<double>::max() * 0.5;
    double priorLogLoss = std::numeric_limits<double>::lowest() * 0.5;
    double logLoss = std::numeric_limits<double>::lowest() * 0.5;
    double N = d + std::size_t(d) * (d + 1) / 2;


    shallowCopy(Gvec, best);


    int it = 1;
    double endTime = std::clock() + Nthreads * tlimit * CLOCKS_PER_SEC;
    bool allOver = false;
    vec<double> container(Xsize);
    // int globalCounter = 0;
    while(std::clock() <= endTime)
    {
      it = 0;
      while(it <= maxit and std::clock() <= endTime) // This loop updates the GMM component-wise.
      {
        ++it;


        int j = 0;
        if(verb != 0)
        {
          Rcout << "Component-wise update, round " << it <<
            ", " << Gvec.size() << " components:\n";
        }


        // Inside this loop, components could be annihilated.
        // The GMM is being updated component-wise, one round, in this loop
        if(verb != 0) Rcout << "Which component is being updated: ";
        int componentI = -1;
        while(j < int(Gvec.size())) // Gvec's size is dynamic
        {
          ++componentI;
          // ++globalCounter;


          // if(verb != 0 and j % std::max<unsigned> (Gvec.size() / 10, 1) == 0)
          if(verb != 0) Rcout << componentI << ", ";
          // vec<double> &CS = container;


          // Print weight matrix
          if(false)
          // if(globalCounter <= 110 and globalCounter >= 84)
          {
            Rcout << "\n==============================\n";
            for(int i = 0; i < Xsize; ++i)
            {
              for(int u = 0, uend = Gvec.size(); u < uend; ++u)
              {
                Rcout << Gvec[u].ptr[i] / rowSum[i] << ", ";
                // Rcout << Gvec[u].ptr[i] << ", ";
              }
              Rcout << "\n";
            }
            Rcout << "==============================\n";
          }


          // How it was done in the paper.
          //   comptColSum<int, double> (
          //       Gvec.size(), d, Xsize, &Gvec[0], &rowSum[0], &CS[0], xw, Nthreads);
          //   // Update alphas and thus the density matrix and its row sums.
          //   double GjOldAlpha = Gvec[j].alpha;
          //   Gvec[j].alpha = CS[j] / std::accumulate(CS.begin(), CS.begin() + Gvec.size(), 0.0);
          //   double alphaSum = 0;
          //   for(int u = 0, uend = Gvec.size(); u < uend; ++u)
          //     alphaSum += Gvec[u].alpha;
          //   double r = 1.0 / alphaSum; // current alphas sum up to 1 of course.
          //   for(int u = 0, uend = Gvec.size(); u < uend; ++u)
          //     Gvec[u].alpha *= r;


          // How it was done in FJ's source code.
          // if(true)
          // {
          double S = 0.0;
          for(int u = 0; u < Xsize; ++u)
            S += Gvec[j].ptr[u] / rowSum[u] * xw[u];
          // double GjOldAlpha = Gvec[j].alpha;
          Gvec[j].alpha = std::max(S - N / 2.0, 0.0) / Xsize;
          // Rcout << "Gvec[j].alpha = " << Gvec[j].alpha << ", ";
          double alphaSum = 0.0;
          for(int u = 0, uend = Gvec.size(); u < uend; ++u)
            alphaSum += Gvec[u].alpha;
          double r = 1.0 / alphaSum; // current alphas sum up to 1 of course.
          for(int u = 0, uend = Gvec.size(); u < uend; ++u)
            Gvec[u].alpha *= r;
          // }


          if(false)
          {
            Rcout << "All alphas: ";
            for(int u = 0, uend = Gvec.size(); u < uend; ++u)
              Rcout << Gvec[u].alpha << " ";
            Rcout << "\n\n";
          }


          // Print weight matrix
          if(false)
          {
            Rcout << "\n############################ print weight matrix\n";
            for(int i = 0; i < Xsize; ++i)
            {
              for(int u = 0, uend = Gvec.size(); u < uend; ++u)
              {
                Rcout << Gvec[u].ptr[i] / rowSum[i] << ", ";
              }
              Rcout << "\n";
            }
            Rcout << "############################\n";
          }


          // Column sums are now stored in CS.
          if(Gvec[j].alpha <= 0.0)
          {
            if(Gvec.size() <= 2)
            {
              allOver = true;
              break;
            }


            updateDensityMatAndRowSumDueToAlphaChange<int, double> (
                Xsize, j, r, 0.0, Gvec.size(),
                &Gvec[0], &rowSum[0], auxCntr, Nthreads);


            eraseComponent(Gvec, j);
            if(verb != 0) Rcout << "(" << componentI << " erased), ";
            continue;
          }


          // mtp --> multiplier. Virtually update the weight matrix.
          if(false)
          {
            Rcout << "Examine weight matrix.\n";
            Rcout.precision(6);
            Rcout << "\n####################################\n";
            for(int i = 0; i < Xsize; ++i)
            {
              for(int u = 0, uend = Gvec.size(); u < uend; ++u)
              {
                Rcout << Gvec[u].ptr[i] / rowSum[i] << ", ";
              }
              Rcout << "\n";
            }
            Rcout << "####################################\n";
          }


          // Rcout << "1.1, " << Gvec[j].alpha << ", ";
          // G<int, double> &gaussian = Gvec[j];
          double *w = &container[0]; // container now is used to store the weights for updating one Gaussian component.
          double fakeAlpha;
          // Function paraWeight()'s purpose here is purely to update weights of Gaussian components.
          paraWeight<int, double> (fakeAlpha, Xsize, &Gvec[j].ptr.front(), xw, &rowSum[0], w, Nthreads);
          // Rcout << "1.2, " << Gvec[j].alpha << ", ";
          // Now w contains the normalized point-weights for calculating the mean and covar for Gvec[j].
          paraWeightMean<int, double> (X, w, d, Xsize, &Gvec[j].mu.front(), Nthreads);
          paraWeightSigma<int, double> (d, Xsize, X, w, &Gvec[j].cholU[0], &Gvec[j].mu[0], Nthreads);
          Gvec[j].computeCholUandSqrtOfDet(Gvec[j].cholU);
          paraCalDenWithAlphaOneGaussian<int, double> (d, Xsize, X, Gvec[j], Nthreads);
          updateDensityMatAndRowSumDueToAlphaChange<int, double> (
              Xsize, j, r, 1.0, Gvec.size(),
              &Gvec[0], &rowSum[0], auxCntr, Nthreads);
          ++j;
        }


        if(allOver) break;
        if(verb != 0) Rcout << "\n";


        cmptLogLoss<int, double> (logLoss, &rowSum[0], xw, Xsize, Nthreads);
        if(std::abs(logLoss - priorLogLoss) < eps * std::abs(priorLogLoss))
        {
          if(verb != 0)
          {
            Rcout << "===============================================================================\n";
            Rcout << "Component-wise GMM converged;\nNegative log-likelihood = " << -logLoss <<
              ";\nNumber of components = " << Gvec.size() << ".\n";
            Rcout << "===============================================================================\n\n";
          }
          break;
        }
        if(verb != 0) Rcout << "Negative log-likelihood = " << -logLoss << "\n";
        // Rcout << "globalCounter = " << globalCounter << "\n";
        priorLogLoss = logLoss;
      }


      if(allOver) break;


      if(verb != 0)
      {
        Rcout << "===============================================================================\n";
        Rcout << "Current best model's loss = " << minLoss;
      }


      // Compute the final loss.
      if(true)
      {
        double logAlpha = 0;
        for(int i= 0, iend = Gvec.size(); i < iend; ++i)
          if(Gvec[i].alpha > 0) logAlpha += std::log(Gvec[i].alpha);
        loss = N / 2.0 * logAlpha + 0.5 * Gvec.size() * (N + 1) * log(Xsize + 0.0) - logLoss;
      }


      if(loss <= minLoss)
      {
        if(verb != 0)
        {
          Rcout << " > " << loss << " = the converged model's loss.\n";
          Rcout << "Set the converged model to current best.\n";
        }
        shallowCopy(Gvec, best);
        minLoss = loss;
        std::copy(rowSum.begin(), rowSum.end(), rowSumBest.begin());
      }
      else
      {
        if(verb != 0)
        {
          Rcout << " < " << loss << " = the converged model's loss.\n";
          Rcout << "Do not set the converged model to current best.\n";
        }
      }


      if(verb != 0)
      {
        Rcout << "Erase the least probable component in the current converged model.\n";
        Rcout << "Continue training.\n";
        Rcout << "===============================================================================\n\n";
      }


      if(Gvec.size() <= unsigned(kmin)) break;


      // Find which component has least alpha and annihilate it.
      if(true)
      {
        int whichMin = 0;
        double minAlpha = Gvec[0].alpha;
        for(int u = 1, uend = Gvec.size(); u < uend; ++u)
        {
          if(Gvec[u].alpha < minAlpha)
          {
            whichMin = u;
            minAlpha = Gvec[u].alpha;
          }
        }
        double r = 1.0 / (1.0 - minAlpha);
        for(int u = 0, uend = Gvec.size(); u < uend; ++u)
          Gvec[u].alpha *= r;
        updateDensityMatAndRowSumDueToAlphaChange<int, double> (
            Xsize, whichMin, r, 0, Gvec.size(), &Gvec[0], &rowSum[0], auxCntr, Nthreads);


        eraseComponent(Gvec, whichMin);
      }
    }
  }


  // out
  {
    IntegerVector clust(Xsize);
    clusterLabeling<int, double> (
        best.size(), Xsize, d, X, &clust[0], &best[0], Nthreads);


    normalizeAlpha(best);
    NumericVector w(best.size());
    NumericMatrix mu(d, best.size()), sigma(d * d, best.size());
    int imu = 0, isigma = 0;
    for(int i = 0, iend = best.size(); i < iend; ++i, imu += d, isigma += d * d)
    {
      w[i] = best[i].alpha;
      std::copy(best[i].mu.begin(), best[i].mu.end(), &mu[imu]);
      vec<double*> tmp(d);
      triCholToFullSigma(&best[i].cholU.front(), &sigma[isigma], d, &tmp[0]);
    }


    if(verb != 0)
    {
      Rcout << "###############################################################################\n";
      Rcout << "Best model has " << w.size() << " components. Loss = " << minLoss << ".\n";
      Rcout << "###############################################################################\n";
    }


    return List::create(Named("alpha") = w, Named("mu") = mu,
                        Named("sigma") = sigma, Named("fitted") = rowSumBest,
                        Named("loss") = minLoss,
                        Named("clusterMember") = clust);
  }
}




// [[Rcpp::export]]
List paraGmmFJ(
    NumericMatrix X,
    NumericVector Xw,
    int G,
    int Gmin,
    NumericVector alpha,
    NumericMatrix mu,
    NumericMatrix sigma,
    double eigenRatioLim,
    double convergenceEPS,
    double alphaEPS,
    int maxIter,
    double tlimit,
    bool verbose,
    int maxCore)
{
  if(alpha.size() == 0)
  {
    alpha = NumericVector(G, 1.0 / G);
  }
  if(mu.size() == 0)
  {
    mu = findSpreadedMean(X, G, maxCore);
  }
  if(sigma.size() == 0)
  {
    sigma = makeCovariances02(X, G);
  }


  // Validate initial covariance matrices and maxEigenRatio.
  if(true)
  {
    int d = mu.nrow();
    arma::mat tmp(d, d);
    arma::colvec tmpv(d);
    int dd = d * d;
    for(int i = 0, iend = mu.ncol(); i < iend; ++i)
    {
      std::copy(&sigma[0] + i * dd, &sigma[0] + i * dd + dd, &tmp[0]);
      arma::eig_sym(tmpv, tmp);
      if(tmpv[0] <= 0)
      {
        Rcout << "The " << i << "th covariance matrix is not positive-definite. Quit.";
        return List::create();
      }
      double tmpRatio = tmpv[d - 1] / tmpv[0];
      if(eigenRatioLim > 0 and tmpRatio > eigenRatioLim)
      {
        Rcout << "The " << i << "th covariance matrix's max:min eigen ratio exceeds threshold. Quit.";
        return List::create();
      }
    }
  }


  return paraGmmFJfullInitial(
    X, alpha, mu, sigma, Xw, maxCore, maxIter,
    Gmin, convergenceEPS, alphaEPS, tlimit, verbose);
}











































































