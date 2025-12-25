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




// return the total relative error
template<typename indtype, typename valtype>
void update1Gparallel(G<indtype, valtype> &gaussian, valtype *X, valtype *pointW,
                      valtype *rowSum, valtype &totalDiff,
                      G<indtype, valtype> *gv, indtype gvSize, unsigned Ncore)
{
  indtype d = gaussian.mu.size();
  indtype Xsize = gaussian.ptr.size();
  vec<valtype> W(Xsize, 0);
  valtype alphaNew = 0;
  paraWeight<indtype, valtype> (alphaNew, Xsize, &gaussian.ptr[0], pointW, rowSum, &W[0], Ncore);
  totalDiff += relaErr<valtype> (alphaNew, gaussian.alpha);
  // Rcout << "\n\n\n\n\n\n";
  // Rcout << "totalDiff = " << totalDiff << "\n";
  // Rcout << "======================================\n";
  gaussian.alpha = alphaNew;


  vec<valtype> paraReserve(std::size_t(d) * (d + 1) / 2);
  std::copy(gaussian.mu.begin(), gaussian.mu.end(), paraReserve.begin());


  paraWeightMean<indtype, valtype> (X, &W.front(), d, Xsize, &gaussian.mu.front(), Ncore);
  totalDiff += relaErrSum(&gaussian.mu.front(), &paraReserve.front(), d);
  // Rcout << "\n======================================\n";
  // Rcout << "totalDiff = " << totalDiff << "\n";


  std::copy(gaussian.cholU.begin(), gaussian.cholU.end(), paraReserve.begin());
  paraWeightSigma<indtype, valtype> (
      d, Xsize, X, &W.front(), &gaussian.cholU.front(), &gaussian.mu.front(), Ncore);


  // Rcout << "\n+++++++++++++++++++++++++++++++++++++++\n";
  // for(int i = 0, iend = paraReserve.size(); i < iend; ++i)
  // {
  //   Rcout << paraReserve[i] << ", ";
  // }
  // Rcout << "\n";
  // for(int i = 0, iend = gaussian.cholU.size(); i < iend; ++i)
  // {
  //   Rcout << gaussian.cholU[i] << ", ";
  // }
  // Rcout << "\n+++++++++++++++++++++++++++++++++++++++\n";


  gaussian.computeCholUandSqrtOfDet(gaussian.cholU);
  totalDiff += relaErrSum(&gaussian.cholU.front(), &paraReserve[0], d);
  // Rcout << "\n======================================\n";
  // Rcout << "totalDiff = " << totalDiff << "\n";
  // Rcout << "\n\n\n\n\n\n";


  paraSingleGdensityUpdateRowSum<indtype, valtype> (
      d, Xsize, X, rowSum, gaussian, gv, gvSize, Ncore);
}




// dat[, i] is the i_th data point
// sigma[, i] is the i_th initial full covariance matrix for the i_th Gaussian component
List paraGmmComponentWiseFullInitial(
    NumericMatrix dat,
    NumericVector alpha,
    NumericMatrix mu,
    NumericMatrix sigma,
    NumericVector xweight,
    int Nthreads,
    int maxit,
    double maxEigenRatio,
    double eps,
    double annihilationEPS,
    double tlimit,
    int verbose
  )
{
  // structure initilization
  int d = dat.nrow(), Xsize = dat.ncol();


  // gaussian kernel initialization
  vec<G<int, double> > Gvec(alpha.size());
  {
    for(int i = 0, iend = Gvec.size(); i < iend; ++i)
    {
      Gvec[i].alpha = alpha[i];
      double *m = &mu[0] + std::size_t(i) * d;
      Gvec[i].mu.assign(m, m + d);
      if(true) // extract tri-sigma, perform cholesky decomposition
      {
        double *fullSigma = &sigma[0] + std::size_t(i) * d * d;
        vec<double> &triSigma = Gvec[i].cholU;
        triSigma.resize(std::size_t(d) * (d + 1) / 2);
        fullSigmaToTriSigma<int, double> (fullSigma, &triSigma[0], d);


        // Rcout << "\n=================================================\n";
        // for(int i = 0, iend = d * d; i < iend; ++i)
        //   Rcout << fullSigma[i] << ", ";
        // Rcout << "\n\n";


        // for(int i = 0, iend = triSigma.size(); i < iend; ++i)
        //   Rcout << triSigma[i] << ", ";
        // Rcout << "\n\n";


        Gvec[i].computeCholUandSqrtOfDet(triSigma);


        // for(int i = 0, iend = triSigma.size(); i < iend; ++i)
        //   Rcout << triSigma[i] << ", ";
        // Rcout << "\n=================================================\n";
      }
    }


    // annihilate if alpha < annihilationEPS
    annihilateGinVec<int, double> (Gvec, annihilationEPS);


    // density container initialization
    for(int i = 0, iend = Gvec.size(); i < iend; ++i)
    {
      Gvec[i].ptr.assign(Xsize, 0);
    }
  }


  // EM
  NumericVector rowSum(Xsize);
  {
    double *xw = &xweight[0];
    int gaussianParaN = std::size_t(d + 1) * d / 2 + d + 1;


    cmptDensity<int, double> (d, Xsize, Gvec.size(), &dat[0], &Gvec.front(), Nthreads);
    {
      vec<double> auxCntr;
      cmptRowSum<int, double> (Xsize, Gvec.size(), &Gvec.front(), &rowSum[0], auxCntr, Nthreads);
    }


    // cmptLogDensity<int, double> (d, Xsize, Gvec.size(), &dat[0], &Gvec.front(), Nthreads);
    // cmptDensityGivenLogDenistyAndRowSum<int, double> (
    //     &Gvec.front(), Gvec.size(), &rowSum[0], Xsize, Nthreads);


    double endTime = std::clock() + Nthreads * tlimit * CLOCKS_PER_SEC;
    for(int iter = 0, iterEnd = maxit; iter < iterEnd; ++iter)
    {
      int GvecSize = Gvec.size();
      int totalParameters = GvecSize * gaussianParaN;


      // componentWise
      double totalErr = 0;
      for(int g = 0; g < GvecSize; ++g)
      {
        update1Gparallel<int, double> (
            Gvec[g], &dat[0], xw, &rowSum[0], totalErr, &Gvec[0], Gvec.size(), Nthreads);
      }
      double avgRelativeErr = totalErr / totalParameters;


      cleanGaussianKernelNotMeetingEigenRatio<int, double> (
          Gvec, d, maxEigenRatio, Nthreads);
      // The function does nothing if maxEigenRatio <= 0.
      // Erase components that have outrageous eigen ratios.
      if(int(Gvec.size()) < GvecSize)
      {
        avgRelativeErr = eps * 2;
        if(verbose != 0)
          Rcout << "Eigenvalue ratios exceed or component weights fall below thresholds, " <<
            "Gaussian components annihilated.\n";
      }
      if(avgRelativeErr < eps or std::clock() > endTime) break;


      annihilateGinVec(Gvec, annihilationEPS);
      if(verbose != 0)
      {
        Rcout << "iteration = " << iter + 1 << ", mean absolute relative change = " <<
          avgRelativeErr << ", number of remaining kernels = " << Gvec.size() << "\n";
      }
    }
  }


  // out
  {
    NumericVector w(Gvec.size());
    NumericMatrix mu(d, Gvec.size()), sigma(d * d, Gvec.size());
    int imu = 0, isigma = 0;
    for(int i = 0, iend = Gvec.size(); i < iend; ++i, imu += d, isigma += d * d)
    {
      w[i] = Gvec[i].alpha;
      std::copy(Gvec[i].mu.begin(), Gvec[i].mu.end(), &mu[imu]);
      vec<double*> tmp(d);
      triCholToFullSigma(&Gvec[i].cholU.front(), &sigma[isigma], d, &tmp[0]);
    }


    IntegerVector clust(Xsize);
    for(int i = 0; i < Xsize; ++i)
    {
      int whichMax = 0;
      double denMax = Gvec[0].ptr[i];
      for(int j = 1, jend = Gvec.size(); j < jend; ++j)
      {
        if(denMax >= Gvec[j].ptr[i]) continue;
        whichMax = j;
        denMax = Gvec[j].ptr[i];
      }
      clust[i] = whichMax;
    }


    return List::create(Named("alpha") = w, Named("mu") = mu,
                        Named("sigma") = sigma, Named("fitted") = rowSum,
                        Named("clusterMember") = clust);
  }
}




// xweight does not need to sum up to 1
// [[Rcpp::export]]
List paraGmmCW(
    NumericMatrix X,
    NumericVector Xw,
    int G,
    NumericVector alpha,
    NumericMatrix mu,
    NumericMatrix sigma,
    double eigenRatioLim,
    double convergenceEPS,
    double alphaEPS,
    int maxIter,
    double tlimit,
    int verbose,
    int maxCore
  )
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
    sigma = makeCovariances01(X, G);
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


  return paraGmmComponentWiseFullInitial(
    X, alpha, mu, sigma, Xw, maxCore, maxIter, eigenRatioLim,
    convergenceEPS, alphaEPS, tlimit, verbose);
  // paraGmmComponentWiseFullInitial(
  //   NumericMatrix dat,
  //   NumericVector alpha,
  //   NumericMatrix mu,
  //   NumericMatrix sigma,
  //   NumericVector xweight,
  //   int Nthreads,
  //   int maxit,
  //   double maxEigenRatio,
  //   double eps,
  //   double annihilationEPS,
  //   double tlimit,
  //   int verbose
}














































