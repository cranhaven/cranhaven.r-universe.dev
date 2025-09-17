// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppThread)]]
#include <RcppThread.h>

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


using namespace arma;

double fraction(const mat coeffMatA, const mat coeffMatB) {

  // mat zeroesLocation =
  // mat res = (abs(coeffMatA - coeffMatB)/(coeffMatA+coeffMatB));
  return(accu((abs(coeffMatA.elem(find(coeffMatA != 0 || coeffMatB != 0))
                     - coeffMatB.elem(find(coeffMatA != 0 || coeffMatB != 0)))
                 /(coeffMatA.elem(find(coeffMatA != 0 || coeffMatB != 0))+
                 coeffMatB.elem(find(coeffMatA != 0 || coeffMatB != 0))))));
}

double logDiff(const mat coeffMatA, const mat coeffMatB) {
  return(accu(log(1+abs(coeffMatA - coeffMatB))));
}

double wLogDiff(const mat coeffMatA, const mat coeffMatB) {

  vec w(coeffMatA.n_rows+1,fill::ones);
  w = regspace(1,(coeffMatA.n_rows));
  w = shift(w, 1);
  w = pow(w,-2);

  mat logDiff(log(1+abs(coeffMatA - coeffMatB)));

  return(accu(logDiff.each_col()%w));
}

double logDiffComplex(const cx_rowvec coeffMatA, const cx_rowvec coeffMatB) {
  return(accu(log(1+abs(coeffMatA - coeffMatB))));
}

double fractionComplex(const cx_rowvec coeffMatA, const cx_rowvec coeffMatB) {
  return(accu((abs(coeffMatA.elem(find(coeffMatA != 0 || coeffMatB != 0))
                     - coeffMatB.elem(find(coeffMatA != 0 || coeffMatB != 0)))
                 /abs(coeffMatA.elem(find(coeffMatA != 0 || coeffMatB != 0))+
                 coeffMatB.elem(find(coeffMatA != 0 || coeffMatB != 0))))));
}


double binB(const mat coeffMatA, const mat coeffMatB) {
  return(accu((coeffMatA != 0) && (coeffMatB == 0)));
}

double binC(const mat coeffMatA, const mat coeffMatB) {
  return(accu((coeffMatA == 0) && (coeffMatB != 0)));
}

double tipLab(const cx_mat coeffMatA, const cx_mat coeffMatB) {
  return(accu(log(1+abs(coeffMatA - coeffMatB))));
}

// [[Rcpp::export]]
std::vector<double> coeffDist(Rcpp::List coeffsList, std::string method, int nThreads = -1){
  int coeffsLength = coeffsList.length();

  size_t numThreads = std::thread::hardware_concurrency();
  if(nThreads != -1){
    numThreads = nThreads;
  }

  if(method == "fraction"){
    std::vector<mat> Y = Rcpp::as<std::vector<mat>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = fraction(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else if(method == "logDiff"){
    std::vector<mat> Y = Rcpp::as<std::vector<mat>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = logDiff(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else if (method == "wLogDiff"){
    std::vector<mat> Y = Rcpp::as<std::vector<mat>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = wLogDiff(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else if (method == "pa"){
    std::vector<mat> Y = Rcpp::as<std::vector<mat>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = binB(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else if (method == "ap"){
    std::vector<mat> Y = Rcpp::as<std::vector<mat>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = binC(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else if (method == "logDiffComplex"){
    std::vector<cx_rowvec> Y = Rcpp::as<std::vector<cx_rowvec>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = logDiffComplex(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else if (method == "fractionComplex"){
    std::vector<cx_rowvec> Y = Rcpp::as<std::vector<cx_rowvec>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = fractionComplex(Y[0],Y[i]);
    },numThreads,0);

    return(distVect);

  } else {
    std::vector<cx_mat> Y = Rcpp::as<std::vector<cx_mat>>(coeffsList);
    std::vector<double> distVect(coeffsLength-1);

    RcppThread::parallelFor(1, coeffsLength, [&distVect,&Y] (unsigned int i) {
      distVect[i-1] = logDiffComplex(Y[0],Y[i]);
    },numThreads,0);


    return(distVect);

  }
}


// [[Rcpp::export]]
Rcpp::NumericMatrix coeffDistMat(Rcpp::List coeffsList, std::string method, int nThreads = -1){

  size_t numThreads = std::thread::hardware_concurrency();
  if(nThreads != -1){
    numThreads = nThreads;
  }

  if (method == "fraction"){
    std::vector<mat> coeffs = Rcpp::as<std::vector<mat>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = fraction(coeffs[i],coeffs[j]);
      }
    }, numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else if(method == "logDiff"){
    std::vector<mat> coeffs = Rcpp::as<std::vector<mat>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = logDiff(coeffs[i],coeffs[j]);
      }
    }, numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else if (method == "wLogDiff"){
    std::vector<mat> coeffs = Rcpp::as<std::vector<mat>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = wLogDiff(coeffs[i],coeffs[j]);
      }
    },numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else if (method == "pa"){
    std::vector<mat> coeffs = Rcpp::as<std::vector<mat>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = binB(coeffs[i],coeffs[j]);
      }
    },numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else if (method == "ap"){
    std::vector<mat> coeffs = Rcpp::as<std::vector<mat>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
       for(int j = i+1; j < numCoeffs; j++){
         distMat(i,j) = binC(coeffs[i],coeffs[j]);
       }
    },numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else if (method == "logDiffComplex"){
    std::vector<cx_rowvec> coeffs = Rcpp::as<std::vector<cx_rowvec>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = logDiffComplex(coeffs[i],coeffs[j]);
      }
    },numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else if (method == "fractionComplex"){
    std::vector<cx_rowvec> coeffs = Rcpp::as<std::vector<cx_rowvec>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = fractionComplex(coeffs[i],coeffs[j]);
      }
    },numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  } else {
    std::vector<cx_mat> coeffs = Rcpp::as<std::vector<cx_mat>>(coeffsList);
    int numCoeffs = coeffs.size();
    mat distMat(numCoeffs, numCoeffs, fill::zeros);

    RcppThread::parallelFor(0, numCoeffs, [&distMat,&numCoeffs,&coeffs] (unsigned int i) {
      for(int j = i+1; j < numCoeffs; j++){
        distMat(i,j) = tipLab(coeffs[i],coeffs[j]);
      }
    },numThreads,0);

    distMat = distMat.t() + distMat;

    return(Rcpp::wrap(distMat));

  }
}
