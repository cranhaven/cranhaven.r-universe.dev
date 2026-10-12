#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector algoritmo(NumericMatrix Gn, NumericMatrix I, NumericMatrix J, int Niter = 100, double error = 1e-06) {


  int nrows = Gn.nrow();
  int ncolumns = Gn.ncol();
  NumericMatrix nCn(nrows, ncolumns);
  int i = 1;

  for(int j = 0; j < ncolumns; ++j) {
    nCn(i, j) = sum(I(j,_)/ Gn(i - 1, _ ));
  }

  nCn(i, _ ) = nCn(i, _) * sum(1 / nCn(i, _));


  for(int k = 0; k < ncolumns; ++k) {
    Gn(i, k) = sum( J(k,_) / nCn(i,_));
  }

  if(max(abs(Gn(i,_) - Gn(i - 1,_))) < error){

    return(Gn(i,_));
  }

  int w = 1;

  while(max(abs(Gn(i,_) - Gn(i - 1,_))) > error) {

    for(int j = 0; j < ncolumns; ++j) {
      nCn(i, j) = sum(I(j,_)/ Gn(i - 1, _ ));
    }

    nCn(i, _ ) = nCn(i, _) * sum(1 / nCn(i, _));

    for(int k = 0; k < ncolumns; ++k) {
      Gn(i, k) = sum( J(k,_) / nCn(i,_));
    }

    Gn(0,_) = Gn(1,_);


    for(int j = 0; j < ncolumns; ++j) {
      nCn(i, j) = sum(I(j,_)/ Gn(i - 1, _ ));
    }

    nCn(i, _ ) = nCn(i, _) * sum(1 / nCn(i, _));

    for(int k = 0; k < ncolumns; ++k) {
      Gn(i, k) = sum( J(k,_) / nCn(i,_));
    }

    int t = w + 1;

    if (t > Niter){
      return(Gn(i,_));
    }


  }

  return(Gn(i,_));
}


