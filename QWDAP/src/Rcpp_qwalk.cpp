#include <Rcpp.h>
#include <vector>
#include "qwalkLib.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix qwalkRcpp(Rcpp::NumericMatrix edges, int startindex, int lens, Rcpp::NumericVector scals, int flag, int getfloat, int multiple){
  // translate vector to array
  int x = edges.nrow();
  int y = edges.ncol();
  if(x!=y){
    Rcpp::stop("The matrix 'edges' is not a square matrix.");
  }
  if(startindex<0 || startindex>=x){
    Rcpp::stop("The parameter 'startindex' is out of range.");
  }
  if(flag >= x){
    Rcpp::stop("The flag index is wrong.");
  }
  int r = initQwalk(x,startindex);
  if(!r){
    Rcpp::stop("Initialization error.");
  }
  // decomposition
  Matrix A = initMatrix(x,y);
  for(int i=0;i<x;i++){
    for(int j=0;j<y;j++){
      A[i][j] = edges(i,j);
    }
  }
  r = specRun(A, x);
  destroyMatrix(A,x);
  if(!r){
    Rcpp::stop("Decomposition error.");
  }
  // run and collect data
  Rcpp::NumericMatrix res(lens*scals.size(), x);
  int curLine = 0;
  for(int i=0;i<scals.size();i++){
    qwalk.curTime = 0;
    Matrix items = collectData(qwalk.N, scals[i], lens, flag, getfloat, multiple);
    for(int j=0;j<lens;j++, curLine++){
      for(int k=0; k<qwalk.N;k++){
        res(curLine, k) = items[j][k];
      }
    }
    destroyMatrix(items, lens);
  }
  releaseMemory(qwalk.N);
  
  return res;
}
