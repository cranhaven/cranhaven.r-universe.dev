#ifndef __OPTIMAL_MAIN_H
#define __OPTIMAL_MAIN_H

arma::vec FindOptimal(arma::vec X, arma::mat G, Distr& distr, bool bParallel, int nThreads);

arma::vec FindOptimal(arma::vec X, arma::mat G, Distr& distr, bool bParallel, int nThreads){

  int n = X.size();

  arma::vec T1(2);
  T1.zeros();

  double OptimalFVal=0;
  double OptimalX = 0;


  if(bParallel){

    T1=Parallel_GetT1(X, G, nThreads);
    OptimalFVal=T1[1];
    OptimalX = T1[0];


  }else{

    T1 = GetT1(X, G);
    OptimalFVal=T1[1];
    OptimalX = T1[0];

  }


  //  Check when i=n
  double FVal = ObjVal(X[n-1], X, G, distr);

  if(FVal>OptimalFVal){
    OptimalX = X[n-1];
    OptimalFVal = FVal;

  }

  arma::vec out(2);

  out[0] = OptimalX;
  out[1] = OptimalFVal;


  return out;

}


#endif
