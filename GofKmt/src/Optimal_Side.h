#ifndef __OPTIMAL_SIDE_H
#define __OPTIMAL_SIDE_H


arma::vec GetT1(arma::vec X, arma::mat G);
arma::vec Parallel_GetT1(arma::vec X, arma::mat G, int nThr);
double ObjVal(double z, arma::vec X, arma::mat G, Distr& distr);


double ObjVal(double z, arma::vec X, arma::mat G, Distr& distr){

  int n = X.size();

  double out = 0;

  int nIndex = 0;

  for(int i=1;i<=n;i++){
    if(z < X[(i-1)]){
      nIndex = i-1;
      break;
    }
  }

  if(X[(n-1)] <= z){
    nIndex = n;
  }

  double Xi = 0;
  if(nIndex == 0){

    for(int i=1;i<=n;i++){
      Xi = X[(i-1)];
      out -= distr.Gi(z, Xi);
    }

  }else if(nIndex == n){

    for(int i=1;i<=n;i++){
      Xi = X[(i-1)];
      out -= G(i-1,i-1); //Gi(z, Xi)
    }
    out += n;
  }else{
    for(int i=(nIndex+1);i<=n;i++){
      Xi = X[(i-1)];
      out -= distr.Gi(z, Xi);
    }

    for(int i=1;i<=nIndex;i++){
      Xi = X[(i-1)];
      out += (1 - G(i-1,i-1));
    }
  }
  return (abs(out)/sqrt(n));
}






arma::vec GetT1(arma::vec X, arma::mat G){

  int n = X.size();


  double U1=0;
  double U2=0;
  double U=0;

  double dMax = 0;
  double Optimal = X[0];

  double Xi = 0;

  for(int i=1;i<=n;i++){

    Xi =  X[i-1];

    U1 = 0;

    for(int k=1;k<=n;k++){
      if(k<=i){
        U1 -= G(k-1,k-1);
      }else{
        U1 -= G(k-1,i-1);
      }
    }

    U1 += i;
    U2 = U1-1;

    if(abs(U1) >= abs(U2)){
      U = abs(U1);

    }else{
      U = abs(U2);

    }

    if(U>dMax){
      dMax = U;
      Optimal = Xi;
    }

    //Rcout <<"Optimal is"  <<Optimal << " dMax is " << dMax << "\n" << std::endl;


  }

  arma::vec out(2);
  out[0] = Optimal;
  out[1] = dMax/sqrt(n);

  return out;
}




arma::vec Parallel_GetT1(arma::vec X, arma::mat G, int nThr){

  int n = X.size();


  arma::vec Parallel_out(n);
  Parallel_out.zeros();

  //omp_set_num_threads(nThreads);

#pragma omp parallel
{
#pragma omp for
  for(int i=1;i<=n;i++){

    double U1 = 0;
    double U2 = 0;
    double U=0;



    for(int k=1;k<=n;k++){
      if(k<=i){
        U1 -= G(k-1,k-1);
      }else{
        U1 -= G(k-1,i-1);
      }
    }

    U1 += i;
    U2 = U1-1;

    if(abs(U1) >= abs(U2)){
      U = abs(U1);

    }else{
      U = abs(U2);

    }

    Parallel_out[i-1] = U;


  }

}

  arma::uword idx = Parallel_out.index_max();

  arma::vec out(2);
  out[0] = X[idx];
  out[1] = Parallel_out[idx]/sqrt(n);

  return out;

}




#endif
