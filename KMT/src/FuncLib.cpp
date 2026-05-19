#include <RcppArmadillo.h>
using namespace Rcpp;


// [[Rcpp::depends(RcppArmadillo)]]

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

#include <omp.h>

#include "Distr.h"
#include "Optimal_Side.h"
#include "Optimal_Main.h"
#include "Common.h"



//[[Rcpp::export]]
double Unz(double z, arma::vec& Normed_X, Rcpp::String strDistr,
                     arma::vec& xVec, arma::mat& SMat, arma::mat& VMat, arma::vec& ReVec){


  double dGap= 1e-3;
  int LB=-5;
  int UB=5;

  arma::vec S1Vec = SMat.col(0);
  arma::vec S2Vec = SMat.col(1);
  arma::vec S3Vec = SMat.col(2);

  arma::vec v0Vec = VMat.col(0);
  arma::vec v1Vec = VMat.col(1);
  arma::vec v2Vec = VMat.col(2);


  int n = Normed_X.n_elem;
  arma::mat G(n,n);
  double out=0;

  if(strDistr == "Normal"){
    LB=-5;
    UB=5;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = ObjVal(z, Normed_X, G, distr);

  }else if(strDistr == "Cauchy"){
    LB=-1000;
    UB=1000;
    dGap = 0.005;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = ObjVal(z, Normed_X, G, distr);

  }else if(strDistr == "Logistic"){
    LB=-30;
    UB=13;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, ReVec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = ObjVal(z, Normed_X, G, distr);

  }else if(strDistr == "Gumbel"){
    LB=-10;
    UB=30;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, v0Vec, v1Vec, v2Vec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = ObjVal(z, Normed_X, G, distr);
  }


  return out;

}



//[[Rcpp::export]]
List GetUnzVec(arma::vec Normed_X, Rcpp::String strDistr,
                arma::vec xVec, arma::mat SMat, arma::mat VMat, arma::vec ReVec, int nNum=10, double sighat=0){

  int n = Normed_X.n_elem;

  int nGraph = (n+1)*nNum;
  arma::vec GraphVec(nGraph);
  arma::vec LineVec(nGraph);

  LineVec.zeros();
  LineVec = GetLineVec(Normed_X, nNum, sighat);

  GraphVec.zeros();


  double xi=0;
  double tmp = 0;

  for(int i=1;i<= (nGraph-nNum);i++){

    xi = LineVec[i-1];
    tmp = Unz(xi, Normed_X, strDistr,xVec, SMat, VMat, ReVec);
    GraphVec[i-1] = tmp;

  }

  for(int i=(nGraph-nNum+1);i<= nGraph;i++){

    if(i == (nGraph-nNum+1)){
      xi = LineVec[i-1];
      tmp = Unz(xi, Normed_X, strDistr,xVec, SMat, VMat, ReVec);
      GraphVec[i-1] = tmp;
    }else{
      GraphVec[i-1] = tmp;
    }

  }



  List lst(2);
  lst[0] = GraphVec;
  lst[1] = LineVec;

  return lst;

}






//[[Rcpp::export]]
arma::vec KMT_beta(Rcpp::String strDistr, arma::mat SMat, arma::mat VMat, arma::vec ReVec, arma::vec xVec, arma::vec Normed_X,
             bool bGreedy=false, bool bParallel=false, int nThreads=32){


  double dGap= 1e-3;
  int LB=-5;
  int UB=5;

  arma::vec S1Vec = SMat.col(0);
  arma::vec S2Vec = SMat.col(1);
  arma::vec S3Vec = SMat.col(2);

  arma::vec v0Vec = VMat.col(0);
  arma::vec v1Vec = VMat.col(1);
  arma::vec v2Vec = VMat.col(2);


  int n = Normed_X.n_elem;
  arma::mat G(n,n);
  arma::vec out(2);

  if(strDistr == "Normal"){
    LB=-5;
    UB=5;


    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = FindOptimal(Normed_X, G, distr, bParallel, nThreads);


  }else if(strDistr == "Cauchy"){
    LB=-1000;
    UB=1000;
    dGap = 0.005;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = FindOptimal(Normed_X, G, distr, bParallel, nThreads);


  }else if(strDistr == "Logistic"){
    LB=-30;
    UB=13;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, ReVec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = FindOptimal(Normed_X, G, distr, bParallel, nThreads);


  }else if(strDistr == "Gumbel"){
    LB=-10;
    UB=30;

    Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, v0Vec, v1Vec, v2Vec, xVec);
    G = distr.GetGiMat(Normed_X);
    out = FindOptimal(Normed_X, G, distr, bParallel, nThreads);

  }


  return out;

}



//[[Rcpp::export]]
List Init_Distr(Rcpp::String strDistr, arma::mat SMat, arma::mat VMat,
                arma::vec ReVec, arma::vec xVec, double x){


  double dGap= 1e-3;
  int LB=-5;
  int UB=5;

  arma::vec S1Vec = SMat.col(0);
  arma::vec S2Vec = SMat.col(1);
  arma::vec S3Vec = SMat.col(2);

  arma::vec v0Vec = VMat.col(0);
  arma::vec v1Vec = VMat.col(1);
  arma::vec v2Vec = VMat.col(2);

  if(strDistr == "Normal"){
    LB=-5;
    UB=5;


  }else if(strDistr == "Cauchy"){
    LB=-1000;
    UB=1000;
    dGap = 0.005;


  }else if(strDistr == "Logistic"){
    LB=-30;
    UB=13;

  }else if(strDistr == "Gumbel"){
    LB=-10;
    UB=30;


  }

  Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, v0Vec, v1Vec, v2Vec, ReVec, xVec);

  //Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, v0Vec, v1Vec, v2Vec, xVec);

  double fl = distr.fl(x);
  double Fl = distr.Fl(x);

  double re = 0;
  double Re = 0;

  if(strDistr=="Logistic"){
    re = distr.re(x);
    Re = distr.Re(x);
  }

  double phix = distr.phix(x);

  double v0 = distr.v0(x);
  double v1 = distr.v1(x);
  double v2 = distr.v2(x);

  double c0 = distr.c0(x);
  double c1 = distr.c1(x);
  double c2 = distr.c2(x);

  arma::mat Gamma = distr.Gamma(x);

  double s1 = distr.s1(x);
  double s2 = distr.s2(x);
  double s3 = distr.s3(x);

  double S1 = distr.S1(x);
  double S2 = distr.S2(x);
  double S3 = distr.S3(x);


  List lst(18);
  lst[0] = fl;
  lst[1] = Fl;
  lst[2] = re  ;
  lst[3] = Re  ;
  lst[4] = phix;
  lst[5] = v0;
  lst[6] = v1;
  lst[7] = v2 ;
  lst[8] = c0;
  lst[9] = c1;
  lst[10]= c2;
  lst[11]= Gamma  ;
  lst[12]= s1;
  lst[13] = s2 ;
  lst[14] = s3 ;
  lst[15] = S1 ;
  lst[16] = S2 ;
  lst[17] = S3 ;

  return lst;

}



//[[Rcpp::export]]
arma::mat Get_G_Mat(Rcpp::String strDistr, arma::vec Xhat,
               arma::mat SMat, arma::mat VMat,
                arma::vec ReVec, arma::vec xVec){


  double dGap= 1e-3;
  int LB=-5;
  int UB=5;

  arma::vec S1Vec = SMat.col(0);
  arma::vec S2Vec = SMat.col(1);
  arma::vec S3Vec = SMat.col(2);

  arma::vec v0Vec = VMat.col(0);
  arma::vec v1Vec = VMat.col(1);
  arma::vec v2Vec = VMat.col(2);

  if(strDistr == "Normal"){
    LB=-5;
    UB=5;


  }else if(strDistr == "Cauchy"){
    LB=-1000;
    UB=1000;
    dGap = 0.005;


  }else if(strDistr == "Logistic"){
    LB=-30;
    UB=13;

  }else if(strDistr == "Gumbel"){
    LB=-10;
    UB=30;


  }

  Distr distr( dGap, LB, UB, strDistr, S1Vec, S2Vec, S3Vec, v0Vec, v1Vec, v2Vec, ReVec, xVec);

  arma::mat out = distr.GetGiMat(Xhat);

  return out;

}





