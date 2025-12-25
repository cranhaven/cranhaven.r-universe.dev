
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix ChangePointAnalyzeNorm(NumericVector Data, IntegerVector InputLength,
    IntegerVector MaxJump, NumericVector Prior) {

 int TSize = InputLength(0);
 int MaxJ= MaxJump(0);

 NumericVector SufStatSum(TSize);
 NumericVector SufStatSumofSq(TSize);

 int ii;
 int jj;
 int kk;

 for(ii=0;ii<TSize;ii++){

    SufStatSum(ii)=0;
    SufStatSumofSq(ii)=0;

 }

 double Temp;

 double P_mu=Prior(0);
 double P_kappa=Prior(1);
 double P_nu=Prior(2);
 double P_sigma=Prior(3);

 NumericMatrix RetMl(MaxJ, TSize);
 NumericMatrix RetCha(MaxJ, TSize);

 for (ii=0; ii<TSize; ii++){
    NumericVector NewEst(ii+1);

    for (kk=0; kk<=ii;kk++){
      SufStatSum(kk)+= Data(ii);
      SufStatSumofSq(kk)+= Data(ii)*Data(ii);
      NewEst(kk)=0;
      NewEst(kk)=NewEst(kk)+std::log(P_kappa/(ii-kk+1+P_kappa))/2+P_nu/2*(std::log(P_sigma)+std::log(P_nu))-(ii-kk+1+P_nu)/2*std::log(SufStatSumofSq(kk)-(SufStatSum(kk)*SufStatSum(kk))/(ii-kk+1)+P_sigma*P_nu+(ii-kk+1)*P_kappa*std::pow(SufStatSum(kk)/(ii-kk+1)-P_mu,2)/(ii-kk+1+P_kappa))+lgamma((ii-kk+1+P_nu)/2)-lgamma(P_nu/2);
    }

    RetMl(0,ii)=NewEst(0);
    RetCha(0,ii)=0;

    for (jj=1; (jj<MaxJ) & (jj<=ii) ;jj++){
        RetMl(jj,ii)=NewEst(ii)+RetMl(jj-1,ii-1);
        RetCha(jj,ii)=ii;

        for (kk=ii-1;kk>=jj;kk--){

          Temp=NewEst(kk)+RetMl(jj-1,kk-1);

          if (Temp>RetMl(jj,ii)){
              RetMl(jj,ii)=Temp;
              RetCha(jj,ii)=kk;
          }

        }

    }
 }

 NumericMatrix ans(2*MaxJ, TSize);
 for (ii=0; ii<TSize; ii++){
    for (jj=0; jj<MaxJ; jj++){
        ans(jj,ii)=RetMl(jj,ii);
        ans(jj+MaxJ, ii)=RetCha(jj,ii);
    }
 }

 return ans;
}
