
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
 NumericVector ChangePointAnalyzeNormUnRes(NumericVector Data, 
    IntegerVector InputLength, NumericVector Prior) {

 int TSize = InputLength(0);
 NumericVector SufStatSum(TSize);
 NumericVector SufStatSumofSq(TSize);
    
 int ii;
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

 NumericVector RetMl(TSize);
 NumericVector RetCha(TSize);

 for (ii=0; ii<TSize; ii++){
    NumericVector NewEst(ii+1);

    for (kk=0; kk<=ii;kk++){   
      SufStatSum(kk)+= Data(ii);
      SufStatSumofSq(kk)+= Data(ii)*Data(ii);
      NewEst(kk)=0;
      NewEst(kk)=NewEst(kk)+std::log(P_kappa/(ii-kk+1+P_kappa))/2+P_nu/2*(std::log(P_sigma)+std::log(P_nu))-(ii-kk+1+P_nu)/2*std::log(SufStatSumofSq(kk)-(SufStatSum(kk)*SufStatSum(kk))/(ii-kk+1)+P_sigma*P_nu+(ii-kk+1)*P_kappa*std::pow(SufStatSum(kk)/(ii-kk+1)-P_mu,2)/(ii-kk+1+P_kappa))+lgamma((ii-kk+1+P_nu)/2)-lgamma(P_nu/2);
    }

    RetMl(ii)=NewEst(0);
    RetCha(ii)=0;

    for (kk=1;kk<=ii;kk++){
      Temp=NewEst(kk)+RetMl(kk-1);  
      if (Temp>RetMl(ii)){
          RetMl(ii)=Temp;
          RetCha(ii)=kk;
      }
    }

 }

 NumericMatrix ans(2, TSize);
 for (ii=0; ii<TSize; ii++){
    ans(0,ii)=RetMl(ii);
    ans(1,ii)=RetCha(ii);   
 }
 return ans;
 }
