
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
 NumericVector ChangePointAnalyzePoissUnRes(NumericVector Data, 
    IntegerVector InputLength, NumericVector Prior) {

 int TSize = InputLength(0);
 NumericVector SufStat(TSize);
    
 int ii;
 int kk;

 for(ii=0;ii<TSize;ii++){
    SufStat(ii)=0;
 }

 double Temp;

 double P_alpha=Prior(0);
 double P_beta=Prior(1);

 NumericVector RetMl(TSize);
 NumericVector RetCha(TSize);

 for (ii=0; ii<TSize; ii++){
    NumericVector NewEst(ii+1);

    for (kk=0; kk<=ii;kk++){   
      SufStat(kk)+= Data(ii);
      NewEst(kk)=0;
      NewEst(kk)=NewEst(kk)+lgamma(SufStat(kk)+P_alpha)-(SufStat(kk)+P_alpha)*std::log(ii-kk+1+P_beta)+P_alpha*std::log(P_beta)-lgamma(P_alpha);
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
