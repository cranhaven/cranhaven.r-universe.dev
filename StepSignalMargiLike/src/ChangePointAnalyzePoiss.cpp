
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix ChangePointAnalyzePoiss(NumericVector Data, IntegerVector InputLength,
    IntegerVector MaxJump, NumericVector Prior) {

 int TSize = InputLength(0);
 int MaxJ= MaxJump(0);

 NumericVector SufStat(TSize);

 int ii;
 int jj;
 int kk;

 for(ii=0;ii<TSize;ii++){
    SufStat(ii)=0;
 }

 double Temp;

 double P_alpha=Prior(0);
 double P_beta=Prior(1);

 NumericMatrix RetMl(MaxJ, TSize);
 NumericMatrix RetCha(MaxJ, TSize);

 for (ii=0; ii<TSize; ii++){
    NumericVector NewEst(ii+1);

    for (kk=0; kk<=ii;kk++){
      SufStat(kk)+= Data(ii);
      NewEst(kk)=0;
      NewEst(kk)=NewEst(kk)+lgamma(SufStat(kk)+P_alpha)-(SufStat(kk)+P_alpha)*std::log(ii-kk+1+P_beta)+P_alpha*std::log(P_beta)-lgamma(P_alpha);
    }

    RetMl(0,ii)=NewEst(0);
    RetCha(0,ii)=0;

    for (jj=1; (jj<MaxJ) & (jj<=ii);jj++){
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
