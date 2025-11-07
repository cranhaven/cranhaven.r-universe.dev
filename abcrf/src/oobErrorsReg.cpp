#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector oobErrorsReg(IntegerVector sequo, int ntrain, int ntree, NumericVector resp, IntegerMatrix inbag, NumericMatrix pred){
  NumericVector res(sequo.size());
  for(int j=0; j < res.size() ; j++){
    NumericVector mama(ntrain, NA_REAL);
    IntegerVector tmp = (seq_len(sequo[j])-1);
    for(int i=0; i<ntrain; i++){
      IntegerVector rowInbag = inbag.row(i);
      NumericVector rowPred = pred.row(i);
      IntegerVector out = head(rowInbag, tmp.size());
      LogicalVector isOob = (out == 0);
      IntegerVector outbag = tmp[ isOob ];
      NumericVector predOob = rowPred[ outbag ];
      if( outbag.size() > 0 ){
        mama[i] = mean(predOob);
      }
    }
    res[j] = mean(pow(na_omit(mama - resp), 2));
  }
  return(res);
}

// [[Rcpp::export]]
NumericVector oobMedErrorsReg(IntegerVector sequo, int ntrain, int ntree, NumericVector resp, IntegerMatrix inbag, NumericMatrix pred){
  NumericVector res(sequo.size());
  for(int j=0; j < res.size() ; j++){
    NumericVector mama(ntrain, NA_REAL);
    IntegerVector tmp = (seq_len(sequo[j])-1);
    for(int i=0; i<ntrain; i++){
      IntegerVector rowInbag = inbag.row(i);
      NumericVector rowPred = pred.row(i);
      IntegerVector out = head(rowInbag, tmp.size());
      LogicalVector isOob = (out == 0);
      IntegerVector outbag = tmp[ isOob ];
      NumericVector predOob = rowPred[ outbag ];
      if( outbag.size() > 0 ){
        mama[i] = median(predOob);
      }
    }
    res[j] = mean(pow(na_omit(mama - resp), 2));
  }
  return(res);
}
