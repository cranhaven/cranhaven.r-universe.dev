// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
#include <algorithm>
#include <iterator>

using namespace Rcpp;

int which_maxMap(std::map<int, int> x){
  int memoire = 0;
  int model = 0;
  for(std::map<int, int>::iterator it= x.begin() ; it != x.end(); ++it){
    if(it->second > memoire){
      model = it->first;
      memoire = it->second;
    }
  }
  return model;
}

std::map<int, int> tableC(IntegerVector x){
  std::map<int, int> counts;
  int n = x.size();
  for (int i = 0; i < n; i++) {
    counts[x[i]]++;
  }
  return counts;
}

// [[Rcpp::export]]
NumericVector oobErrors(IntegerVector sequo, int ntrain, IntegerVector mod, int ntree, IntegerVector modindex, IntegerMatrix inbag, IntegerMatrix mimi){
  NumericVector res(sequo.size());
  for(int j=0; j < res.size() ; j++){
    IntegerVector mama = RcppArmadillo::sample(mod, ntrain, TRUE);
    IntegerVector tmp = (seq_len(sequo[j])-1);
    for(int i=0; i<ntrain; i++){
      IntegerVector rowInbag = inbag.row(i);
      IntegerVector rowMimi = mimi.row(i);
      IntegerVector out = head(rowInbag, tmp.size());
      LogicalVector temporaire = (out == 0);
      IntegerVector outbag = tmp[ temporaire ];
      if( outbag.size() > 0 ){
        std::map<int, int> table = tableC(rowMimi[outbag]);
        mama[i] = which_maxMap(table);
      }
    }
    res[j] = mean(mama != modindex);
  }
  return(res);
}
