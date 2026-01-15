#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
DataFrame isSilent(NumericVector x, double threshold) {
  x.push_back(-1);
  int n = x.size(), startind, endind;
  std::vector<int> startinds, endinds;
  bool insegment = false;

  for(int i=0; i<n; i++) {
    if(!insegment) {
      if(x[i] <= threshold) {
        startind = i + 1;
        insegment = true;}
    } else {
      if(x[i] >= threshold) {
        endind = i;
        insegment = false;
        startinds.push_back(startind);
        endinds.push_back(endind);
      }
    }
  }

  return DataFrame::create(_["start"]= startinds, _["end"]= endinds);
}
