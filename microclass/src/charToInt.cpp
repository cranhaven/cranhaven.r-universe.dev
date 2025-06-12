#include <Rcpp.h>
#include "pow4inthead.h"
using namespace Rcpp;

// [[Rcpp::export]]
List charToInt(CharacterVector Seq) {
  std::vector<std::string> seq = as<std::vector<std::string> >(Seq);
  std::vector<std::vector<int> > seq_int;
  std::vector<int> row;
  int acgt;


  // Convert strings to integers
  for(unsigned i=0; i<seq.size(); ++i){
    std::vector<int> row;
    for(std::string::iterator it = seq[i].begin(), end = seq[i].end(); it != end; ++it) {
      acgt = *it;
      switch (acgt) {
      case 65:
        row.push_back(0); // A
        break;
      case 67:
        row.push_back(1); // C
        break;
      case 71:
        row.push_back(2); // G
        break;
      case 84:
        row.push_back(3); // T
        break;
      case 85:
        row.push_back(3); // U
        break;
      default:
        row.push_back(-1073741824); // -4^15
        break;
      }
    }
    seq_int.push_back(row);
  }
  return wrap(seq_int);
}
