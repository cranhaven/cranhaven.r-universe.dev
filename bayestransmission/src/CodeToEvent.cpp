#include "infect/infect.h"

#include <Rcpp.h>
using namespace Rcpp;

#include <string>
#include <cctype>

//' Convert coded events to string events
//'
//' @param x A vector of integers
//'
//' @return A vector of strings
//' @export
//' @examples
//' CodeToEvent(c(-1:19, 21:23, 31:33, -999))
// [[Rcpp::export]]
CharacterVector CodeToEvent(IntegerVector x) {
  CharacterVector out(x.size());
  
  for(auto i = x.begin(); i != x.end(); ++i) {
    out[i - x.begin()] = infect::EventCoding::eventString(*i);
  }
  
  return(out);
}

std::string tolower(const std::string& s) {
  string out(s);
  for(auto c = out.begin(); c != out.end(); ++c) {
    *c = std::tolower(*c);
  }
  return s;
}

//' Convert string events to coded events
//' @param x A vector of strings
//'
//' @return A vector of integers
//'
//' @export
//' @examples
//' EventToCode(c("admission", "discharge", "postest", "negtest"))
//' EventToCode(c("abxon", "abxoff", "isolon", "isoloff"))
// [[Rcpp::export]]
std::vector<int> EventToCode(const std::vector<std::string> x) {
    std::vector<int> out(x.size());

  for(std::size_t i = 0; i < x.size(); ++i) {
      string y = x[i];
    out[i] = infect::EventCoding::toEventCode(tolower(y));
  }

  return(out);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
CodeToEvent(c(-1:19, 21:23, 31:33, -999))
EventToCode(c('admission','discharge','postest','negtest'))
*/
