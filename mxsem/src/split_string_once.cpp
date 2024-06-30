#include <Rcpp.h>
#include "string_operations.h"

equation_elements split_string_once(const std::string& str, const std::string& at) {
  // adapted from Vincenzo Pii at https://stackoverflow.com/questions/14265581/parse-split-a-string-in-c-using-string-delimiter-standard-c

  equation_elements eq_elem;

  auto start = str.find(at);
  if(start == std::string::npos)
    Rcpp::stop("Could not find " + at + " in " + str);

  eq_elem.lhs = str.substr(0, start);
  eq_elem.separator = at;
  eq_elem.rhs = str.substr(start + at.length(), str.length());

  return(eq_elem);
}

