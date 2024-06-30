#include <Rcpp.h>
#include "string_operations.h"

//' char_in_string
//'
//' checks if a character is contained in a string
//' @param c character
//' @param str string
//' @return bool: true if char is in string
bool char_in_string(const char c, const std::string& str) {
  for(auto&s: str){
    if(s  == c)
      return(true);
  }
  return(false);
}

