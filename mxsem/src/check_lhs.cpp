#include <Rcpp.h>
#include "string_operations.h"

void check_lhs(const std::string& lhs, const std::string not_allowed){

  int n_curly_open = 0; // indicates if the user specified a block of code
  // that should not be changed

  for(char c: lhs){
    // check for curly braces:
    switch(c){
    case '{':
      n_curly_open++;
      break;
    case '}':
      n_curly_open--;
      if(n_curly_open < 0){
        Rcpp::stop("Error parsing the syntax: Found a closing curly brace } without an opening curly brance {. The last line was "  +
          lhs);
      }
      break;
    }

    if((n_curly_open != 0) | (c == '}'))
      continue;

    if(char_in_string(c, not_allowed))
      Rcpp::stop("The following is not allowed: " +
        lhs +
        ". It contains one of the following characters: " +
        not_allowed);
  }
}
