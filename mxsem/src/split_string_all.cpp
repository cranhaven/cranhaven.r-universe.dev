#include <Rcpp.h>
#include "string_operations.h"

//' split_string_all
//'
//' splits a string
//' @param str string to be splitted
//' @param at where to split the string at
//' @return vector of strings
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> split_string_all(const std::string& str, const char at){
  // adapted from Vincenzo Pii at https://stackoverflow.com/questions/14265581/parse-split-a-string-in-c-using-string-delimiter-standard-c

  std::string base_string = str;
  std::vector<std::string> splitted_str;
  int n_curly_open = 0; // indicates if the user specified a block of code
  // that should not be changed

  std::string current_element = "";

  for(char c: str){
    // check for curly braces:
    switch(c){
    case '{':
      n_curly_open++;
      break;
    case '}':
      n_curly_open--;
      if(n_curly_open < 0){
        Rcpp::stop("Error parsing the syntax: Found a closing curly brace } without an opening curly brance {. The last line was "  +
          str);
      }
      break;
    }

    if((n_curly_open != 0) | (c == '}')){
      current_element += c;
      continue;
    }

    if(c == at){
      // split string
      splitted_str.push_back(current_element);
      current_element = "";
    }else{
      current_element += c;
    }
  }

  if(current_element.length() != 0)
    splitted_str.push_back(current_element);

  return(splitted_str);
}

