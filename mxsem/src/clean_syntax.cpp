#include <Rcpp.h>
#include "string_operations.h"
#include "clean_syntax.h"

//' clean_syntax
//'
//' takes in a lavaan style syntax and removes comments, white space, etc.
//' @param syntax lavaan style syntax
//' @return vector of strings with cleaned syntax
// [[Rcpp::export]]
std::vector<std::string> clean_syntax(const std::string& syntax) {
  std::vector<std::string> cleaned_syntax;
  std::string current_syntax {""};
  bool is_comment  = false;
  bool is_open     = false;
  int n_curly_open = 0; // indicates if the user specified a block of code
  // that should not be changed

  for(char c: syntax){

    // check for curly braces:
    switch(c){
    case '{':
      if(is_comment)
        break;
      n_curly_open++;
      break;
    case '}':
      if(is_comment)
        break;
      n_curly_open--;
      if(n_curly_open < 0){
        Rcpp::stop("Error parsing the syntax: Found a closing curly brace } without an opening curly brance {. The last line was "  +
          current_syntax);
      }
      break;
    }
    if(n_curly_open != 0){
      current_syntax += c;
      continue;
    }

    // if we are not in the curly-brace area:
    switch(c){
    case ' ':
      // removes  whitespace
      break;
    case '\t':
      // remove tabs
      break;
    case '#':
      // as long as the is_comment flag is set to true, anything else will
      // be skipped
      is_comment = true;
      break;
    case '\n':
      // reset comment
      is_comment = false;
      if(!is_open && (current_syntax.length() != 0)){
        // add current syntax if the string did end and is not empty
        cleaned_syntax.push_back(current_syntax);
        current_syntax = "";
        break;
      }
      break;
    case ';':
      // ; is an alternative to a new line
      if(is_comment)
        break;
      // the only difference to a new line is that commands cannot continue after
      // a semicolon.
      if(is_open)
        Rcpp::stop("Line ended with ; but it seems like the previous sign was an operator (e.g., =~;!). The last line was " +
          current_syntax);
      if(current_syntax.length() != 0){
        cleaned_syntax.push_back(current_syntax);
        current_syntax = "";
        break;
      }
      break;
    default:
        if(is_comment)
          break;
        if(char_in_string(c, "+*=~:")){
          // is_open allows for line breaks.
          is_open = true;
        }else{
          is_open = false;
        }
        current_syntax += c;
        break;
    }
  }

  if(n_curly_open != 0)
    Rcpp::stop("Found unbalanced curly braces (e.g., {{}) in your syntax.");

  // if the syntax does not end with a new line -> add last element:
  if(current_syntax.length() != 0)
    cleaned_syntax.push_back(current_syntax);

  return(cleaned_syntax);
}

