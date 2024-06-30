#include <Rcpp.h>
#include "string_operations.h"
#include "check_syntax.h"

std::vector<str_rhs_elem> split_eqation_rhs(std::string rhs){

  std::vector<str_rhs_elem> str_elems;

  // split at +. This separates different rhs elements
  std::vector<std::string> rhs_split = split_string_all(rhs, '+');

  // split right hand side further at the modifier
  for(std::string rhs_split_elem: rhs_split){
    str_rhs_elem current_elem;

    // check for modifier (*)
    std::vector<std::string> split_modifiers = split_string_all(rhs_split_elem, '*');

    if(split_modifiers.size() > 2){

      Rcpp::stop("The following element seems to have more than two modifiers: " +
        rhs_split_elem);

    }else if(split_modifiers.size() == 2){

      check_modifier(split_modifiers.at(0));

      current_elem.modifier = split_modifiers.at(0);
      current_elem.rhs      = split_modifiers.at(1);

    }else if(split_modifiers.size() == 1){

      current_elem.modifier = "";
      current_elem.rhs      = split_modifiers.at(0);

    }else{
      Rcpp::stop("Could not parse the following element: " + rhs_split_elem);
    }

    str_elems.push_back(current_elem);

  }

  return(str_elems);
}
