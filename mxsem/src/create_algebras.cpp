#include <Rcpp.h>
#include "string_operations.h"
#include "clean_syntax.h"
#include "create_algebras.h"

bool is_in_curly(const std::string& what, const std::string& where){
  int n_curly = 0;
  unsigned int match   = 0;
  for(char c: where){
    if(c == '{')
      n_curly++;
    if(c == '}'){
      if(n_curly == 0)
        Rcpp::stop("Unmatched closing bracket in " + where);
      n_curly--;
      }
    if(c == what[match]){
      if(match == what.size()-1){
        if(n_curly != 0)
          return(true);
        return(false);
      }
      match++;
    }
  }

  Rcpp::stop("No match found");

}

void make_algebras(const std::vector<std::string>& equations,
                   parameter_table& pt){

  algebra alg;

  // find newly created variables
  for(std::string eq: equations){
    if(eq.at(0) == '{'){
      // skip user defined
      continue;
    }
    if(eq.at(0) == '!'){
      // remove exclamation mark
      eq.erase(eq.begin());
      // check_lhs can be used to check for disallowed elements:
      check_lhs(eq, "!+*=~: ");
      alg.new_parameters.push_back(eq);
      alg.new_parameters_free.push_back("TRUE");
    }
  }

  equation_elements eq_elem;

  for(std::string eq: equations){
    if(eq.at(0) == '{'){
      // skip user defined
      continue;
    }
    std::vector<std::string> check_for = {":="};

    for(std::string c_for: check_for){

      if(eq.find(c_for) != std::string::npos){

        // skip if := is in a curly brace. Such algebras
        // will be taken care of separately
        if(is_in_curly(c_for, eq))
          continue;

        eq_elem = split_string_once(eq, c_for);

        check_lhs(eq_elem.lhs);

        alg.lhs.push_back(eq_elem.lhs);
        alg.op.push_back(c_for);
        alg.rhs.push_back(eq_elem.rhs);

        // If an element is on the left hand side of an equation, it is no longer free:
        for(unsigned int i = 0; i < pt.modifier.size(); i++){
          if(pt.modifier.at(i).compare(eq_elem.lhs) == 0)
            pt.free.at(i) = "FALSE";
        }
        for(unsigned int i = 0; i < alg.new_parameters_free.size(); i++){
          if(alg.new_parameters.at(i).compare(eq_elem.lhs) == 0)
            alg.new_parameters_free.at(i) = "FALSE";
        }

        // we don't check the subsequent elements
        break;
      }

    }
  }

  pt.alg = alg;
}
