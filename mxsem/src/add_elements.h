#ifndef ADD_ELEMENTS_H
#define ADD_ELEMENTS_H
#include "parameter_table.h"
#include <Rcpp.h>

void add_variances(parameter_table& pt);
void add_intercepts(parameter_table& pt);
void add_covariances(std::vector<std::string> variables,
                     parameter_table& pt);

inline void add_unique(std::vector<std::string>& where_to_add, const std::vector<std::string>& what_to_add){
  bool variable_exists = false;
  for(unsigned int i = 0; i < what_to_add.size(); i++){
    variable_exists = false;
    for(unsigned int j = 0; j < where_to_add.size(); j++){
      if(what_to_add.at(i).compare(where_to_add.at(j)) == 0){
        variable_exists = true;
        break;
      }
    }
    if(!variable_exists){
      where_to_add.push_back(what_to_add.at(i));
    }
  }
}

#endif
