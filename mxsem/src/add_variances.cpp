#include "add_elements.h"

void add_variances(parameter_table& pt){

  std::vector<std::string> variables;
  add_unique(variables, pt.lhs);
  add_unique(variables, pt.rhs);

  bool has_variance = false;
  for(unsigned int i = 0; i < variables.size(); i++){
    // skip intercepts:
    if(variables.at(i).compare("1") == 0)
      continue;
    has_variance = false;

    for(unsigned int j = 0; j < pt.lhs.size(); j++){
      // check if variable was found
      if(pt.lhs.at(j).compare(variables.at(i)) == 0){

        if(pt.op.at(j).compare("~~") == 0){
          // is (co)variance
          if(pt.lhs.at(j).compare(pt.rhs.at(j)) == 0){
            // is variance
            has_variance = true;
            break;
          }
        }
      }
    }

    if(!has_variance){
      pt.add_line();
      pt.lhs.at(pt.lhs.size()-1) = variables.at(i);
      pt.rhs.at(pt.rhs.size()-1) = variables.at(i);
      pt.op.at(pt.op.size()-1) = "~~";
    }
  }

}
