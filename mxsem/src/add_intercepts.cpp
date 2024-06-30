#include <Rcpp.h>
#include "parameter_table.h"
#include "add_elements.h"

void add_intercepts(parameter_table& pt){

  std::vector<std::string> manifests = pt.vars.manifests;

  bool has_intercept = false;
  for(unsigned int i = 0; i < manifests.size(); i++){
    // skip intercepts:
    has_intercept = false;

    for(unsigned int j = 0; j < pt.lhs.size(); j++){
      // check if variable was found
      if(pt.lhs.at(j).compare(manifests.at(i)) == 0){

        if((pt.op.at(j).compare("~") == 0) &&
           (pt.rhs.at(j).compare("1") == 0)){
          has_intercept = true;
          break;
        }
      }
    }
    if(!has_intercept){
      pt.add_line();
      pt.lhs.at(pt.lhs.size()-1) = manifests.at(i);
      pt.rhs.at(pt.rhs.size()-1) = "1";
      pt.op.at(pt.op.size()-1) = "~";
    }
  }

}
