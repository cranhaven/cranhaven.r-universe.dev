#include "add_elements.h"

void add_covariances(std::vector<std::string> variables,
                     parameter_table& pt){

  std::vector<std::string> exogenous;

  bool is_exogenous = true;
  // now, we want to check of the variables are exogenous
  for(std::string var: variables){
    is_exogenous = true;
    for(unsigned int i = 0; i < pt.rhs.size(); i++){

      const std::string& lhs_var = pt.lhs.at(i);
      const std::string& op = pt.op.at(i);
      const std::string& rhs_var = pt.rhs.at(i);

      if(op.compare("=~") == 0){
        // we are checking a loading. In this case, the predictor is on the
        // right hand side
        // If we find that var is on the right hand side, we found an endogenous variable!
        if((var.compare(rhs_var) == 0)) // check if on right hand side
        {
          is_exogenous = false;
          break;
        }
      }else if(op.compare("~") == 0){
        // predictors are on the right hand side of equations (y ~ x).
        // If we find that var is on the left hand side, we found an endogenous variable!
        if((var.compare(lhs_var) == 0) && // check if on left hand side
           (rhs_var.compare("1") != 0) // check if intercept -> this does not count as predictor
        ){
          is_exogenous = false;
          break;
        }
      }
    }

    if(is_exogenous){
      exogenous.push_back(var);
    }
  }

  // no covariances
  if(exogenous.size() <= 1)
    return;

  // now, we check if all covariances between the latents exists. If not,
  // we add the covariance.
  bool covariance_exists = false;
  for(unsigned int i = 0; i < exogenous.size()-1; i++){
    for(unsigned int j = i+1; j < exogenous.size(); j++){

      covariance_exists = false;
      for(unsigned int k = 0; k < pt.lhs.size(); k++){

        if((pt.lhs.at(k).compare(exogenous.at(i)) == 0) &&
           (pt.op.at(k).compare("~~") == 0) &&
           (pt.rhs.at(k).compare(exogenous.at(j)) == 0)
        ){
          covariance_exists = true;
          break;
        }
        // the order does not matter:
        if((pt.lhs.at(k).compare(exogenous.at(j)) == 0) &&
           (pt.op.at(k).compare("~~") == 0) &&
           (pt.rhs.at(k).compare(exogenous.at(i)) == 0)
        ){
          covariance_exists = true;
          break;
        }
      }
      if(!covariance_exists){
        pt.add_line();
        pt.lhs.at(pt.lhs.size()-1) = exogenous.at(i);
        pt.rhs.at(pt.rhs.size()-1) = exogenous.at(j);
        pt.op.at(pt.op.size()-1) = "~~";
      }
    }
  }
}
