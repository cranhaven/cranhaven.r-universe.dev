#include "scale_latent_variables.h"
#include "string_operations.h"

void scale_latent_variances(parameter_table& pt){
  std::vector<std::string> latents = pt.vars.latents;

  for(std::string& latent: latents){

    for(unsigned int i = 0; i < pt.lhs.size(); i++){

      if((pt.lhs.at(i).compare(latent) == 0) &&
         (pt.op.at(i).compare("~~") == 0) &&
         (pt.rhs.at(i).compare(latent) == 0)
      ){
        // it's a variance, now we only have to check if it is
        // already fixed
        if(pt.modifier.at(i).compare("") == 0){
          // not fixed
          pt.modifier.at(i) = "1.0";
          break;
        }else if(is_number(pt.modifier.at(i))){
          // is fixed
          Rcpp::Function message("message");
          message("Skipping the automatic scaling by constraining the variance of " + latent +
            ". The variable's variance was already scaled manually (e.g., eta ~~ 1*eta).");
        }else{
          Rcpp::warning("Automatic scaling by constraining the variance of " + latent +
            " failed because a label was assigned to the variance (e.g., eta ~~ var*eta).");
        }
      }
    }
  }
}

void scale_loadings(parameter_table& pt){
  std::vector<std::string> latents = pt.vars.latents;
  for(std::string& latent: latents){

    bool was_scaled = false; // set to true if the latent variable was already
    // scaled manually by the user
    int scale_location = -1; // set to the location of the parameter used
    // for scaling

    for(unsigned int i = 0; i < pt.lhs.size(); i++){

      // check if there is already a loading with fixed value
      if((pt.lhs.at(i).compare(latent) == 0) &&
         (pt.op.at(i).compare("=~") == 0) &&
         (is_number(pt.modifier.at(i)))
      ){
        was_scaled = true;
        break;
      }

      // if not, save the location of the first loading
      if((scale_location == -1) &&
         (pt.lhs.at(i).compare(latent) == 0) &&
         (pt.op.at(i).compare("=~") == 0) &&
         (pt.modifier.at(i).compare("") == 0)
      ){
        // set the first loading of each latent variable to 1
        scale_location = i;
      }
    }

    if(was_scaled){
      Rcpp::Function message("message");
      message("Skipping the automatic scaling of " + latent +
        ". The variable was already scaled manually (e.g., eta =~ 1*y1 + ...).");
    }
    if((!was_scaled) && (scale_location != -1))
      pt.modifier.at(scale_location) = "1.0";
    if((!was_scaled) && (scale_location == -1))
      Rcpp::warning("Automatically scaling latent variable " + latent +
        " failed. Could not find an unlabeled free loading on observed items." +
        " Did you give labels to all loadings? If so, remove the label for one of the items or manually" +
        " set one of the loadings to a fixed value (e.g., eta =~ 1*y1 + ...).");
  }
}
