#include <Rcpp.h>
#include "check_syntax.h"

void check_modifier(const std::string modifier){

    if(modifier.compare("NA") == 0){
      std::string wrn = "NA found as modifier (e.g., label) for one of the parameters. ";
      Rcpp::warning(
        wrn +
          "Note that this does not set a loading to being freely estimated in mxsem. " +
          "Use the argument scale_loadings = FALSE to freely estimate all loadings and " +
          "scale_latent_variances = TRUE to set latent variances to 1."
      );
    }
}
