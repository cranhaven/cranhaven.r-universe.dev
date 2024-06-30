#include "find_variables.h"

variables find_variables(const parameter_table& pt){

  variables vars;
  std::vector<std::string> all_variables;
  add_unique(all_variables, pt.lhs);
  add_unique(all_variables, pt.rhs);
  std::vector<std::string> manifests;
  std::vector<std::string> latents;

  for(unsigned int i = 0; i < pt.op.size(); i++){
    if(pt.op.at(i).compare("=~") == 0){
      latents.push_back(pt.lhs.at(i));
    }
  }
  // all variables that are not latent are manifest.
  for(std::string av: all_variables){
    if(av.compare("1") == 0)
      continue; // skip intercepts
    bool is_manifest = true;
    for(std::string lv: latents){
      if(av.compare(lv) == 0){
        is_manifest = false;
        break;
      }
    }
    if(is_manifest)
      manifests.push_back(av);
  }

  add_unique(vars.manifests, manifests);
  add_unique(vars.latents, latents);

  return(vars);
}
