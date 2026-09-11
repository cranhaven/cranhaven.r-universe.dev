#ifndef DEFM_COMMON_H
#define DEFM_COMMON_H

void check_covar(
  int & idx_,
  std::string & idx,
  Rcpp::XPtr< defm::DEFM > & ptr
) {

  // Retrieving the matching covariate
  if (idx != "")
  {

    // Getting the covariate names
    auto cnames = ptr->get_X_names();

    // Can we find it?
    for (size_t i = 0u; i < cnames.size(); ++i) {
      if (cnames[i] == idx)
      {
        idx_ = i;
        break;
      }
    }

    if (idx_ < 0)
      Rcpp::stop("The variable %s does not exists.", idx.c_str());

  }

}
#endif
