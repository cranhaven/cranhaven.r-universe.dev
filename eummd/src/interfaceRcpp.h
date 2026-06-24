#ifndef GUARD_interfaceRcpp_h
#define GUARD_interfaceRcpp_h

#include<Rcpp.h>


Rcpp::List mmd_lap_Rcpp(Rcpp::NumericVector X, 
                        Rcpp::NumericVector Y, 
                        Rcpp::IntegerVector nX_, 
                        Rcpp::IntegerVector dX_,
                        Rcpp::IntegerVector nY_, 
                        Rcpp::IntegerVector dY_,
                        Rcpp::NumericVector beta_);


Rcpp::List mmd_gau_Rcpp(Rcpp::NumericVector X, 
                        Rcpp::NumericVector Y, 
                        Rcpp::IntegerVector nX_, 
                        Rcpp::IntegerVector dX_,
                        Rcpp::IntegerVector nY_, 
                        Rcpp::IntegerVector dY_,
                        Rcpp::NumericVector beta_);


Rcpp::List mmd_lap_pval_Rcpp(Rcpp::NumericVector X, 
                             Rcpp::NumericVector Y, 
                             Rcpp::IntegerVector nX_, 
                             Rcpp::IntegerVector dX_,
                             Rcpp::IntegerVector nY_, 
                             Rcpp::IntegerVector dY_,
                             Rcpp::IntegerVector numperm_,
                             Rcpp::IntegerVector seednum_,
                             Rcpp::NumericVector beta_, 
                             Rcpp::IntegerVector twosided_,
                             Rcpp::IntegerVector boundedminpval_);


Rcpp::List mmd_gau_pval_Rcpp(Rcpp::NumericVector X, 
                             Rcpp::NumericVector Y, 
                             Rcpp::IntegerVector nX_, 
                             Rcpp::IntegerVector dX_,
                             Rcpp::IntegerVector nY_, 
                             Rcpp::IntegerVector dY_,
                             Rcpp::IntegerVector numperm_,
                             Rcpp::IntegerVector seednum_,
                             Rcpp::NumericVector beta_,
                             Rcpp::IntegerVector twosided_, 
                             Rcpp::IntegerVector boundedminpval_);


Rcpp::NumericVector fast_median_diff_Rcpp(Rcpp::NumericVector X_);


Rcpp::NumericVector naive_median_diff_Rcpp(Rcpp::NumericVector Z_, 
                                           Rcpp::IntegerVector nZ_, 
                                           Rcpp::IntegerVector dZ_,
                                           Rcpp::IntegerVector kmethod_);


Rcpp::List eummd_Rcpp(Rcpp::NumericVector X_, 
                      Rcpp::NumericVector Y_, 
                      Rcpp::NumericVector beta_);


Rcpp::List eummd_pval_Rcpp(Rcpp::NumericVector X_, 
                           Rcpp::NumericVector Y_, 
                           Rcpp::NumericVector beta_,
                           Rcpp::IntegerVector numperm_,
                           Rcpp::IntegerVector seednum_,
                           Rcpp::IntegerVector twosided_, 
                           Rcpp::IntegerVector boundedminpval_);


double meammd_proj_Rcpp(Rcpp::NumericVector X, 
                        Rcpp::NumericVector Y, 
                        Rcpp::IntegerVector nX_, 
                        Rcpp::IntegerVector dX_, 
                        Rcpp::IntegerVector nY_, 
                        Rcpp::IntegerVector dY_, 
                        Rcpp::IntegerVector numproj_,
                        Rcpp::NumericVector beta_);


Rcpp::List meammd_proj_pval_Rcpp(Rcpp::NumericVector X, 
                                 Rcpp::NumericVector Y, 
                                 Rcpp::IntegerVector nX_, 
                                 Rcpp::IntegerVector dX_, 
                                 Rcpp::IntegerVector nY_, 
                                 Rcpp::IntegerVector dY_, 
                                 Rcpp::IntegerVector numperm_,
                                 Rcpp::IntegerVector numproj_,
                                 Rcpp::IntegerVector seednum_,
                                 Rcpp::NumericVector beta_,
                                 Rcpp::IntegerVector twosided_, 
                                 Rcpp::IntegerVector boundedminpval_);


double meammd_dist_pval_Rcpp(Rcpp::NumericVector X, 
                             Rcpp::NumericVector Y, 
                             Rcpp::IntegerVector nX_, 
                             Rcpp::IntegerVector dX_, 
                             Rcpp::IntegerVector nY_, 
                             Rcpp::IntegerVector dY_, 
                             Rcpp::IntegerVector numperm_,
                             Rcpp::IntegerVector seednum_,
                             Rcpp::NumericVector beta_, 
                             Rcpp::NumericVector pmethod_, 
                             Rcpp::NumericVector nmethod_,
                             Rcpp::IntegerVector twosided_, 
                             Rcpp::IntegerVector boundedminpval_);

Rcpp::List energydist_Rcpp(Rcpp::NumericVector X, 
                           Rcpp::NumericVector Y, 
                           Rcpp::IntegerVector nX_, 
                           Rcpp::IntegerVector dX_,
                           Rcpp::IntegerVector nY_, 
                           Rcpp::IntegerVector dY_);


Rcpp::List energydist_pval_Rcpp(Rcpp::NumericVector X, 
                                Rcpp::NumericVector Y, 
                                Rcpp::IntegerVector nX_, 
                                Rcpp::IntegerVector dX_,
                                Rcpp::IntegerVector nY_, 
                                Rcpp::IntegerVector dY_,
                                Rcpp::IntegerVector numperm_,
                                Rcpp::IntegerVector seednum_, 
                                Rcpp::IntegerVector twosided_, 
                                Rcpp::IntegerVector boundedminpval_);

#endif
