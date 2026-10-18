
#include <RcppArmadillo.h>
// [[Rcpp::init]]
void export_cpp_symbols(DllInfo *dll) {
  R_useDynamicSymbols(dll, TRUE);
}
