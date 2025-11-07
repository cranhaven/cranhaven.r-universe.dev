#ifndef CORE_SELECTION_H
#define CORE_SELECTION_H

#include <Rcpp.h>
#include <time.h>

class CoreSelection {
  protected:
    std::string method;
  public:
    static Rcpp::IntegerVector computeRandomSelection(Rcpp::NumericMatrix & dist, int requiredN, Rcpp::IntegerVector & preselected);
    static Rcpp::IntegerVector createSelectionResult(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & selected);
    static void initialise(int seed);
};

#endif
