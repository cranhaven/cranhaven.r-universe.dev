#ifndef CORE_METHOD_ENTRY_ENTRY_H
#define CORE_METHOD_ENTRY_ENTRY_H

#include "CoreMethod.h"

extern const std::string METHOD_ENTRY_ENTRY;

class CoreMethodEntryEntry: public CoreMethod {
  private:
    Rcpp::IntegerVector getLocalInitial() {
      return getDefaultInitial();
    }
  public:
    Rcpp::IntegerVector getInitial() {
      return getLocalInitial();
    }
    static double measure (Rcpp::NumericMatrix & dm, Rcpp::IntegerVector & c);
    double measure (Rcpp::IntegerVector & c);
    bool improvement (double m1, double m2);
    Rcpp::IntegerVector adjustRandomNeighbour(Rcpp::IntegerVector coreInstance, int i);
    CoreMethodEntryEntry(Rcpp::NumericMatrix & dm, Rcpp::List & g) : CoreMethod(METHOD_ENTRY_ENTRY, dm, g) {
    };
};

#endif
