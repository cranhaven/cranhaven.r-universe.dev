#ifndef CORE_METHOD_ENTRY_NEAREST_ENTRY_H
#define CORE_METHOD_ENTRY_NEAREST_ENTRY_H

#include "CoreMethod.h"

extern const std::string METHOD_ENTRY_NEAREST_ENTRY;

class CoreMethodEntryNearestEntry: public CoreMethod {
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
    CoreMethodEntryNearestEntry(Rcpp::NumericMatrix & dm, Rcpp::List & g) : CoreMethod(METHOD_ENTRY_NEAREST_ENTRY, dm, g) {
    };
};

#endif
