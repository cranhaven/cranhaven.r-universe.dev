#ifndef CORE_METHOD_H
#define CORE_METHOD_H

#include <string>
#include <Rcpp.h>
#include <time.h>

class CoreMethod {
  protected:
    std::string method;
    Rcpp::NumericMatrix distanceMatrix;
    Rcpp::List groups;
    Rcpp::IntegerVector selectedFixedPositions;
    Rcpp::IntegerVector selectedRandomPositions;
    Rcpp::IntegerVector getDefaultInitial() {
      Rcpp::Environment base("package:base");
      Rcpp::Function numeric = base["as.integer"];
      return numeric(Rcpp::_["x"] = groups.names());
    }
  public:
    int accessionNumber;
    int coreNumber;
    int fixedCoreNumber;
    int randomCoreNumber;
    std::string getMethod();
    virtual Rcpp::IntegerVector getInitial() {
      Rcpp::Rcout << "Call to default getInitial, should not happen!" << std::endl;
      return Rcpp::IntegerVector(0);
    };
    static double measure (Rcpp::NumericMatrix & dm, Rcpp::IntegerVector & c) {
      Rcpp::Rcout << "Call to default measure, should not happen!" << std::endl;
      return 0;
    };
    virtual double measure (Rcpp::IntegerVector & c) {
      Rcpp::Rcout << "Call to default measure, should not happen!" << std::endl;
      return 0;
    };
    virtual bool improvement (double m1, double m2) {
      Rcpp::Rcout << "Call to default improvement, should not happen!" << std::endl;
      return FALSE;
    };
    virtual Rcpp::IntegerVector adjustRandomNeighbour (Rcpp::IntegerVector coreInstance, int i) {
      Rcpp::Rcout << "Call to default getRandomNeighbour, should not happen!" << std::endl;
      return FALSE;
    };
    Rcpp::IntegerVector getRandomNeighbour(Rcpp::IntegerVector coreInstance);
    Rcpp::IntegerVector getRandom();
    CoreMethod(std::string m, Rcpp::NumericMatrix & dm, Rcpp::List & g);
};

#endif
