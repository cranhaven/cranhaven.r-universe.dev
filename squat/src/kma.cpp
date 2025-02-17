#include "kma.h"
#include "squatQTSClass.h"
#include "squatSO3Utils.h"
#include <RcppEigen.h>

double GeodesicQuaternionDistance(const Rcpp::NumericMatrix &M1,
                                  const Rcpp::NumericMatrix &M2,
                                  const unsigned int index1,
                                  const unsigned int index2)
{
  Eigen::Quaterniond q1Value(M1(0, index1), M1(1, index1), M1(2, index1), M1(3, index1));
  Eigen::Quaterniond q2Value(M2(0, index2), M2(1, index2), M2(2, index2), M2(3, index2));
  return q1Value.angularDistance(q2Value);
}

Rcpp::NumericMatrix RegularizeGrid(const Rcpp::NumericVector &grid,
                                   const Rcpp::NumericMatrix &values,
                                   const double gridLowerBound,
                                   const double gridUpperBound,
                                   const unsigned int numberOfPoints)
{
  Rcpp::DataFrame qtsValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = grid,
    Rcpp::Named("w") = values.row(0),
    Rcpp::Named("x") = values.row(1),
    Rcpp::Named("y") = values.row(2),
    Rcpp::Named("z") = values.row(3)
  );

  qtsValue = resample_qts_impl(qtsValue, gridLowerBound, gridUpperBound, numberOfPoints);

  Rcpp::NumericMatrix outValue(4, numberOfPoints);
  outValue.row(0) = Rcpp::as<Rcpp::NumericVector>(qtsValue["w"]);
  outValue.row(1) = Rcpp::as<Rcpp::NumericVector>(qtsValue["x"]);
  outValue.row(2) = Rcpp::as<Rcpp::NumericVector>(qtsValue["y"]);
  outValue.row(3) = Rcpp::as<Rcpp::NumericVector>(qtsValue["z"]);

  return outValue;
}

Rcpp::NumericMatrix GetGeodesicMean(const Rcpp::NumericMatrix &values)
{
  unsigned int nGrid = values.rows();

  std::vector<Eigen::VectorXd> qValues(nGrid);
  for (unsigned int i = 0;i < nGrid;++i)
  {
    qValues[i].resize(4);
    for (unsigned int j = 0;j < 4;++j)
      qValues[i](j) = values(i, j);
  }

  Eigen::Vector4d meanQuaternion = gmean(qValues);
  Rcpp::NumericMatrix outValue(1, 4);
  for (unsigned int i = 0;i < 4;++i)
    outValue(0, i) = meanQuaternion(i);
  return outValue;
}
