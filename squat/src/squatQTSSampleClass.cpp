#include "squatQTSSampleClass.h"
#include "squatSO3Utils.h"
#include <RcppEigen.h>

Rcpp::DataFrame mean_qts_impl(const Rcpp::List &qts_list)
{
  unsigned int nSamples = qts_list.size();
  Rcpp::DataFrame outValue, tmpValue;
  outValue = Rcpp::clone(qts_list)[0];
  unsigned int nGrid = outValue.nrows();
  Rcpp::NumericVector wValues, xValues, yValues, zValues;
  std::vector<Eigen::VectorXd> qValues(nSamples);
  Eigen::Vector4d avgQValue;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    for (unsigned int j = 0;j < nSamples;++j)
    {
      tmpValue = qts_list[j];
      wValues = tmpValue["w"];
      xValues = tmpValue["x"];
      yValues = tmpValue["y"];
      zValues = tmpValue["z"];
      avgQValue = {wValues(i), xValues(i), yValues(i), zValues(i)};
      qValues[j] = avgQValue;
    }

    avgQValue = gmean(qValues);

    wValues = outValue["w"];
    xValues = outValue["x"];
    yValues = outValue["y"];
    zValues = outValue["z"];
    wValues(i) = avgQValue(0);
    xValues(i) = avgQValue(1);
    yValues(i) = avgQValue(2);
    zValues(i) = avgQValue(3);
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("qts", "tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame median_qts_impl(const Rcpp::List &qts_list)
{
  unsigned int nSamples = qts_list.size();
  Rcpp::DataFrame outValue, tmpValue;
  outValue = Rcpp::clone(qts_list)[0];
  unsigned int nGrid = outValue.nrows();
  Rcpp::NumericVector wValues, xValues, yValues, zValues;
  std::vector<Eigen::VectorXd> qValues(nSamples);
  Eigen::Vector4d avgQValue;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    for (unsigned int j = 0;j < nSamples;++j)
    {
      tmpValue = qts_list[j];
      wValues = tmpValue["w"];
      xValues = tmpValue["x"];
      yValues = tmpValue["y"];
      zValues = tmpValue["z"];
      avgQValue = {wValues(i), xValues(i), yValues(i), zValues(i)};
      qValues[j] = avgQValue;
    }

    avgQValue = gmedian(qValues);

    wValues = outValue["w"];
    xValues = outValue["x"];
    yValues = outValue["y"];
    zValues = outValue["z"];
    wValues(i) = avgQValue(0);
    xValues(i) = avgQValue(1);
    yValues(i) = avgQValue(2);
    zValues(i) = avgQValue(3);
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("qts", "tbl_df", "tbl", "data.frame");
  return outValue;
}
