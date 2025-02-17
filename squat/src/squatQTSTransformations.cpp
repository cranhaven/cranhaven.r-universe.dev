#include "squatQTSTransformations.h"
#include <RcppEigen.h>

Rcpp::DataFrame qts2dts_impl(const Rcpp::DataFrame &first_qts,
                             const Rcpp::DataFrame &second_qts)
{
  unsigned int nGrid = first_qts.nrows();
  Rcpp::NumericVector firstWValues = first_qts["w"];
  Rcpp::NumericVector firstXValues = first_qts["x"];
  Rcpp::NumericVector firstYValues = first_qts["y"];
  Rcpp::NumericVector firstZValues = first_qts["z"];
  Rcpp::NumericVector secondWValues = second_qts["w"];
  Rcpp::NumericVector secondXValues = second_qts["x"];
  Rcpp::NumericVector secondYValues = second_qts["y"];
  Rcpp::NumericVector secondZValues = second_qts["z"];
  Eigen::Quaterniond firstQValue, secondQValue;

  Rcpp::NumericVector distanceValues(nGrid);
  for (unsigned int i = 0;i < nGrid;++i)
  {
    firstQValue = Eigen::Quaterniond(
      firstWValues(i),
      firstXValues(i),
      firstYValues(i),
      firstZValues(i)
    );
    secondQValue = Eigen::Quaterniond(
      secondWValues(i),
      secondXValues(i),
      secondYValues(i),
      secondZValues(i)
    );
    distanceValues(i) = secondQValue.angularDistance(firstQValue);
  }

  Rcpp::DataFrame outValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = first_qts["time"],
    Rcpp::Named("distance") = distanceValues
  );

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame qts2nts_impl(const Rcpp::DataFrame &qts,
                             const bool disable_normalization)
{
  unsigned int nSamples = qts.nrows();
  Eigen::Quaterniond qValue;
  Rcpp::NumericVector normValues(nSamples);
  Rcpp::NumericVector wValues = qts["w"];
  Rcpp::NumericVector xValues = qts["x"];
  Rcpp::NumericVector yValues = qts["y"];
  Rcpp::NumericVector zValues = qts["z"];

  Eigen::Quaterniond refValue;
  refValue.w() = 1.0;
  refValue.x() = 0.0;
  refValue.y() = 0.0;
  refValue.z() = 0.0;
  if (!disable_normalization)
    refValue.normalize();

  for (unsigned int i = 0;i < nSamples;++i)
  {
    qValue.w() = wValues(i);
    qValue.x() = xValues(i);
    qValue.y() = yValues(i);
    qValue.z() = zValues(i);
    if (!disable_normalization)
      qValue.normalize();

    normValues(i) = qValue.angularDistance(refValue);
  }

  Rcpp::DataFrame outValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = qts["time"],
    Rcpp::Named("norm") = normValues
  );

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame qts2ats_impl(const Rcpp::DataFrame &qts,
                             const bool disable_normalization)
{
  unsigned int nSamples = qts.nrows();
  Eigen::Quaterniond qValue;
  Rcpp::NumericVector angleValues(nSamples);
  Rcpp::NumericVector wValues = qts["w"];
  Rcpp::NumericVector xValues = qts["x"];
  Rcpp::NumericVector yValues = qts["y"];
  Rcpp::NumericVector zValues = qts["z"];

  Eigen::Quaterniond refValue;
  refValue.w() = wValues(0);
  refValue.x() = xValues(0);
  refValue.y() = yValues(0);
  refValue.z() = zValues(0);
  if (!disable_normalization)
    refValue.normalize();

  for (unsigned int i = 0;i < nSamples;++i)
  {
    qValue.w() = wValues(i);
    qValue.x() = xValues(i);
    qValue.y() = yValues(i);
    qValue.z() = zValues(i);
    if (!disable_normalization)
      qValue.normalize();

    angleValues(i) = qValue.angularDistance(refValue);
  }

  Rcpp::DataFrame outValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = qts["time"],
    Rcpp::Named("angle") = angleValues
  );

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame qts2avts_impl(const Rcpp::DataFrame &qts, const bool body_frame)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::NumericVector inputTValues = qts["time"];
  Rcpp::NumericVector inputWValues = qts["w"];
  Rcpp::NumericVector inputXValues = qts["x"];
  Rcpp::NumericVector inputYValues = qts["y"];
  Rcpp::NumericVector inputZValues = qts["z"];
  Rcpp::NumericVector outputTValues(nGrid - 1);
  Rcpp::NumericVector outputXValues(nGrid - 1);
  Rcpp::NumericVector outputYValues(nGrid - 1);
  Rcpp::NumericVector outputZValues(nGrid - 1);

  Eigen::Quaterniond prevQValue, currQValue;
  for (unsigned int i = 1;i < nGrid;++i)
  {
    prevQValue = Eigen::Quaterniond(inputWValues(i - 1), inputXValues(i - 1), inputYValues(i - 1), inputZValues(i - 1));
    currQValue = Eigen::Quaterniond(inputWValues(i), inputXValues(i), inputYValues(i), inputZValues(i));
    double deltaTime = inputTValues(i) - inputTValues(i - 1);

    currQValue = (body_frame) ? prevQValue.inverse() * currQValue : currQValue * prevQValue.inverse();

    currQValue.coeffs() *= (2.0 / deltaTime);
    outputTValues(i - 1) = inputTValues(i);
    outputXValues(i - 1) = currQValue.x();
    outputYValues(i - 1) = currQValue.y();
    outputZValues(i - 1) = currQValue.z();
  }

  Rcpp::DataFrame outValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = outputTValues,
    Rcpp::Named("x") = outputXValues,
    Rcpp::Named("y") = outputYValues,
    Rcpp::Named("z") = outputZValues
  );

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame qts2aats_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector angleValues = outValue["w"];
  Rcpp::NumericVector axisXValues = outValue["x"];
  Rcpp::NumericVector axisYValues = outValue["y"];
  Rcpp::NumericVector axisZValues = outValue["z"];

  Eigen::Quaterniond quatValue;
  Eigen::AngleAxisd axisAngleValue;
  for (unsigned int i = 0;i < nGrid;++i)
  {
    quatValue.w() = angleValues(i);
    quatValue.x() = axisXValues(i);
    quatValue.y() = axisYValues(i);
    quatValue.z() = axisZValues(i);
    axisAngleValue = Eigen::AngleAxisd(quatValue);
    angleValues(i) = axisAngleValue.angle();
    axisXValues(i) = axisAngleValue.axis().x();
    axisYValues(i) = axisAngleValue.axis().y();
    axisZValues(i) = axisAngleValue.axis().z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}
