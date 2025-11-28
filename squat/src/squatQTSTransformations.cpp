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

Rcpp::DataFrame qts2rpyts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::NumericVector inputWValues = qts["w"];
  Rcpp::NumericVector inputXValues = qts["x"];
  Rcpp::NumericVector inputYValues = qts["y"];
  Rcpp::NumericVector inputZValues = qts["z"];
  Rcpp::NumericVector outputRollValues(nGrid);
  Rcpp::NumericVector outputPitchValues(nGrid);
  Rcpp::NumericVector outputYawValues(nGrid);

  double roll, pitch, yaw;
  for (unsigned int i = 0;i < nGrid;++i)
  {
    GetRPYAngles(
      inputWValues(i), inputXValues(i), inputYValues(i), inputZValues(i),
      roll, pitch, yaw
    );
    outputRollValues(i)  = roll;
    outputPitchValues(i) = pitch;
    outputYawValues(i)   = yaw;
  }

  Rcpp::DataFrame outValue = Rcpp::DataFrame::create(
    Rcpp::Named("time")  = qts["time"],
    Rcpp::Named("roll")  = outputRollValues,
    Rcpp::Named("pitch") = outputPitchValues,
    Rcpp::Named("yaw")   = outputYawValues
  );

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame rpyts2qts_impl(const Rcpp::DataFrame &rpyts)
{
  unsigned int nGrid = rpyts.nrows();
  Rcpp::NumericVector inputRollValues  = rpyts["roll"];
  Rcpp::NumericVector inputPitchValues = rpyts["pitch"];
  Rcpp::NumericVector inputYawValues   = rpyts["yaw"];
  Rcpp::NumericVector outputWValues(nGrid);
  Rcpp::NumericVector outputXValues(nGrid);
  Rcpp::NumericVector outputYValues(nGrid);
  Rcpp::NumericVector outputZValues(nGrid);

  Eigen::Quaterniond quatValue;
  for (unsigned int i = 0;i < nGrid;++i)
  {
    quatValue = Eigen::AngleAxisd(inputYawValues(i), Eigen::Vector3d::UnitZ()) *
                Eigen::AngleAxisd(inputPitchValues(i), Eigen::Vector3d::UnitY()) *
                Eigen::AngleAxisd(inputRollValues(i), Eigen::Vector3d::UnitX());
    outputWValues(i) = quatValue.w();
    outputXValues(i) = quatValue.x();
    outputYValues(i) = quatValue.y();
    outputZValues(i) = quatValue.z();
  }

  Rcpp::DataFrame outValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = rpyts["time"],
    Rcpp::Named("w") = outputWValues,
    Rcpp::Named("x") = outputXValues,
    Rcpp::Named("y") = outputYValues,
    Rcpp::Named("z") = outputZValues
  );

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

void GetRPYAngles(const double &w, const double &x, const double &y, const double &z,
                  double &roll, double &pitch, double &yaw)
{
  // roll (x-axis rotation)
  double sinr_cosp = 2 * (w * x + y * z);
  double cosr_cosp = 1 - 2 * (x * x + y * y);
  roll = std::atan2(sinr_cosp, cosr_cosp);

  // pitch (y-axis rotation)
  double sinp = 2 * (w * y - z * x);
  if (std::abs(sinp) >= 1)
    pitch = std::copysign(M_PI / 2, sinp); // use 90 degrees if out of range
  else
    pitch = std::asin(sinp);
  // double sinp = std::sqrt(1 + 2 * (w * y - x * z));
  // double cosp = std::sqrt(1 - 2 * (w * y - x * z));
  // pitch = 2 * std::atan2(sinp, cosp) - M_PI / 2;

  // yaw (z-axis rotation)
  double siny_cosp = 2 * (w * z + x * y);
  double cosy_cosp = 1 - 2 * (y * y + z * z);
  yaw = std::atan2(siny_cosp, cosy_cosp);
}
