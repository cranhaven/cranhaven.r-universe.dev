#include "squatQTSClass.h"
#include "squatSO3Utils.h"
#include <RcppEigen.h>
#include <algorithm>

Rcpp::DataFrame reorient_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nSamples = qts.nrows();
  Eigen::Quaterniond qValue;
  Rcpp::DataFrame resValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = resValue["w"];
  Rcpp::NumericVector xValues = resValue["x"];
  Rcpp::NumericVector yValues = resValue["y"];
  Rcpp::NumericVector zValues = resValue["z"];

  Eigen::Quaterniond refValue;
  refValue.w() = wValues(0);
  refValue.x() = xValues(0);
  refValue.y() = yValues(0);
  refValue.z() = zValues(0);
  refValue = refValue.inverse();

  for (unsigned int i = 0;i < nSamples;++i)
  {
    qValue.w() = wValues(i);
    qValue.x() = xValues(i);
    qValue.y() = yValues(i);
    qValue.z() = zValues(i);

    qValue = refValue * qValue;

    wValues(i) = qValue.w();
    xValues(i) = qValue.x();
    yValues(i) = qValue.y();
    zValues(i) = qValue.z();
  }

  resValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return resValue;
}

Rcpp::DataFrame normalize_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];
  Eigen::Quaterniond qValue;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    qValue.w() = wValues(i);
    qValue.x() = xValues(i);
    qValue.y() = yValues(i);
    qValue.z() = zValues(i);
    qValue.normalize();
    wValues(i) = qValue.w();
    xValues(i) = qValue.x();
    yValues(i) = qValue.y();
    zValues(i) = qValue.z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame derivative_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];
  Eigen::Quaterniond currentQValue, previousQvalue;
  currentQValue.w() = wValues(nGrid - 1);
  currentQValue.x() = xValues(nGrid - 1);
  currentQValue.y() = yValues(nGrid - 1);
  currentQValue.z() = zValues(nGrid - 1);

  for (unsigned int i = nGrid - 1;i > 0;--i)
  {
    previousQvalue.w() = wValues(i - 1);
    previousQvalue.x() = xValues(i - 1);
    previousQvalue.y() = yValues(i - 1);
    previousQvalue.z() = zValues(i - 1);

    currentQValue = previousQvalue.inverse() * currentQValue;

    wValues(i) = currentQValue.w();
    xValues(i) = currentQValue.x();
    yValues(i) = currentQValue.y();
    zValues(i) = currentQValue.z();

    currentQValue = previousQvalue;
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame log_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];
  Eigen::Quaterniond qValue;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    qValue = Eigen::Quaterniond(wValues(i), xValues(i), yValues(i), zValues(i));
    qValue = logq(qValue);
    wValues(i) = qValue.w();
    xValues(i) = qValue.x();
    yValues(i) = qValue.y();
    zValues(i) = qValue.z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame exp_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];
  Eigen::Quaterniond qValue;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    qValue = Eigen::Quaterniond(wValues(i), xValues(i), yValues(i), zValues(i));
    qValue = expq(qValue);
    wValues(i) = qValue.w();
    xValues(i) = qValue.x();
    yValues(i) = qValue.y();
    zValues(i) = qValue.z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::List centring_qts_impl(const Rcpp::DataFrame &qts, const bool standardize)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];

  std::vector<Eigen::VectorXd> qValues(nGrid);
  Eigen::Vector4d meanValue;
  for (unsigned int i = 0;i < nGrid;++i)
  {
    meanValue(0) = wValues(i);
    meanValue(1) = xValues(i);
    meanValue(2) = yValues(i);
    meanValue(3) = zValues(i);
    qValues[i] = meanValue;
  }

  meanValue = gmean(qValues);
  Eigen::Quaterniond meanQValue(meanValue(0), meanValue(1), meanValue(2), meanValue(3)), workQValue;
  meanQValue = meanQValue.inverse();

  for (unsigned int i = 0;i < nGrid;++i)
  {
    workQValue = Eigen::Quaterniond(wValues(i), xValues(i), yValues(i), zValues(i));
    workQValue = meanQValue * workQValue;
    wValues(i) = workQValue.w();
    xValues(i) = workQValue.x();
    yValues(i) = workQValue.y();
    zValues(i) = workQValue.z();
  }

  double sdValue = 0;
  if (standardize)
  {
    outValue = log_qts_impl(outValue);
    sdValue = std::sqrt(gvariance(qValues, meanValue));
    wValues = outValue["w"];
    xValues = outValue["x"];
    yValues = outValue["y"];
    zValues = outValue["z"];
    wValues = wValues / sdValue;
    xValues = xValues / sdValue;
    yValues = yValues / sdValue;
    zValues = zValues / sdValue;
    outValue = exp_qts_impl(outValue);
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");

  return Rcpp::List::create(
    Rcpp::Named("qts") = outValue,
    Rcpp::Named("mean") = meanValue,
    Rcpp::Named("sd") = sdValue
  );
}

Rcpp::DataFrame resample_qts_impl(const Rcpp::DataFrame &qts,
                                  double tmin,
                                  double tmax,
                                  const unsigned int nout)
{
  // Assumes qts$time is sorted in ascending order
  int sizeIn = qts.nrows();
  unsigned int sizeOut = (nout == 0) ? sizeIn : nout;

  Eigen::Quaterniond Qinf, Qsup, Qinterp;
  double xinf, xsup;

  Rcpp::NumericVector inputTimeValues = qts["time"];
  Rcpp::NumericVector inputWValues = qts["w"];
  Rcpp::NumericVector inputXValues = qts["x"];
  Rcpp::NumericVector inputYValues = qts["y"];
  Rcpp::NumericVector inputZValues = qts["z"];

  if (R_IsNA(tmin))
    tmin = inputTimeValues(0);
  if (R_IsNA(tmax))
    tmax = inputTimeValues(sizeIn - 1);

  auto posInf = inputTimeValues.begin();
  auto posSup = inputTimeValues.begin();
  auto oldPosInf = inputTimeValues.end();
  auto oldPosSup = inputTimeValues.end();

  Rcpp::NumericVector outputTimeValues = Rcpp::wrap(Eigen::VectorXd::LinSpaced(sizeOut, tmin, tmax));;
  Rcpp::NumericVector outputWValues = Rcpp::NumericVector(sizeOut);
  Rcpp::NumericVector outputXValues = Rcpp::NumericVector(sizeOut);
  Rcpp::NumericVector outputYValues = Rcpp::NumericVector(sizeOut);
  Rcpp::NumericVector outputZValues = Rcpp::NumericVector(sizeOut);

  Rcpp::DataFrame outputValue = Rcpp::DataFrame::create(
    Rcpp::Named("time") = outputTimeValues,
    Rcpp::Named("w") = outputWValues,
    Rcpp::Named("x") = outputXValues,
    Rcpp::Named("y") = outputYValues,
    Rcpp::Named("z") = outputZValues
  );
  outputValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");

  double epsValue = std::sqrt(std::numeric_limits<double>::epsilon());

  for (unsigned int i = 0;i < sizeOut;++i)
  {
    // Grab new time point
    double tnew = outputTimeValues(i);

    // Assign NA quaternion to output if new point is not in QTS range
    if (tnew < inputTimeValues(0) || tnew > inputTimeValues(sizeIn - 1))
    {
      outputWValues(i) = NA_REAL;
      outputXValues(i) = NA_REAL;
      outputYValues(i) = NA_REAL;
      outputZValues(i) = NA_REAL;
      continue;
    }

    if (posSup == inputTimeValues.end())
    {
      outputWValues(i) = inputWValues(sizeIn - 1);
      outputXValues(i) = inputXValues(sizeIn - 1);
      outputYValues(i) = inputYValues(sizeIn - 1);
      outputZValues(i) = inputZValues(sizeIn - 1);
      continue;
    }

    posSup = std::upper_bound(posSup, inputTimeValues.end(), tnew);
    posInf = posSup - 1;

    if (posInf != oldPosInf)
    {
      unsigned int idxInf = posInf - inputTimeValues.begin();
      xinf = *posInf;
      Qinf.w() = inputWValues(idxInf);
      Qinf.x() = inputXValues(idxInf);
      Qinf.y() = inputYValues(idxInf);
      Qinf.z() = inputZValues(idxInf);
    }

    Qinterp = Qinf;

    if (posSup != inputTimeValues.end())
    {
      if (posSup != oldPosSup)
      {
        unsigned int idxSup = posSup - inputTimeValues.begin();
        xsup = *posSup;
        Qsup.w() = inputWValues(idxSup);
        Qsup.x() = inputXValues(idxSup);
        Qsup.y() = inputYValues(idxSup);
        Qsup.z() = inputZValues(idxSup);
      }

      if (xsup > xinf)
      {
        double range = xsup - xinf;
        double alpha = (tnew - xinf) / range;
        Qinterp = Qinf.slerp(alpha, Qsup);
      }
    }

    outputWValues(i) = Qinterp.w();
    outputXValues(i) = Qinterp.x();
    outputYValues(i) = Qinterp.y();
    outputZValues(i) = Qinterp.z();

    oldPosSup = posSup;
    oldPosInf = posInf;
  }

  return outputValue;
}

Rcpp::DataFrame smooth_qts_impl(const Rcpp::DataFrame &qts,
                                const double alpha)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];
  std::vector<Eigen::Quaterniond> qValues(nGrid);

  for (unsigned int i = 0;i < nGrid;++i)
  {
    qValues[i] = Eigen::Quaterniond(wValues(i), xValues(i), yValues(i), zValues(i));
    if (i == 0)
      continue;
    qValues[i] = qValues[i].slerp(alpha, qValues[i - 1]);
  }

  for (int i = nGrid - 2;i >= 0;--i)
  {
    qValues[i] = qValues[i].slerp(alpha, qValues[i + 1]);
    wValues(i) = qValues[i].w();
    xValues(i) = qValues[i].x();
    yValues(i) = qValues[i].y();
    zValues(i) = qValues[i].z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame hemispherize_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];

  std::vector<Eigen::Quaterniond> qValues(nGrid);
  for (unsigned int i = 0;i < nGrid;++i)
  {
    qValues[i] = Eigen::Quaterniond(wValues(i), xValues(i), yValues(i), zValues(i));
    if (i == 0)
      continue;
    if (qValues[i].dot(qValues[i - 1]) < 0)
    {
      qValues[i].coeffs() *= -1.0;
      wValues(i) = qValues[i].w();
      xValues(i) = qValues[i].x();
      yValues(i) = qValues[i].y();
      zValues(i) = qValues[i].z();
    }
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame moving_average_qts_impl(const Rcpp::DataFrame &qts,
                                        const unsigned int window_size)
{
  unsigned int nGrid = qts.nrows();
  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector inputWValues = qts["w"];
  Rcpp::NumericVector inputXValues = qts["x"];
  Rcpp::NumericVector inputYValues = qts["y"];
  Rcpp::NumericVector inputZValues = qts["z"];
  Eigen::Vector4d inputFirstQValue = {inputWValues(0), inputXValues(0), inputYValues(0), inputZValues(0)};
  Rcpp::NumericVector outputWValues = outValue["w"];
  Rcpp::NumericVector outputXValues = outValue["x"];
  Rcpp::NumericVector outputYValues = outValue["y"];
  Rcpp::NumericVector outputZValues = outValue["z"];
  std::vector<Eigen::VectorXd> qValues;
  Eigen::Vector4d qVector;

  for (int i = 0;i < nGrid;++i)
  {
    unsigned int minIndex = std::max(0, i - (int)window_size);
    unsigned int maxIndex = std::min((int)nGrid - 1, i + (int)window_size);
    qValues.resize(maxIndex - minIndex + 1);
    for (unsigned int j = minIndex;j <= maxIndex;++j)
    {
      qVector = {inputWValues(j), inputXValues(j), inputYValues(j), inputZValues(j)};
      qValues[j - minIndex] = qVector;
    }
    qVector = gmedian(qValues);
    outputWValues(i) = qVector(0);
    outputXValues(i) = qVector(1);
    outputYValues(i) = qVector(2);
    outputZValues(i) = qVector(3);
  }

  outValue = hemispherize_qts_impl(outValue);

  Eigen::Vector4d outputFirstQValue = {outputWValues(0), outputXValues(0), outputYValues(0), outputZValues(0)};

  if (outputFirstQValue.dot(inputFirstQValue) < 0)
  {
    outputWValues = -1.0 * outputWValues;
    outputXValues = -1.0 * outputXValues;
    outputYValues = -1.0 * outputYValues;
    outputZValues = -1.0 * outputZValues;
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame multiply_qts_impl(const Rcpp::DataFrame &qts_left,
                                  const Rcpp::DataFrame &qts_right)
{
  unsigned int nGrid = qts_left.nrows();
  if (qts_right.nrows() != nGrid)
    Rcpp::stop("The left and right QTS should be of the same length.");

  Rcpp::DataFrame outValue = Rcpp::clone(qts_left);
  Rcpp::NumericVector lhsWValues = outValue["w"];
  Rcpp::NumericVector lhsXValues = outValue["x"];
  Rcpp::NumericVector lhsYValues = outValue["y"];
  Rcpp::NumericVector lhsZValues = outValue["z"];
  Rcpp::NumericVector rhsWValues = qts_right["w"];
  Rcpp::NumericVector rhsXValues = qts_right["x"];
  Rcpp::NumericVector rhsYValues = qts_right["y"];
  Rcpp::NumericVector rhsZValues = qts_right["z"];
  Eigen::Quaterniond lhsQuat, rhsQuat;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    lhsQuat = Eigen::Quaterniond(lhsWValues(i), lhsXValues(i), lhsYValues(i), lhsZValues(i));
    rhsQuat = Eigen::Quaterniond(rhsWValues(i), rhsXValues(i), rhsYValues(i), rhsZValues(i));
    lhsQuat *= rhsQuat;
    lhsWValues(i) = lhsQuat.w();
    lhsXValues(i) = lhsQuat.x();
    lhsYValues(i) = lhsQuat.y();
    lhsZValues(i) = lhsQuat.z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

Rcpp::DataFrame inverse_qts_impl(const Rcpp::DataFrame &qts)
{
  unsigned int nGrid = qts.nrows();

  Rcpp::DataFrame outValue = Rcpp::clone(qts);
  Rcpp::NumericVector wValues = outValue["w"];
  Rcpp::NumericVector xValues = outValue["x"];
  Rcpp::NumericVector yValues = outValue["y"];
  Rcpp::NumericVector zValues = outValue["z"];
  Eigen::Quaterniond currQuat;

  for (unsigned int i = 0;i < nGrid;++i)
  {
    currQuat = Eigen::Quaterniond(wValues(i), xValues(i), yValues(i), zValues(i));
    currQuat = currQuat.inverse();
    wValues(i) = currQuat.w();
    xValues(i) = currQuat.x();
    yValues(i) = currQuat.y();
    zValues(i) = currQuat.z();
  }

  outValue.attr("class") = Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  return outValue;
}

