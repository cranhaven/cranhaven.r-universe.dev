#include "squatSO3Utils.h"

Eigen::Matrix3d logSO3C(const Eigen::Matrix3d &R)
{
  /* This function takes a 3-by-3 rotation matrix (in SO(3)) and
   returns its logarithm, a 3-by-3 skew symmetric matrix (in so(3)) */

  double workValue = 0.5 * R.trace() - 0.5;
  if (workValue > 1.0)
    workValue = 1.0;
  if (workValue < -1.0)
    workValue = -1.0;
  double theta = std::acos(workValue);
  double denomValue = 2.0 * std::sin(theta);

  if (denomValue < std::sqrt(std::numeric_limits<double>::epsilon()))
    return Eigen::Matrix3d::Zero();

  return (R - R.transpose()) * theta / denomValue;
}

Eigen::Matrix3d expskewC(const Eigen::Matrix3d &M)
{
  /* This function takes a 3-by-3 skew symmetric matrix (in so(3)) and
   returns its exponential, a 3-by-3 rotation matrix (in SO(3)) */

  Eigen::Matrix3d expM = Eigen::Matrix3d::Identity();

  double a = std::sqrt(0.5 * (M.transpose() * M).trace());

  if (std::abs(a) < std::sqrt(std::numeric_limits<double>::epsilon()))
    return expM;

  expM += (std::sin(a) / a) * M + (1.0 - std::cos(a)) * M * M / (a * a);

  return expM;
}

Eigen::Matrix3d projectSO3C(const Eigen::Matrix3d &M)
{
  /* This function projects a 3-by-3 symmetric matrix (in M(3)) into SO(3). */

  Eigen::Matrix3d Msq = M.transpose() * M;
  Eigen::SelfAdjointEigenSolver<Eigen::Matrix3d> eigenSolver(Msq, Eigen::ComputeEigenvectors);
  Eigen::Matrix3d u = eigenSolver.eigenvectors().rowwise().reverse();

  Eigen::Matrix3d dMat = eigenSolver.eigenvalues().array().rsqrt().matrix().reverse().asDiagonal();
  if (M.determinant() < 0)
    dMat(2, 2) *= -1.0;

  return M * u * dMat * u.transpose();
}

Eigen::Matrix3d meanSO3C(const Eigen::MatrixXd &Rs)
{
  /* Compute the projected mean for a sample of n rotations, Rs.
   This function expects Rs to be a n-by-9 matrix where each row
   represents an observation in SO(3) */

  Eigen::VectorXd Rbarels = Rs.colwise().mean();
  Eigen::Map<Eigen::Matrix3d> Rbar(Rbarels.data());

  return projectSO3C(Rbar);
}

Eigen::Quaterniond expq(const Eigen::Quaterniond& q)
{
  double a = q.vec().norm();
  double exp_w = std::exp(q.w());

  if (a == double(0))
    return Eigen::Quaterniond(exp_w, double(0), double(0), double(0));

  Eigen::Quaterniond res;
  res.w() = exp_w * double(std::cos(a));
  res.vec() = exp_w * double(std::sin(a) / a) * q.vec();

  return res;
}

Eigen::Quaterniond logq(const Eigen::Quaterniond& q)
{
  double exp_w = q.norm();
  double w = std::log(exp_w);
  double a = std::acos(q.w() / exp_w);

  if (a == double(0))
  {
    return Eigen::Quaterniond(w, double(0), double(0), double(0));
  }

  Eigen::Quaterniond res;
  res.w() = w;
  res.vec() = q.vec() / exp_w / (sin(a) / a);

  return res;
}

Eigen::MatrixXd GetRotationsFromQuaternions(const std::vector<Eigen::VectorXd> &quaternionSample)
{
  unsigned int nQuaternions = quaternionSample.size();
  Eigen::MatrixXd rotationSample(nQuaternions, 9);
  Eigen::Quaterniond qValue;
  Eigen::Vector4d workValue;

  for (unsigned int i = 0;i < nQuaternions;++i)
  {
    workValue = quaternionSample[i];
    qValue = Eigen::Quaterniond(workValue(0), workValue(1), workValue(2), workValue(3));
    rotationSample.row(i) = Eigen::Map<Eigen::VectorXd>(qValue.toRotationMatrix().data(), 9);
  }

  return rotationSample;
}

Eigen::Vector4d geometric_mean(const std::vector<Eigen::VectorXd> &quaternionSample,
                               unsigned int maxIterations,
                               double maxEpsilon)
{
  unsigned int nSamples = quaternionSample.size();
  Eigen::Quaterniond meanValue(1.0, 0.0, 0.0, 0.0), invMeanValue, logMeanValue, tmpQValue;
  double epsilon = 1.0;
  unsigned int iterations = 0;
  Eigen::Vector4d resValue;

  while (epsilon > maxEpsilon && iterations < maxIterations)
  {
    invMeanValue = meanValue.inverse();

    logMeanValue = Eigen::Quaterniond(0.0, 0.0, 0.0, 0.0);
    for (unsigned int i = 0;i < nSamples;++i)
    {
      resValue = quaternionSample[i];
      tmpQValue = Eigen::Quaterniond(resValue(0), resValue(1), resValue(2), resValue(3));
      tmpQValue = invMeanValue * tmpQValue;
      logMeanValue.coeffs() *= (double)(i) / (i + 1.0);
      logMeanValue.coeffs() += logq(tmpQValue).coeffs() / (i + 1.0);
    }

    // logMeanValue.coeffs() /= nSamples;
    meanValue = meanValue * expq(logMeanValue);
    epsilon = logMeanValue.norm();
    ++iterations;
  }

  resValue(0) = meanValue.w();
  resValue(1) = meanValue.x();
  resValue(2) = meanValue.y();
  resValue(3) = meanValue.z();
  return resValue;
}

Eigen::VectorXd gmean(const std::vector<Eigen::VectorXd> &quaternionSample,
                      unsigned int maxIterations,
                      double maxEpsilon)
{
  Eigen::MatrixXd rotationSample = GetRotationsFromQuaternions(quaternionSample);
  unsigned int numPoints = rotationSample.rows();

  Eigen::Vector4d euclideanMean;
  for (unsigned int i = 0;i < 4;++i)
  {
    euclideanMean(i) = 0.0;
    for (unsigned int j = 0;j < numPoints;++j)
      euclideanMean(i) += quaternionSample[j](i);
    euclideanMean(i) /= (double)numPoints;
  }

  unsigned int iterations = 0;
  Eigen::Matrix3d Rsi;
  Eigen::Matrix3d r;
  Eigen::Matrix3d S = meanSO3C(rotationSample);
  double epsilon = 1.0;

  while (epsilon > maxEpsilon && iterations < maxIterations)
  {
    r = Eigen::Matrix3d::Zero();

    for (unsigned int i = 0;i < numPoints;++i)
    {
      for (unsigned int j = 0;j < 9;++j)
        Rsi(j) = rotationSample(i, j);

      r += logSO3C(S.transpose() * Rsi);
    }

    r = r / numPoints;
    S = S * expskewC(r);
    epsilon = r.norm();
    ++iterations;
  }

  Eigen::Quaterniond meanQValue(S);

  double dotProduct = meanQValue.w() * euclideanMean(0) +
    meanQValue.x() * euclideanMean(1) +
    meanQValue.y() * euclideanMean(2) +
    meanQValue.z() * euclideanMean(3);

  if (dotProduct < 0)
    meanQValue.coeffs() *= -1.0;

  Eigen::Vector4d outValue;
  outValue(0) = meanQValue.w();
  outValue(1) = meanQValue.x();
  outValue(2) = meanQValue.y();
  outValue(3) = meanQValue.z();
  return outValue;
}

double gvariance(const std::vector<Eigen::VectorXd> &quaternionSample,
                 const Eigen::VectorXd &quaternionMean)
{
  unsigned int numPoints = quaternionSample.size();
  Eigen::Quaterniond workQuaternion, meanQuaternion(quaternionMean(0), quaternionMean(1), quaternionMean(2), quaternionMean(3));

  double varValue = 0.0;
  for (unsigned int i = 0;i < numPoints;++i)
  {
    workQuaternion = Eigen::Quaterniond(quaternionSample[i](0), quaternionSample[i](1), quaternionSample[i](2), quaternionSample[i](3));
    double distValue = workQuaternion.angularDistance(meanQuaternion);
    varValue += distValue * distValue;
  }

  return varValue;
}

Eigen::VectorXd gmedian(const std::vector<Eigen::VectorXd> &quaternionSample,
                        const unsigned int maxIterations,
                        const double maxEpsilon)
{
  Eigen::MatrixXd rotationSample = GetRotationsFromQuaternions(quaternionSample);
  unsigned int numPoints = rotationSample.rows();
  unsigned int iterations = 0;
  Eigen::Matrix3d S = meanSO3C(rotationSample);
  Eigen::Matrix3d Snew;
  Eigen::Matrix3d delta;
  Eigen::Matrix3d Rsi;
  Eigen::Matrix3d vi;
  double epsilon = 1.0;

  while (epsilon > maxEpsilon && iterations < maxIterations)
  {
    delta = Eigen::Matrix3d::Zero();
    double denom = 0;

    for (unsigned int i = 0;i < numPoints;++i)
    {
      for (unsigned int j = 0;j < 9;++j)
        Rsi(j) = rotationSample(i, j);

      vi = logSO3C(Rsi * S.transpose());
      double vin = std::max(vi.operatorNorm(), 1.0e-5);

      vin = 1.0 / vin;
      delta += vi * vin;
      denom += vin;
    }

    delta /= denom;
    Snew = expskewC(delta) * S;

    ++iterations;
    epsilon = (Snew - S).operatorNorm();
    S = Snew;
  }

  Eigen::Quaterniond medianQValue(S);
  Eigen::Vector4d outValue;
  outValue(0) = medianQValue.w();
  outValue(1) = medianQValue.x();
  outValue(2) = medianQValue.y();
  outValue(3) = medianQValue.z();
  return outValue;
}
