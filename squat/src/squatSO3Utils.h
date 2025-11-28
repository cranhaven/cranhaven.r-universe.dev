#ifndef SQUATSO3UTILS_H
#define SQUATSO3UTILS_H

#include <RcppEigen.h>

// [[Rcpp::interfaces(r, cpp)]]

Eigen::Matrix3d logSO3C(const Eigen::Matrix3d &R);
Eigen::Matrix3d expskewC(const Eigen::Matrix3d &M);
Eigen::Matrix3d projectSO3C(const Eigen::Matrix3d &M);
Eigen::Matrix3d meanSO3C(const Eigen::MatrixXd &Rs);

Eigen::Quaterniond expq(const Eigen::Quaterniond& q);
Eigen::Quaterniond logq(const Eigen::Quaterniond& q);

Eigen::MatrixXd GetRotationsFromQuaternions(
    const std::vector<Eigen::VectorXd> &quaternionSample
);

Eigen::Vector4d geometric_mean(
    const std::vector<Eigen::VectorXd> &quaternionSample,
    unsigned int maxIterations = 2000,
    double maxEpsilon = 1.0e-5
);

// [[Rcpp::export]]
Eigen::VectorXd gmean(
    const std::vector<Eigen::VectorXd> &quaternionSample,
    unsigned int maxIterations = 2000,
    double maxEpsilon = 1.0e-5
);

double gvariance(
    const std::vector<Eigen::VectorXd> &quaternionSample,
    const Eigen::VectorXd &quaternionMean
);

// [[Rcpp::export]]
Eigen::VectorXd gmedian(
    const std::vector<Eigen::VectorXd> &quaternionSample,
    unsigned int maxIterations = 2000,
    double maxEpsilon = 1.0e-5
);

#endif /* SQUATSO3UTILS_H */
