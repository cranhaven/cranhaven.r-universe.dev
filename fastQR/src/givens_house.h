/* File: givens_house.h */

#ifndef givens_house_H
#define givens_house_H

// :::::::::::::::::::::::::::::::::::::::::::::::
// Givens rotation
Eigen::Vector2d givens (const double& a, const double& b);

// :::::::::::::::::::::::::::::::::::::::::::::::
// Householder reflections
Eigen::VectorXd householder (const Eigen::VectorXd& x);
Eigen::VectorXd houseL (const Eigen::VectorXd& x);

#endif

