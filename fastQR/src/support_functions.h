/* File: support_functions.h */

#ifndef support_functions_H
#define support_functions_H

Eigen::ArrayXi seq (const int& n);

Eigen::ArrayXi setdiff (const Eigen::VectorXi& x1,
                        const Eigen::VectorXi& x2);

Eigen::VectorXd repelem (const double& x,
                         const int& n);

#endif
