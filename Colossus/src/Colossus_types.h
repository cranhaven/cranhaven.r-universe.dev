#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <chrono>
#include <random>
#include <ctime>
#include <Eigen/Core>
#include "R_Interface.h"


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(openmp)]]
using namespace std;
using namespace Rcpp;
using namespace Eigen;
using namespace std::chrono;

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::VectorXd;
using Rcpp::as;

typedef Eigen::Matrix< double, Eigen::Dynamic, Eigen::Dynamic, ColMajor > ColXd;
typedef Eigen::Matrix< double, Eigen::Dynamic, Eigen::Dynamic, RowMajor > RowXd;
