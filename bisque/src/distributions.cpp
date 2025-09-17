#include "distributions.h"


using namespace arma;

using Eigen::MatrixXd;
using Eigen::Map;
using Eigen::LLT;
using Eigen::Lower;

// sample x0 | x1 when (x0,x1) ~ N(0, Sigma), where Sigma is partitioned
// into S00, S01, and S11 components
arma::vec mcstat2::mvnorm_cond(MatrixXd &x1,
                               arma::mat &S00_arma,
                               arma::mat &S01_arma,
                               arma::mat &S11_arma) {
    
    // extract dimensions
    int n0 = S00_arma.n_cols;
    int n1 = S11_arma.n_cols;
    
    // read covariance matrices into eigen
    Map<MatrixXd> S00(S00_arma.memptr(), n0, n0);
    Map<MatrixXd> S01(S01_arma.memptr(), n0, n1);
    Map<MatrixXd> S11(S11_arma.memptr(), n1, n1);
    
    // factor covariance matrix; store lower cholesky
    LLT<MatrixXd, Lower> llt(S11);
    
    // compute components of conditional normal distribution
    MatrixXd Delta = llt.matrixL().solve(S01.transpose());
    MatrixXd MuCond = Delta.transpose() * llt.matrixL().solve(x1);
    MatrixXd SigmaCond = S00 - Delta.transpose() * Delta;
    LLT<MatrixXd, Lower> lltCond(SigmaCond);
    
    //Rcpp::Rcout << S11 << endl;
    
    // generate independent normal variates; map variates into eigen
    arma::vec z_arma = arma::randn(n0);
    Map<MatrixXd> z(z_arma.memptr(), n0, 1);
    MatrixXd x = MuCond + lltCond.matrixL() * z;
    
    // convert sample to arma::vec and return
    vec x_arma = vec(x.data(), n0);
    return x_arma;
}
