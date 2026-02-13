#include <cmath>
#include <Rmath.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec vhpd(const arma::vec& x, const double& alpha) {
    const int n = x.n_elem;
    arma::vec x_ = x; // copy x to be rearranged
    double q1 = 0.5 * alpha * static_cast<double>(n);
    double q2 = (1.0 - 0.5 * alpha) * static_cast<double>(n);
    int nq1 = static_cast<int>(q1 + 0.5);
    int nq2 = static_cast<int>(q2 + 0.5);
    int nq = nq2 - nq1;
    std::sort(x_.begin(), x_.end());
    double alow = 0.0;
    double aupp = 0.0;
    double whpd = 0.0;
    for (int j = 0; j < n - nq; ++j) {
        double pdiff1 = x_(j);
        double pdiff2 = x_(j + nq);
        double wb = pdiff2 - pdiff1;
        if (j == 0) {
            whpd = wb;
            aupp = pdiff2;
            alow = pdiff1;
        } else {
            if (whpd > wb) {
                whpd = wb;
                aupp = pdiff2;
                alow = pdiff1;
            }
        }
    }
    arma::vec out = { alow, aupp };
    return out;
}

// [[Rcpp::export]]
arma::mat mhpd(const arma::mat& x, const double& alpha) {
    const int n = x.n_cols;
    const int nr = x.n_rows;
    double q1 = 0.5 * alpha * static_cast<double>(n);
    double q2 = (1.0 - 0.5 * alpha) * static_cast<double>(n);
    int nq1 = static_cast<int>(q1 + 0.5);
    int nq2 = static_cast<int>(q2 + 0.5);
    int nq = nq2 - nq1;
    arma::mat out(nr, 2, arma::fill::zeros);
    for (int irow = 0; irow < nr; ++irow) {
        arma::rowvec x_ = x.row(irow); // copy x to be rearranged
        std::sort(x_.begin(), x_.end());
        double alow = 0.0;
        double aupp = 0.0;
        double whpd = 0.0;
        for (int j = 0; j < n - nq; ++j) {
            double pdiff1 = x_(j);
            double pdiff2 = x_(j + nq);
            double wb = pdiff2 - pdiff1;
            if (j == 0) {
                whpd = wb;
                aupp = pdiff2;
                alow = pdiff1;
            } else {
                if (whpd > wb) {
                    whpd = wb;
                    aupp = pdiff2;
                    alow = pdiff1;
                }
            }
        }
        out(irow, 0) = alow;
        out(irow, 1) = aupp;
    }
    return out;
}

