#include "BigDataStatMeth.hpp"

/**
 * @brief Matrix–scalar weighted product (dense numeric).
 *
 * @details Multiplies a numeric matrix @p A by a scalar weight @p w,
 * returning @f$ w \cdot A @f$. The input is coerced to an
 * `Eigen::Map<Eigen::MatrixXd>`; therefore, it must be a base R
 * numeric matrix (or an object convertible to one). If you work with
 * `DelayedArray`, realize it to a base matrix before calling.
 *
 * @param A R object representing a numeric matrix (convertible to dense).
 * @param w Scalar weight.
 *
 * @return An R numeric matrix with the same dimensions as @p A.
 *
 * @throws Rcpp::exception on type/shape conversion errors.
 *
 * @since 0.99.0
 */

//' Matrix–scalar weighted product
//'
//' @description
//' Multiplies a numeric matrix \code{A} by a scalar weight \code{w},
//' returning \eqn{w * A}. The input must be a base R numeric matrix (or
//' convertible to one). 
//'
//' @param A Numeric matrix (or object convertible to a dense numeric matrix).
//' @param w Numeric scalar weight.
//'
//' @return A numeric matrix with the same dimensions as \code{A}.
//'
//' @examples
//' set.seed(1234)
//' n <- 5; p <- 3
//' X <- matrix(rnorm(n * p), n, p)
//' w <- 0.75
//' bdScalarwproduct(X, w)
//'
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericMatrix bdScalarwproduct(Rcpp::RObject A, double w)
 {
     
     try {
         
         Eigen::Map<Eigen::MatrixXd> mA = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(A);
         
         return( Rcpp::wrap(w*mA) );
         
     }  catch(std::exception &ex) {
         Rcpp::Rcerr << "c++ exception bdblockMult: " << ex.what();
         return(R_NilValue);
     } catch (...) {
         Rcpp::Rcerr << "c++ exception bdblockMult (unknown reason)";
         return(R_NilValue);    
     }
     
 }
