#include "BigDataStatMeth.hpp"


/**
 * @brief Weighted matrix–vector products and weighted cross-products.
 *
 * @details Computes one of the following (case-insensitive @p op):
 *  - "xtwx": \f$ X^\top \, \mathrm{diag}(w) \, X \f$  (row-weights, length(w) = nrow(X))
 *  - "xwxt": \f$ X \, \mathrm{diag}(w) \, X^\top \f$  (column-weights, length(w) = ncol(X))
 *  - "xw"  : \f$ X \, \mathrm{diag}(w) \f$            (column scaling, length(w) = ncol(X))
 *  - "wx"  : \f$ \mathrm{diag}(w) \, X \f$            (row scaling,    length(w) = nrow(X))
 *
 * Inputs can be base R numeric matrices; both
 * are realized to dense form. The weight \p w may be given as a numeric
 * vector or a 1-column/1-row matrix; it is interpreted as the diagonal of
 * \f$\mathrm{diag}(w)\f$ with the required length depending on \p op.
 *
 * @param X Numeric matrix (n x p).
 * @param w Numeric weight vector (length n or p) or a 1D matrix coerced to a vector.
 * @param op Operation selector: one of {"XtwX","xtwx","XwXt","xwxt","Xw","xw","wX","wx"} (case-insensitive).
 *
 * @return Dense numeric matrix with dimensions according to \p op:
 *         - "xtwx": p x p
 *         - "xwxt": n x n
 *         - "xw"  : n x p
 *         - "wx"  : n x p
 *
 * @throws std::runtime_error on unsupported \p op or type/shape conversion errors.
 *
 * @pre For "xtwx" and "wX"/"wx": \c length(w) == nrow(X).
 * @pre For "xwxt" and "Xw"/"xw": \c length(w) == ncol(X).
 *
 * @since 0.99.0
 */



//' Weighted matrix–vector products and cross-products
//'
//' @description
//' Compute weighted operations using a diagonal weight from \code{w}:
//' \itemize{
//' \item \code{"xtwx"}: \eqn{X' diag(w) X}  (row weights; \code{length(w) = nrow(X)})
//' \item \code{"xwxt"}: \eqn{X diag(w) X'}  (column weights; \code{length(w) = ncol(X)})
//' \item \code{"xw"}  : \eqn{X diag(w)}     (column scaling; \code{length(w) = ncol(X)})
//' \item \code{"wx"}  : \eqn{diag(w) X}     (row scaling;    \code{length(w) = nrow(X)})
//' }
//' Inputs may be base numeric matrices .
//'
//' @param X Numeric matrix (n x p).
//' @param w Numeric weight vector (length \code{n} or \code{p}), or a 1D matrix coerced to a vector.
//' @param op Character string (case-insensitive): one of
//'   \code{"XtwX"/"xtwx"}, \code{"XwXt"/"xwxt"}, \code{"Xw"/"xw"}, \code{"wX"/"wx"}.
//'
//' @return Numeric matrix with dimensions depending on \code{op}:
//' \code{p x p} for \code{"xtwx"}, \code{n x n} for \code{"xwxt"}, and \code{n x p} for \code{"xw"}/\code{"wx"}.
//'
//' @details
//' \code{w} is interpreted as the diagonal of a weight matrix; its required length depends on the operation:
//' rows for \code{"xtwx"} and \code{"wx"}, columns for \code{"xwxt"} and \code{"xw"}.
//'
//' @examples
//' set.seed(1)
//' n <- 10; p <- 5
//' X <- matrix(rnorm(n * p), n, p)
//' u <- runif(n); w <- u * (1 - u)
//' bd_wproduct(X, w, "xtwx")  # p x p
//' bd_wproduct(X, w, "wx")    # n x p (row scaling)
//'
//' v <- runif(p)
//' bd_wproduct(X, v, "xw")    # n x p (col scaling)
//' bd_wproduct(X, v, "xwxt")  # n x n
//'
//' @export
 // [[Rcpp::export]]
 Eigen::MatrixXd bd_wproduct(Rcpp::RObject X, Rcpp::RObject w, std::string op)
 {
     
     Eigen::MatrixXd A;
     Eigen::MatrixXd W;
     
     try{  
         A = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(X);
         W = Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(w);
     }
     catch(std::exception &ex) { }
     
     if(op == "XwXt" || op == "xwxt") {
         return(BigDataStatMeth::xwxt(A,W)) ;
     }else if (op == "XtwX" || op == "xtwx") {
         return(BigDataStatMeth::xtwx(A,W));
     }else if (op == "Xw" || op == "xw") {
         return(BigDataStatMeth::Xw(A,W));
     }else if (op == "wX" || op == "wx" ) {
         return(BigDataStatMeth::wX(A,W));
     } else
     {
         throw("Invalid option, valid options : 'xtwx' and 'xwxt' for weighted cross product or 'Xw' and 'wX' for weighted product");
     }
 }
