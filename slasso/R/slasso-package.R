#' @useDynLib slasso
#' @importFrom Rcpp sourceCpp 
NULL
#> NULL
## usethis namespace: start
#' @importFrom Rcpp sourceCpp 
#' @useDynLib slasso, .registration = TRUE
## usethis namespace: end
NULL
#' @importFrom inline cxxfunction



#' @title Smooth LASSO Estimator for the Function-on-Function Linear Regression Model
#' @details
#'
#'\tabular{ll}{
#'Package: \tab slasso\cr
#'Type: \tab Package\cr
#'Version: \tab `r packageVersion("slasso")` \cr
#'Date: \tab  `r Sys.Date()` \cr
#'License: \tab `r packageDescription("slasso", fields="License")`\cr
#'}
#'
#' @author Fabio Centofanti, Matteo Fontana, Antonio Lepore, Simone Vantini
#' @references
#' Centofanti, F., Fontana, M., Lepore, A., & Vantini, S. (2020).
#' Smooth LASSO Estimator for the Function-on-Function Linear Regression Model.
#' \emph{arXiv preprint arXiv:2007.00529}.
#' @seealso \code{\link{slasso.fr}},  \code{\link{slasso.fr_cv}}
#' @examples
#' \donttest{
#' library(slasso)
#' data<-simulate_data("Scenario II",n_obs=150)
#' X_fd=data$X_fd
#' Y_fd=data$Y_fd
#' domain=c(0,1)
#' n_basis_s<-30
#' n_basis_t<-30
#' breaks_s<-seq(0,1,length.out = (n_basis_s-2))
#' breaks_t<-seq(0,1,length.out = (n_basis_t-2))
#' basis_s <- fda::create.bspline.basis(domain,breaks=breaks_s)
#' basis_t <- fda::create.bspline.basis(domain,breaks=breaks_t)
#' 
#' mod_slasso_cv<-slasso.fr_cv(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,
#' lambda_L_vec = 10^seq(0,1,by=1),lambda_s_vec = 10^-9,lambda_t_vec = 10^-7,
#' B0=NULL,max_iterations=10,K=2,invisible=1,ncores=1)
#' mod_slasso<-slasso.fr(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,
#' lambda_L = 10^0.7,lambda_s =10^-5,lambda_t = 10^-6,B0 =NULL,invisible=1,max_iterations=10)
#' 
#' plot(mod_slasso_cv)
#' plot(mod_slasso)
#' }
"_PACKAGE"



