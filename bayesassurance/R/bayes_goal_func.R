#' Decision Making using Rate of Correct Classification
#'
#' Determines the rate of correctly classifying the linear hypothesis
#' as true or false, where the hypothesis test is specified as
#' \deqn{H0: u'\beta = c0} \deqn{vs.} \deqn{H1: u'\beta = c1}. See
#' vignette for more details.
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline aes theme element_text 
#' @importFrom ggplot2 xlab ylab ggtitle scale_x_continuous scale_y_continuous
#' @importFrom rlang .data
#' @importFrom stats pnorm
#' @param n sample size (vector or scalar).
#' @param Xn design matrix that characterizing the data. This is
#' specifically given by the normal linear regression model 
#' \deqn{yn = Xn\beta + \epsilon,} \deqn{\epsilon ~ N(0, \sigma^2 I_n),} 
#' where \eqn{I_n} is an \eqn{n} by \eqn{n} identity matrix. 
#' When set to `NULL`, an appropriate `Xn` is automatically generated
#' `bayesassurance::gen_Xn()`. Note that setting `Xn = NULL` 
#' also enables user to pass in a vector of sample sizes to undergo 
#' evaluation as the function will automatically adjust `Xn` accordingly 
#' based on the sample size.
#' @param K The amount of utility associated with \eqn{H0} being correctly
#' accepted.The null hypothesis is not rejected if the posterior probability
#' of \eqn{H0} is at least \eqn{1/(1+K)}.
#' @param pi constant corresponding to the prior on parameter \eqn{\beta}
#' such that \eqn{P(u'\beta_0) = 1 - P(u'\beta_1) = \pi}.
#' @param sigsq variance constant of the linear regression model
#' @param u fixed scalar or vector of the same dimension as \eqn{\beta_0} and
#' \eqn{\beta_1}
#' @param beta_0 fixed scalar or vector that null hypothesis is set to
#' @param beta_1 fixed scalar or vector that alternative hypothesis is set to
#' @return a list of objects corresponding to the rate of classifications
#' \itemize{
#'      \item{rc_table:} table of sample size and corresponding correct 
#'      classification rates
#'      \item{rc_plot:} plot of correct classification rates for varying 
#'      sample sizes
#' }
#' @examples ## Example
#' n <- seq(100, 1200, 10)
#' out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
#' sigsq = 1, u = 1, beta_0 = 0.5, beta_1 = 0.6)
#' out$rc_plot
#' @export
#'
bayes_goal_func <- function(n, Xn = NULL, K, pi, sigsq, u, beta_0, beta_1){

  # initial checks
  is.scalar <- function(x) {is.atomic(x) && length(x) == 1L}


  if(dim(t(beta_0))[1] != dim(t(beta_1))[1] | dim(t(beta_0))[2] != 
     dim(t(beta_1))[2]){
    stop("beta_0 and beta_1 must have equal dimensions.")
  }

  if(is.null(Xn) == FALSE){
    if(dim(t(beta_0))[2] != dim(t(Xn))[1]){
      stop("Dimension mismatch. Check design matrix.")
    }

    if((dim(t(beta_0))[2] != dim(as.matrix(u))[1])){
      stop("Dimension mismatch. Make sure beta_0, beta_1, and u
           have equal dimensions.")
    }
  }

  # will rely on this embedded function to determine the
  # rate of correct classification for each n
  rate.correct.class <- function(n = n){

    # Case 1: beta_0 and beta_1 are scalar entries
    if(is.scalar(beta_0) & is.scalar(beta_1)){

      # generates a design matrix if Xn = NULL
      if(is.null(Xn)){
        Xn <- bayesassurance::gen_Xn(n = n)
      }

      # check if u'beta is estimable by using the principle theorem that
      # a solution must exist for z in the linear system X'Xz = u

         ## checks the above condition by using properties of ranks to see 
         ## if solution(s) exist, i.e. X'Xz = u is inconsistent 
         ## (no solution exists) iff rank(X'X) < rank(X'X|u)

      if((qr(t(Xn) %*% Xn)$rank == qr(as.matrix(cbind(t(Xn) %*% Xn, u)))$rank) &
         (qr(t(Xn) %*% Xn)$rank <= dim(t(Xn))[2])){
          ## applies Singular Value Decomposition to solve system u = X'z, 
          ## where X is n x p
          Xn_svd <- svd(t(Xn))
          Xn_diag <- 1/Xn_svd$d
          z <- Xn_svd$v %*% Xn_diag %*% t(Xn_svd$u) %*% u
      }else if(qr(t(Xn) %*% Xn)$rank < 
               qr(as.matrix(cbind(t(Xn) %*% Xn, u)))$rank){
        stop("u'beta is not estimable, consider an alternative u")
      }

      # critical difference
      delta <- (u * beta_1) - (u * beta_0)

      # determines the rate of correct classification r_star
      omega_0 <- stats::pnorm((((sqrt(sigsq)*sqrt(t(z) %*% z))/delta) * 
                         log((K * pi)/(1 - pi))) +
                         (delta/(2*sqrt(sigsq)*sqrt(t(z) %*% z))))

      omega_1 <- 1 - stats::pnorm((((sqrt(sigsq)*sqrt(t(z) %*% z))/delta) * 
                         log((K * pi)/(1 - pi))) -
                         (delta/(2*sqrt(sigsq)*sqrt(t(z) %*% z))))

      r_star <- K * pi * omega_0 + (1 - pi) * omega_1
    }else{ # Case 2: beta_0 and beta_1 are vector entries

      if(is.null(Xn)){
        p <- dim(t(beta_0))[2]
        Xn <- bayesassurance::gen_Xn(n = rep(n, p))
      }

      # check if u'beta is estimable by using the principle theorem that
      # a solution must exist for z in X'Xz = u

      # check this by using ranks to see if solutions exist.

      if((qr(t(Xn) %*% Xn)$rank == qr(as.matrix(cbind(t(Xn) %*% Xn, u)))$rank) &
         (qr(t(Xn) %*% Xn)$rank <= dim(t(Xn))[2])){

        ## applies Singular Value Decomposition to solve u = X'z, 
        ## where X is n x p
        Xn_svd <- svd(t(Xn))
        Xn_diag <- diag(1/Xn_svd$d)
        Xn_diag[which(Xn_diag > 1e10)] <- 0
        z <- Xn_svd$v %*% Xn_diag %*% t(Xn_svd$u) %*% u

      }else if(qr(t(Xn) %*% Xn)$rank < 
               qr(as.matrix(cbind(t(Xn) %*% Xn, u)))$rank){
        stop("u'beta is not estimable, consider an alternative u")
      }

      # critical difference (note this should be a scalar)
      delta <- (t(u) %*% beta_1) - (t(u) %*% beta_0)

      # determines the rate of correct classification r_star
      omega_0 <- stats::pnorm((((sqrt(sigsq)*sqrt(t(z) %*% z))/delta) * 
                         log((K * pi)/(1 - pi))) +
                         (delta/(2*sqrt(sigsq)*sqrt(t(z) %*% z))))

      omega_1 <- 1 - stats::pnorm((((sqrt(sigsq)*sqrt(t(z) %*% z))/delta) * 
                         log((K * pi)/(1 - pi))) -
                         (delta/(2*sqrt(sigsq)*sqrt(t(z) %*% z))))

      r_star <- K * pi * omega_0 + (1 - pi) * omega_1

    }

    return(r_star)
  }

  # determines the rate of correct classification for each n
  rate_of_class <- sapply(n, function(i) rate.correct.class(n=i))


  # compiles results into data frame
  assur_tab <- as.data.frame(cbind(n, rate_of_class))
  colnames(assur_tab) <- c("n", "Rate of Correct Classification")
  assur_tab <- structure(assur_tab, class = "data.frame")

  # ggplot
  if(length(n) > 1){
    assur_plot <- ggplot2::ggplot(assur_tab, alpha = 0.5, aes(x = .data$n,
      y = .data$`Rate of Correct Classification`)) +
      geom_line(lwd = 1.2) +
      ggplot2::ggtitle("Goal Function Curve") +
      ggplot2::xlab("Sample Size n") + 
      ggplot2::ylab("Rate of Correct Classification") + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    assur_plot <- structure(assur_plot, class = "ggplot")
  }

  if(length(n) > 1){
    return(list(rc_table = assur_tab, rc_plot = assur_plot))
  }else{
    return(list(rc_val = paste0("Rate of Correct Classification: ", 
    round(assur_tab$`Rate of Correct Classification`, 3))))
  }


}
