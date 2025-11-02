#' Bayesian Simulation with Composite Sampling
#'
#' Approximates the Bayesian assurance of a one-sided hypothesis test
#' through Monte Carlo sampling with the added assumption that
#' the variance is unknown.
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline aes theme element_text 
#' @importFrom ggplot2 xlab ylab ggtitle scale_x_continuous scale_y_continuous
#' @importFrom rlang .data
#' @importFrom MASS mvrnorm
#' @importFrom stats qnorm rgamma
#' @importFrom pbapply pbsapply
#' @import mathjaxr
#' @param n sample size (either vector or scalar)
#' @param p column dimension of design matrix `Xn`. If `Xn = NULL`,
#' `p` must be specified to denote the column dimension of the default 
#' design matrix generated
#' by the function.
#' @param u a scalar or vector to evaluate \deqn{u'\beta > C,}
#' where \eqn{\beta} is an unknown parameter that is to be estimated.
#' @param C constant value to be compared to when evaluating \eqn{u'\beta > C}
#' @param R number of iterations we want to pass through to check for 
#' satisfaction of the analysis stage objective. The proportion of those
#' iterations meeting the analysis objective corresponds to the approximated 
#' Bayesian assurance.
#' @param Xn design matrix that characterizes where the data is to be generated 
#' from. This is specifically given by the normal linear regression model 
#' \deqn{yn = Xn\beta + \epsilon,} \deqn{\epsilon ~ N(0, \sigma^2 Vn),} 
#' where \eqn{\sigma^2} is unknown in this setting. Note that
#' `Xn` must have column dimension `p`.
#' @param Vn an `n` by `n` correlation matrix for the marginal distribution 
#' of the sample data `yn`. Takes on an identity matrix when set to `NULL`.
#' @param Vbeta_d correlation matrix that helps describe the prior information 
#' on \eqn{\beta} in the design stage
#' @param Vbeta_a_inv inverse-correlation matrix that helps describe the prior 
#' information on \eqn{\beta} in the analysis stage
#' @param mu_beta_d design stage mean
#' @param mu_beta_a analysis stage mean
#' @param a_sig_d,b_sig_d shape and scale parameters of the inverse gamma 
#' distribution where variance \eqn{\sigma^2} is sampled from in the design 
#' stage
#' @param a_sig_a,b_sig_a shape and scale parameters of the inverse 
#' gamma distribution where variance \eqn{\sigma^2}
#' is sampled from in the analysis stage
#' @param alt specifies alternative test case, where `alt = "greater"` 
#' tests if \eqn{u'\beta > C}, `alt = "less"` tests if \eqn{u'\beta < C}, 
#' and `alt = "two.sided"` performs a two-sided test. By default,
#' `alt = "greater"`.
#' @param alpha significance level
#' @param mc_iter number of MC samples evaluated under the analysis objective
#' @return a list of objects corresponding to the assurance approximations
#' \itemize{
#'      \item{assurance_table:} table of sample size and corresponding assurance
#'      values
#'      \item{assur_plot:} plot of assurance values
#'      \item{mc_samples:} number of Monte Carlo samples that were generated
#'      and evaluated
#' }
#' @examples
#' 
#' ## O'Hagan and Stevens (2001) include a series of examples with 
#' ## pre-specified parameters that we will be using to replicate their 
#' ## results through our Bayesian assurance simulation.
#' ## The inputs are as follows:
#' \donttest{
#' n <- 285
#' p <- 4 ## includes two parameter measures (cost and efficacy) for each of 
#'        ## the two treatments, for a total of p = 4 parameters. 
#' K <- 20000
#' C <- 0
#' u <- as.matrix(c(-K, 1, K, -1))
#'
#' ## Set up correlation matrices
#' Vbeta_a_inv <- matrix(rep(0, p^2), nrow = p, ncol = p)
#'
#' sigsq <- 4.04^2
#' tau1 <- tau2 <- 8700
#' sig <- sqrt(sigsq)
#' Vn <- matrix(0, nrow = n*p, ncol = n*p)
#' Vn[1:n, 1:n] <- diag(n)
#' Vn[(2*n - (n-1)):(2*n), (2*n - (n-1)):(2*n)] <- (tau1 / sig)^2 * diag(n)
#' Vn[(3*n - (n-1)):(3*n), (3*n - (n-1)):(3*n)] <- diag(n)
#' Vn[(4*n - (n-1)):(4*n), (4*n - (n-1)):(4*n)] <- (tau2 / sig)^2 * diag(n)
#'
#' Vbeta_d <- (1 / sigsq) * 
#' matrix(c(4, 0, 3, 0, 0, 10^7, 0, 0, 3, 0, 4, 0, 0, 0, 0, 10^7),
#' nrow = 4, ncol = 4)
#' mu_beta_d <- as.matrix(c(5, 6000, 6.5, 7200))
#' mu_beta_a <- as.matrix(rep(0, p))
#' alpha <- 0.05
#' epsilon <- 10e-7
#' a_sig_d <- (sigsq / epsilon) + 2
#' b_sig_d <- sigsq * (a_sig_d - 1)
#' a_sig_a <- -p / 2
#' b_sig_a <- 0
#'
#' bayesassurance::bayes_sim_unknownvar(n = n, p = 4, 
#' u = as.matrix(c(-K, 1, K, -1)), C = 0, R = 40,
#' Xn = NULL, Vn = Vn, Vbeta_d = Vbeta_d, 
#' Vbeta_a_inv = Vbeta_a_inv, mu_beta_d = mu_beta_d,
#' mu_beta_a = mu_beta_a, a_sig_a = a_sig_a, b_sig_a = b_sig_a, 
#' a_sig_d = a_sig_d, b_sig_d = b_sig_d, alt = "two.sided", alpha = 0.05, 
#' mc_iter = 500)
#'}
#' @seealso \code{\link{bayes_sim}} for the Bayesian assurance function
#' for known variance.
#' @export


bayes_sim_unknownvar <- function(n, p = NULL, u, C, R, Xn = NULL, Vn, Vbeta_d, 
                                 Vbeta_a_inv, mu_beta_d, mu_beta_a, a_sig_a, 
                                 b_sig_a, a_sig_d, b_sig_d, alt = "greater", 
                                 alpha, mc_iter){

  # will rely on this embedded function to generate data and
  # assess satisfaction of analysis objective for n
  MC_sample <- function(n = n){
    mu_beta_a_t <- t(mu_beta_a)

    if(is.null(Xn)){
      if(is.null(p)){
        stop("Please specify column dimension p since Xn = NULL.")
      }else{
        Xn <- bayesassurance::gen_Xn(n = rep(n, p))
        Xn_t <- t(Xn)
      }
    }

    # Components that only need to be computed once that will be
    # used in the MC sampling loop below
    Xn_mu <- Xn %*% mu_beta_d
    V_mu <- Vbeta_a_inv %*% mu_beta_a
    m_V_m <- mu_beta_a_t %*% Vbeta_a_inv %*% mu_beta_a
    XVX <- Xn %*% Vbeta_d %*% Xn_t
    a_star <- a_sig_a + (n / 2)

    # Compute M needed for Analysis using Cholesky
    if(is.null(Vn)){
      Vn <- matrix(0, nrow = n*p, ncol = n*p)
      Vn <- diag(n)
    }
    Vn_inv <- chol2inv(chol(Vn))
    L_tran <- chol(Vbeta_a_inv + Xn_t %*% Vn_inv %*% Xn)
    v <- backsolve(L_tran, Vn)
    M <- v %*% t(v)

    count2 <- 0 # counter for datasets that meet analysis objective

    # There will be two loops: the outer loop loops through R
    # datasets (indexed by i) characterized by uniquely drawn
    # gamma_sq values that are used to generate y_ni. This y_ni is then used
    # to determine the components of the posterior distribution,
    # to which iterates through a set of MC samples, determining
    # if the overall i-th dataset satisfies the analysis objective.
    # The proportion of datasets that meet the analysis objective
    # is the assurance.
    for(i in 1:R){
      # Design Stage Begins
      gamma_sq <- 1 / stats::rgamma(n = 1, a_sig_d, b_sig_d)
      y_ni <- MASS::mvrnorm(1, Xn_mu, gamma_sq * XVX + gamma_sq * Vn)
      # Design Stage Ends

      # Analysis Stage Begins
      m <- V_mu + Xn_t %*% Vn_inv %*% y_ni
      Mm <- M %*% m
      b_star <- b_sig_a + 0.5 * (m_V_m + t(y_ni) %*% Vn_inv %*% y_ni - 
                                   t(m) %*% Mm)

      count1 <- 0
      # counter for number of times analysis objective is
      # met within the i-th dataset, where the i-th dataset is
      # characterized by a specific gamma_sq, see above.
      for(j in 1:mc_iter){
        sig_j <- 1 / stats::rgamma(n = 1, a_star, b_star)
        beta_j <- MASS::mvrnorm(n = 1, Mm, sig_j * M)

        # checks satisfaction of analysis objective specific to
        # how "alt" is specified
        if(alt == "greater"){
          Zj <- ifelse((C - t(u) %*% Mm) / (sqrt(sig_j) * 
                       sqrt(t(u) %*% M %*% u))
                       < stats::qnorm(alpha), 1, 0)
        }else if(alt == "less"){
          Zj <- ifelse((C - t(u) %*% Mm) / (sqrt(sig_j) * 
                       sqrt(t(u) %*% M %*% u))
                       > stats::qnorm(1-alpha), 1, 0)
        }else if(alt == "two.sided"){
          Zj <- ifelse((C - t(u) %*% Mm) / (sqrt(sig_j) * 
                       sqrt(t(u) %*% M %*% u))
                       > stats::qnorm(1-alpha/2) | (C - t(u) %*% Mm) / 
                         (sqrt(sig_j) * sqrt(t(u) %*% M %*% u))
                       < stats::qnorm(alpha/2), 1, 0)
        }
        count1 <- ifelse(Zj == 1, count1 <- count1 + 1, count1 <- count1)
      }

      Zi <- ifelse((count1 / mc_iter) >= 1 - alpha, 1, 0)
      count2 <- ifelse(Zi == 1, count2 <- count2 + 1, count2 <- count2)
      # Analysis Stage Ends

    }

    assurance <- count2 / R
    return(assurance)
  }

  assurance <- pbapply::pbsapply(n, function(i) MC_sample(n=i))

  # Assurance table
  assur_tab <- as.data.frame(cbind(n, assurance))
  colnames(assur_tab) <- c("n", "Assurance")
  assur_tab <- structure(assur_tab, class = "data.frame")

  # ggplot
  if(length(n) > 1){
    assur_plot <- ggplot2::ggplot(assur_tab, alpha = 0.5, 
      ggplot2::aes(x = .data$n, y = .data$Assurance)) +
      ggplot2::geom_line(aes(x = .data$n, y = .data$Assurance), lwd = 1.2) + 
      ggplot2::ggtitle("Assurance Curve") +
      ggplot2::xlab("Sample Size n") + ggplot2::ylab("Assurance")
    assur_plot <- structure(assur_plot, class = "ggplot")
  }

  if(length(n) > 1){
    return(list(assurance_table = assur_tab, assur_plot = assur_plot, 
                mc_samples = mc_iter))
  }else{
    return(list(assur_val = paste0("Assurance: ", round(assurance, 3)), 
                mc_samples = mc_iter))
  }

}

