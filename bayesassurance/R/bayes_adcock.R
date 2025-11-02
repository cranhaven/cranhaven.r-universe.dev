#' Bayesian Assurance Computation in the Precision-Based Setting
#'
#' Returns the Bayesian assurance of observing that the
#' absolute difference between the true underlying
#' population parameter and the sample estimate falls within a margin
#' of error no greater than a fixed precision level, d.
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline aes theme 
#' @importFrom ggplot2 element_text xlab ylab ggtitle scale_x_continuous 
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom rlang .data
#' @importFrom stats pnorm rnorm
#' @importFrom pbapply pbsapply
#' @param n sample size (either vector or scalar).
#' @param d fixed precision level
#' @param mu_beta_a analysis stage mean
#' @param mu_beta_d design stage mean
#' @param n_a sample size at analysis stage. Also quantifies the amount of 
#' prior information we have for parameter \eqn{\mu}.
#' @param n_d sample size at design stage. Also quantifies the amount 
#' of prior information we have for where the data is being generated from.
#' @param sig_sq known variance \eqn{\sigma^2}.
#' @param alpha significance level
#' @param mc_iter number of MC samples evaluated under the analysis objective
#' @return approximate Bayesian assurance under precision-based conditions
#' @examples
#' n <- seq(20, 145, 5)
#' out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
#'                       n_a = 20, n_d = 10, sig_sq = 0.265,
#'                       alpha = 0.05, mc_iter = 1000)
#' head(out$assurance_table)
#' out$assurance_plot
#' @export
#'

bayes_adcock <- function(n, d, mu_beta_a, mu_beta_d, n_a, n_d, sig_sq, alpha, 
                         mc_iter = 1000){

  is.scalar <- function(x) is.atomic(x) && length(x) == 1L
  
  # test for parameters passed in
  if(is.scalar(d) == FALSE){
    stop("Precision level d must be a scalar value.")
  }
  
  if(is.scalar(sig_sq) == FALSE){
    stop("variance must be scalar value.")
  }
  
  if(is.scalar(n_a) == FALSE | is.scalar(n_d) == FALSE){
    stop("Precision parameters n_a and n_d must be scalar values.")
  }
  
  if(alpha < 0 | alpha > 1){
    stop("Not a valid significance level, alpha must be between 0 and 1.")
  }
  
  count <- 0

  # will rely on this embedded function to generate data and
  # assess satisfaction of analysis objective for n
  MC_sample <- function(n = n){
  for(i in 1:mc_iter){
    # Design Stage Begins (data is generated)
    var_d <- sig_sq * ((n_d + n) / (n * n_d))
    xbar <- stats::rnorm(n=1, mean = mu_beta_d, sd = sqrt(var_d))
    # Design Stage Ends

    # Analysis Stage Begins (checks if data meets analysis objective)
    # Posterior mean for unknown parameter mu
    lambda <- ((n_a * mu_beta_a) + (n * xbar)) / (n_a + n)

    # Components that make up the analysis stage objective
    phi_1 <- (sqrt(n_a + n) / sqrt(sig_sq)) * (xbar + d - lambda)
    phi_2 <- (sqrt(n_a + n) / sqrt(sig_sq)) * (xbar - d - lambda)

    # Analysis Stage Objective
    Zi <- ifelse(stats::pnorm(phi_1) - stats::pnorm(phi_2) >= 1 - alpha, 1, 0)

    # Add 1 to count if analysis objective is met
    count <- ifelse(Zi == 1, count <- count + 1, count <- count)
    # Analysis Stage Ends
  }

  assurance <- count / mc_iter
  return(assurance)
  }

  # determines the assurance across all values of n using
  # the MC_sample() function
  assurance <- pbapply::pbsapply(n, function(i) MC_sample(n=i))

  # Assurance table
  assur_tab <- as.data.frame(cbind(n, assurance))
  colnames(assur_tab) <- c("n", "Assurance")
  assur_tab <- structure(assur_tab, class = "data.frame")

  # creates ggplot for vector entries of n
  if(length(n) > 1){
    assur_plot <- ggplot2::ggplot(assur_tab, alpha = 0.5, aes(x = .data$n, 
                  y = .data$Assurance)) +
      ggplot2::geom_point(aes(x = .data$n, y = .data$Assurance), lwd = 1.2) + 
      ggplot2::ggtitle("Assurance Values") +
      ggplot2::xlab("Sample Size n") + ggplot2::ylab("Assurance")
    assur_plot <- structure(assur_plot, class = "ggplot")
  }

  if(length(n) > 1){
    return(list(assurance_table = assur_tab, assurance_plot = assur_plot, 
                mc_samples = mc_iter))
  }else{
    return(list(assur_val = paste0("Assurance: ", round(assurance, 3)), 
                mc_samples = mc_iter))
  }

}
