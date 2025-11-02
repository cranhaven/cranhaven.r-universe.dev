#' Plotting Power and Assurance Curves Simultaneously
#'
#' Constructs a single plot based on the combined set of inputs used to
#' compute the frequentist power and Bayesian assurance. The plot includes 
#' three curves that include the power curve, the assurance curve obtained 
#' under a closed-form solution, and optionally, the assurance curve 
#' obtained under a simulation setting.
#'
#' @importFrom ggplot2 ggplot aes geom_line theme xlab ylab ggtitle
#' @importFrom rlang .data
#' @param n sample size (either a scalar or vector)
#' @param n_a sample size at analysis stage (setting n_a close to 0 corresponds 
#' to a weak analysis prior)
#' @param n_d sample size at design stage
#' @param theta_0 parameter value that is known a priori (typically 
#' provided by the client)
#' @param theta_1 alternative parameter value that will be tested in 
#' comparison to theta_0. See alt for specification options.
#' @param sigsq known variance \eqn{\sigma^2}
#' @param alt specifies alternative test case, where alt = "greater" 
#' tests if theta_1 > theta_0, "less" tests if theta_1 < theta_0, and 
#' "two.sided" performs a two-sided test. alt = "greater" by default.
#' @param alpha significance level
#' @param bayes_sim when set to "TRUE", this indicates that the user wants 
#' to include simulated assurance results in the outputted plot. 
#' Default setting is "FALSE".
#' @param mc_iter number of MC samples provided that bayes_sim = TRUE
#' @return plot of overlayed power and assurance curves produced using ggplot2
#' @return a list of objects corresponding to the power/assurance curves
#' \itemize{
#'      \item{power:} table of sample sizes and corresponding power values
#'      obtained from bayesassurance::pwr_freq().
#'      \item{assurance_table:} table of sample sizes and corresponding 
#'      assurance values obtained from bayesassurance::assurance_nd_na().
#'      \item{bayes_sim_table:} table of sample sizes and corresponding
#'      assurance values obtained from MC sampling using
#'      bayesassurance::bayes_sim(). Returned only if bayes_sim = TRUE.
#'      \item{mc_samples:} number of Monte Carlo samples that were generated
#'      and evaluated if bayes_sim = TRUE.
#'      \item{plot:} plot of overlayed power/assurance curves.
#' }
#' @examples
#'
#' ## Case 1: Weak Analysis Prior (n_a set to be small) + Strong Design Prior
#' ## (n_d set to be large) that results in the Bayesian assurance and 
#' ## frequentist curve perfectly overlapping one another.
#' \donttest{
#' n <- seq(10, 200, 10)
#' n_a <- 1e-8
#' n_d <- 1e+8
#' theta_0 <- 0.15
#' theta_1 <- 0.25
#' sigsq <- 0.104
#' alpha <- 0.05
#'
#' ## outputs all three plots
#' out <- bayesassurance::pwr_curve(n = n, n_a = n_a, n_d = n_d, 
#' theta_0 = theta_0, theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
#' alpha = alpha, bayes_sim = TRUE, mc_iter = 5000)
#'
#' ## only outputs the closed-form solution power and assurance curves
#' pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, theta_1 = theta_1,
#' sigsq = sigsq, alt = "greater", alpha = alpha, bayes_sim = FALSE)
#' }
#' 
#'
#' ## Case 2: Weak Analysis Prior (n_a set to be small) + Weak Design Prior
#' ## (n_d set to be small) that results in a assurance curve,
#' ## which illustrates the noninformative prior setting.
#' n <- seq(10, 200, 10)
#' n_a <- 1e-8
#' n_d <- 1e-8
#' theta_0 <- 0.15
#' theta_1 <- 0.25
#' sigsq <- 0.104
#' alpha <- 0.05
#'
#' bayesassurance::pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0,
#' theta_1 = theta_1, sigsq = sigsq, alt = "greater", alpha = alpha, 
#' bayes_sim = TRUE, mc_iter = 1000)
#' 
#' @seealso \code{\link{ggplot2}}, \code{\link{pwr_freq}} for 
#' frequentist power function and \code{\link{bayes_sim}} for the 
#' Bayesian assurance function
#' @export


pwr_curve <- function(n, n_a, n_d, theta_0, theta_1, sigsq, alt = "greater", 
                      alpha, bayes_sim = FALSE, mc_iter = NULL){

  if(length(n) <= 1){
    stop("length of n needs to be greater than 1.")
  }

  # obtained the power and assurance curves to include in plot
  y1 <- bayesassurance::pwr_freq(n = n, theta_0 = theta_0, theta_1 = theta_1, 
        sigsq = sigsq, alt = alt, alpha = alpha)
  y2 <- bayesassurance::assurance_nd_na(n = n, n_a = n_a, n_d = n_d, 
        theta_0 = theta_0, theta_1 = theta_1, sigsq = sigsq, alt = alt, 
        alpha = alpha)

  df1 <- y1$pwr_table
  colnames(df1) <- c("n", "Power")
  df1<- structure(df1, class = "data.frame")

  df2 <- y2$assurance_table
  colnames(df2) <- c("n", "Assurance")
  df2 <- structure(df2, class = "data.frame")

  # uses ggplot2 to compile table
  p <- ggplot2::ggplot(df1, alpha = 0.5, ggplot2::aes(x = .data$n, 
       y = round(.data$Power, 3), color="Power")) +
       ggplot2::geom_line(aes(x = .data$n, y = .data$Power), lwd = 1.2)
  p1 <- p + ggplot2::geom_point(data = df2, alpha = 0.5, 
        ggplot2::aes(x = .data$n, y = round(.data$Assurance, 3), 
                     color="Assurance"),lwd=1.2)
  pwr_curves <- p1 + ggplot2::xlab("Sample Size (n)") + 
    ggplot2::ylab("Power/Assurance") +
    ggplot2::ggtitle("Power Curves of Frequentist and Bayesian Methods")

  # checks for bayes_sim option to see if simulated assurance points 
  # should also be included
  if(bayes_sim == TRUE){
    if(is.null(mc_iter)){
      mc_iter <- 5000
    }
    y3 <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = theta_0, Xn = NULL,
                                    Vbeta_d = 1/n_d, Vbeta_a_inv = 0, Vn = NULL,
                                    sigsq = sigsq, mu_beta_d = theta_1,
                                    mu_beta_a = 0, alt= alt, alpha = 0.05,
                                    mc_iter = mc_iter)
    df3 <- y3$assurance_table
    colnames(df3) <- c("n", "Assurance")
    p3 <- pwr_curves + ggplot2::geom_point(data = df3, alpha = 0.5, 
          ggplot2::aes(x = .data$n, y = round(.data$Assurance, 3),
          color = "Simulated Assurance"))
    pwr_curves <- p3 + ggplot2::xlab("Sample Size (n)") + 
      ggplot2::ylab("Power/Assurance") +
      ggtitle("Power Curves of Frequentist and Bayesian Methods")

    pwr_curves<- structure(pwr_curves, class = "ggplot")

    return(list(power_table = df1, assurance_table = df2, 
                bayes_sim_table = df3, mc_samples = mc_iter,
                plot = pwr_curves))
  }else{
    pwr_curves<- structure(pwr_curves, class = "ggplot")
    return(list(power_table = df1, assurance_table = df2,
                plot = pwr_curves))
  }

}



