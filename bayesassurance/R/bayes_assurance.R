#' Bayesian Assurance Computation
#'
#' Takes in a set of parameters and returns the exact Bayesian 
#' assurance based on a closed-formed solution.
#' @importFrom ggplot2 ggplot aes geom_line theme xlab ylab ggtitle
#' @importFrom rlang .data
#' @importFrom stats pnorm qnorm
#' @param n sample size (either scalar or vector)
#' @param theta_0 parameter value that is known a priori 
#' (typically provided by the client)
#' @param theta_1 alternative parameter value that will be 
#' tested in comparison to theta_0. See alt for specification options.
#' @param n_a sample size at analysis stage that quantifies the amount 
#' of prior information we have for parameter \eqn{\theta}. This should 
#' be a single scalar value. 
#' @param n_d sample size at design stage that quantifies the amount of 
#' prior information we have for where the data is being generated from. 
#' This should be a single scalar value. 
#' @param sigsq known variance \eqn{\sigma^2}.
#' @param alt specifies alternative test case, where `alt = "greater"` 
#' tests if \eqn{\theta_1 > \theta_0}, `alt = "less"` tests if 
#' \eqn{\theta_1 < \theta_0}, and `alt = "two.sided"` 
#' performs a two-sided test. `alt = "greater"` by default.
#' @param alpha significance level
#' @return objects corresponding to the assurance
#' \itemize{
#'      \item{assurance_table:} table of sample sizes and 
#'      corresponding assurance values.
#'      \item{assurance_plot:} assurance curve that is only 
#'      returned if n is a vector. This curve covers a wider range 
#'      of sample sizes than the inputted values specified
#'      for n, where specific assurance values are marked in red.
#' }
#' @examples
#' 
#' ## Assign the following fixed parameters to determine the Bayesian assurance
#' ## for the given vector of sample sizes.
#' n <- seq(10, 250, 5)
#' n_a <- 1e-8
#' n_d <- 1e+8
#' theta_0 <- 0.15
#' theta_1 <- 0.25
#' sigsq <- 0.104
#' assur_vals <- assurance_nd_na(n = n, n_a = n_a, n_d = n_d, 
#' theta_0 = theta_0, theta_1 = theta_1,
#' sigsq = sigsq, alt = "two.sided", alpha = 0.05)
#' assur_vals$assurance_plot
#' @export
assurance_nd_na <- function(n, n_a, n_d, theta_0, theta_1, sigsq, alt, 
                            alpha = 0.05){

  is.scalar <- function(x) is.atomic(x) && length(x) == 1L
  
  # test for parameters passed in
  if(is.scalar(n_a) == FALSE | is.scalar(n_d) == FALSE){
    stop("n_a and n_d must be scalar quantites.")
  }
  
  if(is.scalar(theta_0) == FALSE){
    stop("theta_0 must be scalar.")
  }
  
  if(is.scalar(theta_1) == FALSE){
    stop("theta_1 must be scalar.")
  }
  
  if(is.scalar(sigsq) == FALSE | sigsq < 0){
    stop("sigsq must be a positive scalar.")
  }
  
  if(alpha < 0 | alpha > 1){
    stop("Not a valid significance level, alpha must be between 0 and 1.")
  }
  
  if((alt %in% c("greater", "less", "two.sided")) == FALSE | is.null(alt)){
    stop("Please specify one of the three options for alternative test case: 
         greater, less, two.sided.")
  }
  
  # critical difference
  delta <- theta_1 - theta_0

  # computes the assurance based on what is being tested (specification of "alt")
  if(alt == "greater"){
    z_alpha <- stats::qnorm(alpha)
    phi_val <- sqrt((n_d / (n + n_d)) * (1 + n_a / n)) * 
      (((sqrt(n) * delta) / sqrt(sigsq)) + z_alpha)
    b_assurance <- pnorm(phi_val)
  }else if(alt == "less"){
    z_alpha <- stats::qnorm(1-alpha)
    phi_val <- sqrt((n_d / (n + n_d)) * (1 + n_a / n)) * 
      (((sqrt(n) * delta) / sqrt(sigsq)) + z_alpha)
    b_assurance <- 1 - pnorm(phi_val)
  }else if(alt == "two.sided"){
    z_alpha_1 <- stats::qnorm(alpha/2)
    z_alpha_2 <- stats::qnorm(1-alpha/2)
    phi_val_1 <- sqrt((n_d / (n + n_d)) * (1 + n_a / n)) * 
      (((sqrt(n) * delta) / sqrt(sigsq)) + z_alpha_1)
    phi_val_2 <- sqrt((n_d / (n + n_d)) * (1 + n_a / n)) * 
      (((sqrt(n) * delta) / sqrt(sigsq)) + z_alpha_2)
    b_assurance <- 1-stats::pnorm(phi_val_2) + stats::pnorm(phi_val_1)
  }


  # Assurance table
  assur_tab <- as.data.frame(cbind(n, b_assurance))
  colnames(assur_tab) <- c("n", "Assurance")
  assur_tab <- structure(assur_tab, class = "data.frame")

  # If n is a vector of sample sizes, the function will create
  # a full assurance curve that includes both the sample sizes
  # being passed in as well as everything else in between.
  # This is done by specifying a wider range of sample
  # sizes within the bounds of "new_min" and "new_max",
  # resulting in an updated set of sample sizes referred to
  # as "new_n".
  if(length(n) > 1){
    new_min <- min(n) - (max(n) - min(n))
    new_max <- (max(n) - min(n)) + max(n)
    new_n <- seq(new_min, new_max, by = 1)
    new_n <- new_n[new_n > 0]

    # evaluates the assurance for all values of new_n
    if(alt == "greater"){
      z_alpha <- stats::qnorm(alpha)
      new_assurvals <- stats::pnorm(sqrt((n_d / (new_n + n_d)) * 
      (1 + n_a / new_n)) * (((sqrt(new_n) * delta) / sqrt(sigsq)) + z_alpha))
      new_assurtab <- as.data.frame(cbind(new_n, new_assurvals))
      colnames(new_assurtab) <- c("n", "Assurance")
    }else if(alt == "less"){
      z_alpha <- stats::qnorm(1-alpha)
      new_assurvals <- 1 - stats::pnorm(sqrt((n_d / (new_n + n_d)) * 
      (1 + n_a / new_n)) * (((sqrt(new_n) * delta) / sqrt(sigsq)) + z_alpha))
      new_assurtab <- as.data.frame(cbind(new_n, new_assurvals))
      colnames(new_assurtab) <- c("n", "Assurance")
    }else if(alt == "two.sided"){
      z_alpha_1 <- stats::qnorm(alpha/2)
      z_alpha_2 <- stats::qnorm(1-alpha/2)
      phi_val_1 <- sqrt((n_d / (new_n + n_d)) * (1 + n_a / new_n)) * 
        (((sqrt(new_n) * delta) / sqrt(sigsq)) + z_alpha_1)
      phi_val_2 <- sqrt((n_d / (new_n + n_d)) * (1 + n_a / new_n)) * 
        (((sqrt(new_n) * delta) / sqrt(sigsq)) + z_alpha_2)
      new_assurvals <- 1-stats::pnorm(phi_val_2) + stats::pnorm(phi_val_1)
      new_assurtab <- as.data.frame(cbind(new_n, new_assurvals))
      colnames(new_assurtab) <- c("n", "Assurance")
    }

   # Creates assurance plot, highlighting the ones being passed in as red points
    assur_plot <- ggplot2::ggplot(new_assurtab, alpha = 0.5, 
      aes(x = .data$n, y = .data$Assurance)) +
      ggplot2::geom_line(aes(x = .data$n, y = .data$Assurance), lwd = 1.2) + 
      ggplot2::ggtitle("Assurance Curve") +
      ggplot2::xlab("Sample Size n") + ggplot2::ylab("Assurance") + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    assur_plot2 <- assur_plot + ggplot2::geom_point(data = assur_tab, 
      aes(x = .data$n, y = .data$Assurance), size = 1, color = "red")
    assur_plot2 <- structure(assur_plot2, class = "ggplot")
  }

  if(length(n) > 1){
    return(list(assurance_table = assur_tab, assurance_plot = assur_plot2))
  }else{
    return(assurance_val = paste0("Assurance: ", round(b_assurance, 3)))
  }
}

