#' Frequentist Power Computation
#'
#' Constructs a simple hypothesis testing framework based on the parameters 
#' specified and returns the corresponding frequentist power.
#' @importFrom ggplot2 ggplot aes geom_line theme xlab ylab ggtitle
#' @importFrom rlang .data
#' @importFrom stats pnorm qnorm
#' @param n sample size (either scalar or vector)
#' @param theta_0 initial value the parameter is set equal to in the null 
#' hypothesis, where \eqn{H0: \theta = \theta 0}.
#' @param theta_1 alternative value to be compared to theta_0. 
#' See alt for specification options.
#' @param sigsq known variance \eqn{\sigma^2}
#' @param alt specifies comparison between \eqn{\theta_1} and \eqn{\theta_0}, 
#' where `alt = "greater"` tests if \eqn{\theta_1 > \theta_0},
#' `alt = "less"` tests if \eqn{\theta_1 < \theta_0}, and `alt = "two.sided"` 
#' performs a two-sided test. `alt = "greater"` by
#' default.
#' @param alpha significance level
#' @return objects corresponding to the power
#' \itemize{
#'      \item{pwr_table:} table of sample sizes and corresponding power
#'      values.
#'      \item{pwr_plot:} power curve that is only returned if n is a vector. 
#'      This power curve covers a wider range of sample sizes than the 
#'      inputted values specified for n, where specific power values are marked 
#'      in red.
#'      \item{power_val:} single power value that is returned if n is a scalar.
#' }
#' @examples
#' n <- seq(10, 140, 5)
#' theta_0 <- 0.15
#' theta_1 <- 0.35
#' sigsq <- 0.3
#'
#' pwr_vals <- pwr_freq(n = n, theta_0 = theta_0, theta_1 = theta_1, 
#' sigsq = sigsq, alt = "greater", alpha = 0.05)
#' pwr_vals$pwr_plot
#' @export
#'
pwr_freq <- function(n, theta_0, theta_1, sigsq, alt, alpha){
  delta <- theta_1 - theta_0

  if(alt == "greater"){
    z_alpha <- stats::qnorm(1-alpha)
    pwr_val <- ((sqrt(n) * delta) / sqrt(sigsq)) - z_alpha
    pwr <- stats::pnorm(pwr_val)
  }else if(alt == "less"){
    z_alpha <- stats::qnorm(1-alpha)
    pwr_val <- ((sqrt(n) * delta) / sqrt(sigsq)) + z_alpha
    pwr <- 1 - stats::pnorm(pwr_val)
  }else if(alt == "two.sided"){
    z_alpha <- stats::qnorm(1-alpha/2)
    pwr <- 1 + stats::pnorm(((sqrt(n) * delta) / sqrt(sigsq)) - z_alpha) -
      stats::pnorm(((sqrt(n) * delta) / sqrt(sigsq)) + z_alpha)
  }

  # Power table
  pwr_tab <- as.data.frame(cbind(n, pwr))
  colnames(pwr_tab) <- c("n", "Power")
  pwr_tab <- structure(pwr_tab, class = "data.frame")

  # ggplot
  if(length(n) > 1){
    new_min <- min(n) - (max(n) - min(n))
    new_max <- (max(n) - min(n)) + max(n)
    new_n <- seq(new_min, new_max, by = 1)
    new_n <- new_n[new_n > 0]

    if(alt == "greater"){
      new_pwrvals <- stats::pnorm(((sqrt(new_n) * delta) / sqrt(sigsq)) - 
                                    z_alpha)
      new_pwrtab <- as.data.frame(cbind(new_n, new_pwrvals))
      colnames(new_pwrtab) <- c("n", "Power")
    }else if(alt == "less"){
      new_pwrvals <- 1 - stats::pnorm(((sqrt(new_n) * delta) / sqrt(sigsq)) + 
                                        z_alpha)
      new_pwrtab <- as.data.frame(cbind(new_n, new_pwrvals))
      colnames(new_pwrtab) <- c("n", "Power")
    }else if(alt == "two.sided"){
      new_pwrvals <- 1 + stats::pnorm(((sqrt(new_n) * delta) / sqrt(sigsq)) - 
                                        z_alpha) -
        stats::pnorm(((sqrt(new_n) * delta) / sqrt(sigsq)) + z_alpha)
      new_pwrtab <- as.data.frame(cbind(new_n, new_pwrvals))
      colnames(new_pwrtab) <- c("n", "Power")
    }


    pwr_plot <- ggplot2::ggplot(new_pwrtab, alpha = 0.5, aes(x = .data$n, 
                                                             y = .data$Power)) +
      ggplot2::geom_line(aes(x = .data$n, y = .data$Power), lwd = 1.2) + 
      ggplot2::ggtitle("Power Curve") +
      ggplot2::xlab("Sample Size n") + ggplot2::ylab("Power") + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    pwr_plot2 <- pwr_plot + ggplot2::geom_point(data = pwr_tab, 
                                      aes(x = .data$n, y = .data$Power),
                                      size = 1, color = "red")
    pwr_plot2 <- structure(pwr_plot2, class = "ggplot")
  }


  if(length(n) > 1){
    return(list(pwr_table = pwr_tab, pwr_plot = pwr_plot2))
  }else{
    return(power_val = paste0("Power: ", round(pwr, 3)))
  }

}

