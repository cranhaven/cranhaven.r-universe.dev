#' Plot the incubation or latency time distribution
#'
#' @description
#' Plot the estimated probability density function using a doublIn output
#' object.
#'
#' @param doublIn_obj output list from function Estimate_doublIn containing
#' parameter estimates.
#' @param label_x Label for the x-axis.
#' @param label_y Label for the y-axis.
#' @param p_linepieces Percentiles to indicate by vertical line pieces, given
#' as vector of length three with probabilities between 0 and 1.
#' @param p_shaded Which area needs to be shaded, i.e. enclosed by which
#' percentile from the vector p_linepieces (1, 2 or 3).
#'
#' @return Plot of the estimated probability density function.
#'
#' @details
#' The function plots the estimated probability density function using the
#' parameterisation of the generalized gamma distribution by Stacy et al. that
#' includes the gamma and Weibull distributions as a special case.
#'
#' @references Stacy, E. W., and G. A. Mihram, Parameter estimation for a
#' generalized gamma distribution, Technometrics, 7 (3), 349–358,
#' doi:10.1080/00401706.1965.10490268, 1965
#' @author Vera Arntzen, \email{v.h.arntzen@@math.leidenuniv.nl}
#' @keywords survival
#' @import ggplot2
#' @import flexsurv
#' @import dplyr
#'
#' @export
Plot_doublIn <- function(doublIn_obj, label_x = "Days since infection",
                         label_y = "Probability", p_shaded = 2,
                         p_linepieces = c(0.5, 0.9, 0.95)
                         ){

  my_tab <- doublIn_obj$estimates

  theta <- my_tab %>% filter(par == "theta") %>% select(est) %>% as.numeric()
  theta_lower <-  my_tab %>% filter(par == "theta") %>% select(lower_CI) %>%
    as.numeric()
  theta_upper <- my_tab %>% filter(par == "theta") %>% select(upper_CI) %>%
    as.numeric()
  kappa <-  my_tab %>% filter(par == "kappa") %>% select(est) %>%
    as.numeric()
  kappa_lower <- my_tab %>% filter(par == "kappa") %>% select(lower_CI) %>%
    as.numeric()
  kappa_upper <- my_tab %>% filter(par == "kappa") %>% select(upper_CI) %>%
    as.numeric()
  delta <- my_tab %>% filter(par == "delta") %>% select(est) %>% as.numeric()
  delta_lower <- my_tab %>% filter(par == "delta") %>% select(lower_CI) %>%
    as.numeric()
  delta_upper <- my_tab %>% filter(par == "delta") %>% select(upper_CI) %>%
    as.numeric()

  cols <- c("maroon2", "mediumspringgreen", "darkblue")

  # Plot PDF
  Fig <- ggplot() +

      geom_line(aes(x = seq(0, 26, by= 0.01), y = dgengamma.orig(
          x = seq(0, 26, by = 0.01), shape = delta[1], scale = theta[1], k =
          kappa[1]/delta[1])), size = 1, color = cols[p_shaded]) +

      geom_area(aes(x = seq(0, qgengamma.orig(p = p_linepieces[p_shaded], shape = delta[1],
          scale = theta[1], k = kappa[1]/delta[1]), by= 0.01), y =
          dgengamma.orig(x = seq(0, qgengamma.orig(p = p_linepieces[p_shaded], shape = delta[1],
          scale = theta[1], k = kappa[1]/delta[1]), by= 0.01), shape = delta[1],
          scale = theta[1], k = kappa[1]/delta[1])), fill = cols[p_shaded],
          alpha = 0.3, size = 1) +

      annotate("text", x = 0.5 + qgengamma.orig(p = p_linepieces, shape = delta[1],
          scale = theta[1], k = kappa[1]/delta[1]), y = 0.015 +
          dgengamma.orig(x = qgengamma.orig(p = p_linepieces, shape = delta[1],
          scale = theta[1], k = kappa[1]/delta[1]), shape = delta[1], scale =
          theta[1], k = kappa[1]/delta[1]) + 0.04, label =
          paste(round(100 * p_linepieces, 1), "%", sep = ""), size = 4,
          color = "black")


      Fig1 <- Fig + geom_segment(aes(
          x = qgengamma.orig(p = p_linepieces[1], shape = delta[1], scale =
          theta[1], k = kappa[1]/delta[1]),

          xend = qgengamma.orig(p = p_linepieces[1],
          shape = delta[1], scale = theta[1], k = kappa[1]/delta[1]),

          y = dgengamma.orig(x = qgengamma.orig(p = p_linepieces[1],
                                                shape = delta[1], scale =
          theta[1], k = kappa[1]/delta[1]), shape = delta[1], scale = theta[1],
          k = kappa[1]/delta[1]) - 0.04,

          yend = dgengamma.orig(x =
          qgengamma.orig(p = p_linepieces[1], shape = delta[1], scale = theta[1], k =
          kappa[1]/delta[1]), shape = delta[1], scale = theta[1], k =
          kappa[1]/delta[1]) + 0.04),
          size = 1, color = cols[1] )

      Fig2 <- Fig1 + geom_segment(aes(
        x = qgengamma.orig(p = p_linepieces[2], shape = delta[1], scale =
                             theta[1], k = kappa[1]/delta[1]),

        xend = qgengamma.orig(p = p_linepieces[2],
                              shape = delta[1], scale = theta[1], k = kappa[1]/delta[1]),

        y = dgengamma.orig(x = qgengamma.orig(p = p_linepieces[2],
                                              shape = delta[1], scale =
                                                theta[1], k = kappa[1]/delta[1]), shape = delta[1], scale = theta[1],
                           k = kappa[1]/delta[1]) - 0.04,

        yend = dgengamma.orig(x =
                                qgengamma.orig(p = p_linepieces[2], shape = delta[1], scale = theta[1], k =
                                                 kappa[1]/delta[1]), shape = delta[1], scale = theta[1], k =
                                kappa[1]/delta[1]) + 0.04),
        size = 1, color = cols[2] )

      Fig3 <- Fig2 + geom_segment(aes(
        x = qgengamma.orig(p = p_linepieces[3], shape = delta[1], scale =
                             theta[1], k = kappa[1]/delta[1]),

        xend = qgengamma.orig(p = p_linepieces[3],
                              shape = delta[1], scale = theta[1], k = kappa[1]/delta[1]),

        y = dgengamma.orig(x = qgengamma.orig(p = p_linepieces[3],
                                              shape = delta[1], scale =
                                                theta[1], k = kappa[1]/delta[1]), shape = delta[1], scale = theta[1],
                           k = kappa[1]/delta[1]) - 0.04,

        yend = dgengamma.orig(x =
                                qgengamma.orig(p = p_linepieces[3], shape = delta[1], scale = theta[1], k =
                                                 kappa[1]/delta[1]), shape = delta[1], scale = theta[1], k =
                                kappa[1]/delta[1]) + 0.04),
        size = 1, color = cols[3] )

      Fig <- Fig3 +

      # Layout
      theme(panel.background = element_rect(fill = "white", colour = "black",
                                            size = 0.5, linetype = "solid")) +
      theme(panel.grid.major = element_line(colour = "grey"), panel.grid.minor =
              element_line(colour = "grey")) +
      labs(x = label_x, y = label_y) +
      theme(axis.line = element_line(size = 0.3, colour = "black",
              linetype = 1)) + expand_limits(x = 0, y = 0)

      return(Fig)

}
