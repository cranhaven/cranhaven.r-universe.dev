# Required package: ggplot2 (declared in DESCRIPTION Imports)

# Line types corresponding to lty = c(1, 2, 3, 5) in base R, used by the
# four density-plotting functions below.
.line_types <- c("solid", "dashed", "dotted", "longdash")


#' Generate EVBS Density Values
#'
#' Computes \eqn{(t, f(t))} pairs of the Extreme-Value Birnbaum-Saunders
#' (EVBS) density over its support, for given parameters.
#'
#' @param alpha Shape parameter \eqn{\alpha > 0}.
#' @param beta Scale parameter \eqn{\beta > 0}.
#' @param gama Tail-shape parameter \eqn{\gamma} (real). The support is
#'   bounded below when \eqn{\gamma > 0} and above when \eqn{\gamma < 0}.
#'
#' @return A \code{data.frame} with columns \code{t} (support points) and
#'   \code{y} (density values).
#'
#' @references
#' Ferreira, M., Gomes, M. I., and Leiva, V. (2012). On an extreme value
#' version of the Birnbaum-Saunders distribution. \emph{REVSTAT}, 10,
#' 181--210.
#'
#' @seealso \code{\link{plot_evbs_alpha}}, \code{\link{generate_logevbs_data}}.
#'
#' @examples
#' d <- generate_evbs_data(alpha = 0.5, beta = 1, gama = 0.5)
#' plot(d$t, d$y, type = "l", xlab = "t", ylab = "f(t)")
#'
#' @export
generate_evbs_data <- function(alpha, beta, gama) {
  if (gama == 0) {
    t <- seq(0.001, 3.5, by = 0.001)
    a <- (1/alpha) * ((t/beta)^(1/2) - (t/beta)^(-1/2))
    A <- (1/(2*alpha*beta)) * ((t/beta)^(-1/2) + (t/beta)^(-3/2))
    y <- A * exp(-a - exp(-a))
    return(data.frame(t = t, y = y))
  }
  if (gama > 0) {
    l <- (beta/2) * ((alpha/gama)^2 + 2 -
                       (alpha/gama) * sqrt(4 + (alpha/gama)^2))
    t <- seq(l + 0.001, 3.5, by = 0.001)
  } else {
    s <- (beta/2) * ((alpha/gama)^2 + 2 -
                       (alpha/gama) * sqrt(4 + (alpha/gama)^2))
    t <- seq(0.001, s - 0.001, by = 0.001)
  }
  a <- (1/alpha) * ((t/beta)^(1/2) - (t/beta)^(-1/2))
  mask <- (1 + gama * a) > 0
  t <- t[mask]; a <- a[mask]
  if (!length(t)) return(data.frame(t = numeric(0), y = numeric(0)))
  A <- (1/(2*alpha*beta)) * ((t/beta)^(-1/2) + (t/beta)^(-3/2))
  u <- (1 + gama * a)^(-1/gama)
  y <- A * (u^(gama + 1)) * exp(-u)
  data.frame(t = t, y = y)
}



#' Generate log-EVBS Density Values
#'
#' Computes \eqn{(y, f(y))} pairs of the log-EVBS density over its support,
#' for given parameters.
#'
#' @param alpha Shape parameter \eqn{\alpha > 0}.
#' @param eta Location parameter \eqn{\eta} (real).
#' @param gama Tail-shape parameter \eqn{\gamma} (real).
#'
#' @return A \code{data.frame} with columns \code{y_vals} (support points)
#'   and \code{fdp} (density values).
#'
#' @references
#' Leiva, V., Ferreira, M., Gomes, M. I., and Lillo, C. (2016). Extreme
#' value Birnbaum-Saunders regression models applied to environmental data.
#' \emph{Stochastic Environmental Research and Risk Assessment}, 30,
#' 1045--1058.
#'
#' @seealso \code{\link{plot_logevbs_alpha}}, \code{\link{generate_evbs_data}}.
#'
#' @examples
#' d <- generate_logevbs_data(alpha = 1, eta = 0, gama = 0.5)
#' plot(d$y_vals, d$fdp, type = "l", xlab = "y", ylab = "f(y)")
#'
#' @export
generate_logevbs_data <- function(alpha, eta, gama) {
  if (gama == 0) {
    y_vals <- seq(-6, 6, by = 0.001)
    xi1 <- (2/alpha) * cosh((y_vals - eta) / 2)
    xi2 <- (2/alpha) * sinh((y_vals - eta) / 2)
    fdp <- (1/2) * xi1 * exp(-xi2 - exp(-xi2))
    return(data.frame(y_vals = y_vals, fdp = fdp))
  }
  if (gama > 0) {
    l <- eta + 2 * asinh(-alpha / (2 * gama))
    y_vals <- seq(l + 0.001, 6, by = 0.001)
  } else {
    s <- eta + 2 * asinh(-alpha / (2 * gama))
    y_vals <- seq(-6, s - 0.001, by = 0.001)
  }
  xi1 <- (2/alpha) * cosh((y_vals - eta) / 2)
  xi2 <- (2/alpha) * sinh((y_vals - eta) / 2)
  mask <- (1 + gama * xi2) > 0
  y_vals <- y_vals[mask]; xi1 <- xi1[mask]; xi2 <- xi2[mask]
  if (!length(y_vals)) return(data.frame(y_vals = numeric(0), fdp = numeric(0)))
  fdp <- (1/2) * xi1 * (1 + gama * xi2)^(-1 - 1/gama) *
           exp(-(1 + gama * xi2)^(-1/gama))
  data.frame(y_vals = y_vals, fdp = fdp)
}



#' Plot EVBS Densities for Varying alpha
#'
#' Reproduces Figure 1(a) of the paper: EVBS densities for
#' \eqn{\alpha \in \{0.25, 0.5, 0.75, 1\}}, \eqn{\beta = 1},
#' \eqn{\gamma = 0.5}. Uses \pkg{ggplot2} with the Dark2 colour palette.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{plot_evbs_gama}}, \code{\link{generate_evbs_data}}.
#'
#' @examples
#' \donttest{print(plot_evbs_alpha())}
#'
#' @import ggplot2
#' @export
plot_evbs_alpha <- function() {
  params <- list(
    list(alpha = 1.00, beta = 1, gama = 0.50),
    list(alpha = 0.75, beta = 1, gama = 0.50),
    list(alpha = 0.50, beta = 1, gama = 0.50),
    list(alpha = 0.25, beta = 1, gama = 0.50)
  )
  d <- do.call(rbind, lapply(params, function(p) {
    df <- generate_evbs_data(p$alpha, p$beta, p$gama)
    df$label <- paste0("EVBS(", p$alpha, ",", p$beta, ",", p$gama, ")")
    df
  }))
  ggplot2::ggplot(d, ggplot2::aes(x = t, y = y,
                                    color = label, linetype = label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_linetype_manual(values = .line_types) +
    ggplot2::labs(x = expression(italic(t)),
                  y = expression(italic(f(t))),
                  color = NULL, linetype = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = c(0.85, 0.8),
                   legend.background = ggplot2::element_blank())
}



#' Plot EVBS Densities for Varying gamma
#'
#' Reproduces Figure 1(b) of the paper: EVBS densities for
#' \eqn{\gamma \in \{-1.25, -0.75, 0.75, 1.25\}}, \eqn{\alpha = 0.5},
#' \eqn{\beta = 1.5}. Uses \pkg{ggplot2} with the Dark2 colour palette.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{plot_evbs_alpha}}.
#'
#' @examples
#' \donttest{print(plot_evbs_gama())}
#'
#' @import ggplot2
#' @export
plot_evbs_gama <- function() {
  params <- list(
    list(alpha = 0.5, beta = 1.5, gama = -1.25),
    list(alpha = 0.5, beta = 1.5, gama = -0.75),
    list(alpha = 0.5, beta = 1.5, gama =  0.75),
    list(alpha = 0.5, beta = 1.5, gama =  1.25)
  )
  d <- do.call(rbind, lapply(params, function(p) {
    df <- generate_evbs_data(p$alpha, p$beta, p$gama)
    df$label <- paste0("EVBS(", p$alpha, ",", p$beta, ",", p$gama, ")")
    df
  }))
  ggplot2::ggplot(d, ggplot2::aes(x = t, y = y,
                                    color = label, linetype = label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_linetype_manual(values = .line_types) +
    ggplot2::labs(x = expression(italic(t)),
                  y = expression(italic(f(t))),
                  color = NULL, linetype = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = c(0.85, 0.8),
                   legend.background = ggplot2::element_blank())
}



#' Plot log-EVBS Densities for Varying alpha
#'
#' Reproduces Figure 2(a) of the paper: log-EVBS densities for
#' \eqn{\alpha \in \{0.25, 0.5, 2, 4\}}, \eqn{\eta = 0}, \eqn{\gamma = 0}.
#' Uses \pkg{ggplot2} with the Dark2 colour palette.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{plot_logevbs_gama}}.
#'
#' @examples
#' \donttest{print(plot_logevbs_alpha())}
#'
#' @import ggplot2
#' @export
plot_logevbs_alpha <- function() {
  params <- list(
    list(alpha = 0.25, eta = 0, gama = 0),
    list(alpha = 0.50, eta = 0, gama = 0),
    list(alpha = 2.00, eta = 0, gama = 0),
    list(alpha = 4.00, eta = 0, gama = 0)
  )
  d <- do.call(rbind, lapply(params, function(p) {
    df <- generate_logevbs_data(p$alpha, p$eta, p$gama)
    df$label <- paste0("log-EVBS(", p$alpha, ",", p$eta, ",", p$gama, ")")
    df
  }))
  ggplot2::ggplot(d, ggplot2::aes(x = y_vals, y = fdp,
                                    color = label, linetype = label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_linetype_manual(values = .line_types) +
    ggplot2::labs(x = expression(italic(y)),
                  y = expression(italic(f(y))),
                  color = NULL, linetype = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = c(0.85, 0.8),
                   legend.background = ggplot2::element_blank())
}



#' Plot log-EVBS Densities for Varying gamma
#'
#' Reproduces Figure 2(b) of the paper: log-EVBS densities for
#' \eqn{\gamma \in \{-1.05, -0.5, 0.5, 1.05\}}, \eqn{\alpha = 1},
#' \eqn{\eta = 0}. Uses \pkg{ggplot2} with the Dark2 colour palette.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{plot_logevbs_alpha}}.
#'
#' @examples
#' \donttest{print(plot_logevbs_gama())}
#'
#' @import ggplot2
#' @export
plot_logevbs_gama <- function() {
  params <- list(
    list(alpha = 1, eta = 0, gama = -1.05),
    list(alpha = 1, eta = 0, gama = -0.50),
    list(alpha = 1, eta = 0, gama =  0.50),
    list(alpha = 1, eta = 0, gama =  1.05)
  )
  d <- do.call(rbind, lapply(params, function(p) {
    df <- generate_logevbs_data(p$alpha, p$eta, p$gama)
    df$label <- paste0("log-EVBS(", p$alpha, ",", p$eta, ",", p$gama, ")")
    df
  }))
  ggplot2::ggplot(d, ggplot2::aes(x = y_vals, y = fdp,
                                    color = label, linetype = label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_linetype_manual(values = .line_types) +
    ggplot2::labs(x = expression(italic(y)),
                  y = expression(italic(f(y))),
                  color = NULL, linetype = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = c(0.85, 0.8),
                   legend.background = ggplot2::element_blank())
}
