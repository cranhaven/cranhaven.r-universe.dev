#' Geostatistical estimates for function-valued data.
#'
#' \code{geo_fda} finds the ordinary kriging estimate for sptial functional
#' data using the model proposed by Giraldo(2011).
#'
#' @details \code{geo_fda} is similar to model proposed by
#' \cite{giraldo2011ordinary}. The mais difference is we have used
#' gauss-legendre quadrature to estimate the trace-variogram. Using
#' gauss-legendre qudrature gives estimates with smaller mean square error
#' than the trace-variogram estimates from Giraldo(2011).
#'
#' For now, we have used Fourier's series to smooth the time series.
#'
#' @param m_coord a matrix with coordinates (first column is latitude and
#' second column longitude)
#' @param m_data a matrix where each column is a time series in a location
#' @param new_coord a vector with a new coordinate (first column is latitude
#' and second longitude)
#' @param n_quad a scalar with number of quadrature points. Default value
#' \code{nquad = 20}.
#' @param t a vector with points to evaluate from \eqn{-\pi} to \eqn{\pi}.
#' Default \code{t = seq(from = -pi,to = pi,length.out = 1e+3)}.
#' @param m  order of the Fourier polynomial
#'
#' @return a list with three components
#' \describe{
#'   \item{\code{curve}}{estimate curve at \code{t} points}
#'   \item{\code{lambda}}{weights in the linear combination in the functional
#'   kriging}
#'   \item{\code{x}}{points where the curve was evaluated}
#' }
#' @import magrittr stats orthopolynom
#' @export
#'
#' @references
#' Giraldo, R., Delicado, P., & Mateu, J. (2011). Ordinary kriging
#' for function-valued spatial data. \emph{Environmental and Ecological
#' Statistics}, 18(3), 411-426.
#'
#' Giraldo, R., Mateu, J., & Delicado, P. (2012). geofd: an \code{R} package
#' for function-valued geostatistical prediction.
#' \emph{Revista Colombiana de Estadística}, 35(3), 385-407.
#'
#' @seealso \code{\link{coef_fourier}}, \code{\link{fourier_b}}
#'
#'@examples
#'data(canada)
#'
#'y_hat <- geo_fda(canada$m_data, canada$m_coord, canada$ThePas_coord,
#'n_quad = 2)
geo_fda <- function(m_data, m_coord, new_coord, m,
                    n_quad = 20,
                    t = seq(from = -pi, to = pi, length.out = 1e+3)) {
  # order of Fourier Series
  if (missing(m)) m <- ceiling(1 + log2(nrow(m_data)))

  # quadrature points and weigths
  recurrences <- legendre.recurrences(n_quad)
  inner.products <- legendre.inner.products(n_quad)
  quadrature.rules <- function(recurrences, inner.products) {
    np1 <- nrow(recurrences)
    n <- np1 - 1
    rules <- as.list(rep(NULL, n))
    monic.recurrences <- monic.polynomial.recurrences(recurrences)
    matrices <- jacobi.matrices(monic.recurrences)
    matrix.eigens <- lapply(matrices, eigen)
    roots <- polynomial.roots(monic.recurrences)
    h.0 <- inner.products[1]
    for (k in 1:n) {
      values <- matrix.eigens[[k]]$values
      vectors <- matrix.eigens[[k]]$vectors
      x <- values
      w <- rep(0, k)
      for (j in 1:k) {
        v.j <- vectors[1, j]
        w[j] <- h.0 * v.j * v.j
      }
      rule <- data.frame(cbind(x, w))
      names(rule) <- c("x", "w")
      rules[[k]] <- rule
    }
    return(rules)
  }
  quad <- quadrature.rules(recurrences, inner.products)[[n_quad]]
  x <- quad$x
  weight <- quad$w
  z_to_x <- function(a, b, z)  (((b - a) * z + b + a) / 2) %>%
    return()
  z <- z_to_x(-pi, pi, x)

  # number of locations
  n_loc <- nrow(m_coord)

  # exponential model
  s2 <- numeric(n_quad)
  v_phi <- numeric(n_quad)
  for (l in 1:n_quad) {
    u <- z[l]
    v_item <- 1:n_loc
    y <- matrix(v_item, ncol = 1) %>%
      apply(1, function(item) {
        m_data[, item] %>%
          as.vector() %>%
          coef_fourier(m) %>%
          fourier_b(u) %>%
          return()
      }) %>%
      as.vector()

    fit <- geo_model(y, m_coord[v_item, ])
    s2[l] <- fit$sigmasq
    v_phi[l] <- fit$phi + 1e-6
  }


  # coefficient matrix
  h <- m_coord %>%
    dist() %>%
    as.matrix()
  m_a <- matrix(0, nrow = nrow(h), ncol = ncol(h))
  for (l in 1:n_quad) m_a <- m_a +
    weight[l] * s2[l] * (1 - exp(-h / v_phi[l]))
  m_a <- cbind(m_a, rep(1, nrow(m_a)))
  m_a <- rbind(m_a, rep(1, ncol(m_a)))
  m_a[nrow(m_a), ncol(m_a)] <- 0

  #independent vector
  h <- rbind(new_coord,
             m_coord) %>%
    dist() %>%
    as.matrix()
  h <- h[2:(n_loc + 1), 1] %>% matrix(ncol = 1)
  b <- matrix(0, nrow = nrow(h), ncol = ncol(h))
  for (l in 1 : n_quad) b <- b +
    weight[l] * s2[l] * (1 - exp(-h / v_phi[l]))
  b <- rbind(b, 1)

  #solving the linear system
  lambda <- solve(m_a, b)

  #doing the estimates
  m <- matrix(1:n_loc, ncol = 1) %>%
    apply(1, function(item) {
      m_data[, item] %>%
        coef_fourier() %>%
        fourier_b(t) %>%
        return()
    })
  y_est <- length(t) %>%
    numeric()
  for (k in 1:n_loc) y_est <- y_est +
    lambda[k] * m[, k]

  #output
  list(curve = y_est, lambda = lambda[1:n_loc], x = t)
}
