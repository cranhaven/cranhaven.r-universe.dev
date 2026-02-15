args_env <- new.env()
local(envir = args_env, {
  d_m <- 2L
  d_g <- 5L
  d_b <- 5L
  n_y <- 2L
  d_z <- 2L
  d_x <- 2L
  K <- n_y * d_m

  b_ks <- seq(log(1e-2), log(10), length.out = d_b)
  m_ks <- seq(0        , 10     , length.out = d_m)
  g_ks <- seq(0        , 10     , length.out = d_g)

  omega <- c(-1.68, -1.93, -0.62, -5.11, -0.26)
  B <- structure(c(-0.43, 0.33, 0.04, 0.47, -0.37, -0.88, 0.48, -0.16,
                   -0.06, -0.14), .Dim = c(5L, 2L))
  Psi <- structure(c(5.52, 1.09, 0.24, 0.28, 1.09, 3.52, 0.56, -0.37,
                     0.24, 0.56, 1.85, 0.5, 0.28, -0.37, 0.5, 1.96),
                   .Dim = c(4L, 4L))
  sig <- structure(c(0.02, 0, 0, 0.14), .Dim = c(2L, 2L))
  delta <- c(-0.09, -0.22)
  alpha <- c(-0.4, .5)
  gamma <- structure(c(-0.15, 0.19, 0.06, -0.23), .Dim = c(2L, 2L))

  n_obs <- 2000L
})
