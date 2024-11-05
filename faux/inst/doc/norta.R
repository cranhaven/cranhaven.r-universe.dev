## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 5,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(faux)
library(ggplot2)
library(ggExtra)
library(patchwork)

## -----------------------------------------------------------------------------
dat <- rmulti()

get_params(dat)

## -----------------------------------------------------------------------------
dat <- rmulti(n = 200, r = 0.5,
              empirical = TRUE, 
              as.matrix = FALSE)

get_params(dat)

## -----------------------------------------------------------------------------
dat <- rmulti(n = 1000, 
              dist = c(uniform_var = "unif",
                       poisson_var = "pois"),
              params = list(uniform_var = c(min = 0, max = 100),
                            poisson_var = c(lambda = 3)),
              r = 0.5)

get_params(dat)

## ---- echo = FALSE, fig.width = 10, fig.height = 10---------------------------
p <- ggplot(dat, aes(uniform_var, poisson_var)) +
  geom_point() +
  geom_smooth()
ggMarginal(p, type = "histogram")

## -----------------------------------------------------------------------------
dat <- rmulti(
  n = 1000,
  dist = c(N = "norm",
           T = "truncnorm",
           L = "likert"),
  params = list(
    N = list(mean = 100, sd = 10),
    T = list(a = 1, b = 7, mean = 3.5, sd = 2.1),
    L = list(prob = c(`much less` = .10, 
                      `less`      = .20, 
                      `equal`     = .35, 
                      `more`      = .25, 
                      `much more` = .10))
  ),
  r = c(-0.5, -0.6, 0.7)
)

# convert likert-scale variable to integer
dat$L <- as.integer(dat$L)
get_params(dat)

## ---- echo = FALSE------------------------------------------------------------
nt <- ggplot(dat, aes(N, T)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Normal",
       y = "Truncated Normal") +
  scale_y_continuous(breaks = 1:7)
ntm <- ggMarginal(nt, type = "histogram")

nl <- ggplot(dat, aes(N, L)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Normal",
       y = "Likert")
nlm <- ggMarginal(nl, type = "histogram")

tl <- ggplot(dat, aes(T, L)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Truncated Normal",
       y = "Likert") +
  scale_x_continuous(breaks = 1:7)
tlm <- ggMarginal(tl, type = "histogram")

wrap_elements(ntm) + 
  plot_spacer() +
  wrap_elements(nlm) +
  wrap_elements(tlm)
  

## ---- error = TRUE------------------------------------------------------------
dat <- rmulti(
  dist = c(A = "binom", B = "pois", C = "norm"), 
  params = list(A = list(size = 1, prob = 0.5),
                B = list(lambda = 3),
                C = list(mean = 100, sd = 10)),
  r = c(0.8, 0.9, 0.5)
)

## -----------------------------------------------------------------------------
fh_bounds(dist1 = "truncnorm",
          dist2 = "beta",
          params1 = list(a = 0, b = 10, mean = 5, sd = 3),
          params2 = list(shape1 = 1, shape2 = 5))

## -----------------------------------------------------------------------------
adjusted_r <- convert_r(
  target_r = 0.75,
  dist1 = "truncnorm",
  dist2 = "binom",
  params1 = list(a = 0, b = 10, mean = 5, sd = 3),
  params2 = list(size = 1, prob = 0.5)
)

adjusted_r

## -----------------------------------------------------------------------------
# simulate multivariate normal 
dat <- rnorm_multi(n = 1000, 
                   varnames = c("N1", "N2"), 
                   r = adjusted_r, 
                   empirical = TRUE)

# convert to target distributions
dat$T1 <- norm2trunc(dat$N1, 
                     min = 0, max = 10, 
                     mu = 5, sd = 3, 
                     x_mu = 0, x_sd = 1)
dat$B2 = norm2binom(dat$N2, 
                    size = 1, prob = 0.5,
                    mu = 0, sd = 1)

# check
get_params(dat)

## ---- echo = FALSE------------------------------------------------------------
p <- ggplot(dat, aes(T1, B2)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Truncated Normal",
       y = "Binomial") +
  scale_y_continuous(breaks = 0:1)
ggMarginal(p, type = "histogram")

