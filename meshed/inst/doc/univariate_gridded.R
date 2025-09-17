## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = '40%', fig.align="center",
  message=FALSE
)

## -----------------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(meshed)
set.seed(2021)

SS <- 50 # coord values for jth dimension 
dd <- 2 # spatial dimension
n <- SS^2 # number of locations
p <- 3 # number of covariates

xlocs <- seq(0, 1, length.out=SS)
coords <- expand.grid(list(xlocs, xlocs))

sigmasq <- 1.5
nu <- 0.5
phi <- 10
tausq <- .1

# covariance at coordinates
CC <- sigmasq * exp(-phi * as.matrix(dist(coords)))
# cholesky of covariance matrix
LC <- t(chol(CC))
# spatial latent effects are a GP
w <- LC %*% rnorm(n)
# measurement errors
eps <- tausq^.5 * rnorm(n)

# covariates and coefficients 
X <- matrix(rnorm(n*p), ncol=p)
Beta <- matrix(rnorm(p), ncol=1)

# univariate outcome, fully observed
y_full <- X %*% Beta + w + eps

# now introduce some NA values in the outcomes
y <- y_full %>% matrix(ncol=1)
y[sample(1:n, n/5, replace=FALSE), ] <- NA

simdata <- coords %>%
  cbind(data.frame(Outcome_full= y_full, 
                   Outcome_obs = y,
                   w = w)) 

simdata %>% ggplot(aes(Var1, Var2, fill=w)) +
    geom_raster() + 
    scale_fill_viridis_c() +
  theme_minimal() + ggtitle("w: Latent GP")
simdata %>% ggplot(aes(Var1, Var2, fill=y)) +
    geom_raster() + 
    scale_fill_viridis_c() + 
  theme_minimal() + ggtitle("y: Observed outcomes")

## -----------------------------------------------------------------------------
mcmc_keep <- 200 # too small! this is just a vignette.
mcmc_burn <- 400
mcmc_thin <- 2

mesh_total_time <- system.time({
  meshout <- spmeshed(y, X, coords,
                      #axis_partition=c(10,10), #same as block_size=25
                      block_size = 25,
                      n_samples = mcmc_keep, n_burn = mcmc_burn, n_thin = mcmc_thin, 
                      n_threads = 2,
                      verbose = 0,
                      prior=list(phi=c(1,30))
  )})

## -----------------------------------------------------------------------------
summary(meshout$lambda_mcmc[1,1,]^2)
summary(meshout$theta_mcmc[1,1,])
summary(meshout$tausq_mcmc[1,])
summary(meshout$beta_mcmc[1,1,])

## -----------------------------------------------------------------------------
# process means
wmesh <- data.frame(w_mgp = meshout$w_mcmc %>% summary_list_mean())
# predictions
ymesh <- data.frame(y_mgp = meshout$yhat_mcmc %>% summary_list_mean())

outdf <- meshout$coordsdata %>% 
  cbind(wmesh, ymesh) %>%
  left_join(simdata)

outdf %>% 
    ggplot(aes(Var1, Var2, fill=w_mgp)) +
    geom_raster() +
    scale_fill_viridis_c() +
    theme_minimal() + ggtitle("w: recovered")

outdf %>% 
    ggplot(aes(Var1, Var2, fill=y_mgp)) +
    geom_raster() +
    scale_fill_viridis_c() +
    theme_minimal() + ggtitle("y: predictions")

