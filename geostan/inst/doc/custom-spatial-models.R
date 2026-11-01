## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE, echo = TRUE)

## -----------------------------------------------------------------------------
#  library(geostan)
#  data(georgia)
#  A <- shape2mat(georgia, "B")
#  car_list <- prep_car_data(A, style = "WCAR")
#  
#  # prior distributions (to match the Stan model below)
#  prior_list <- list(intercept = normal(0, 5),
#   	           beta = normal(0, 5),
#  		   sigma = student_t(10, 0, 5)
#  		   )
#  
#  # an auto-model
#  # y = mu + rho * C (y - mu) + error
#  # mu = alpha + beta * .x
#  # y = log(income); x = log(population)
#  # x will be centered: .x = x - mean(x)
#  
#  fit <- stan_car(log(income / 10e3) ~ log(population / 10e3),
#      data = georgia, car = car_list, prior = prior_list, centerx = TRUE)

## -----------------------------------------------------------------------------
#  autonormal_file <- "autonormal.stan"

## -----------------------------------------------------------------------------
#  library(rstan)
#  library(geostan)
#  data(georgia)
#  
#  A <- shape2mat(georgia, "B")
#  car_list <- prep_car_data(A, style = "WCAR")
#  
#  # add data
#  ## (centering covariates improves sampling efficiency)
#  car_list$y <- log(georgia$income / 10e3)
#  car_list$x <- scale(log(georgia$population / 10e3), center = TRUE, scale = FALSE)
#  car_list$k <- ncol(car_list$x)
#  
#  # compile Stan model from file
#  autonormal_file <- "autonormal.stan"
#  car_model <- stan_model(autonormal_file)
#  
#  # sample from model
#  samples <- sampling(car_model, data = car_list)

## -----------------------------------------------------------------------------
#  data(georgia)
#  A <- shape2mat(georgia, "B")
#  car_list <- prep_car_data(A, style = "WCAR")
#  
#  # prior distributions (to match the Stan model below)
#  prior_list <- list(intercept = normal(0, 5),
#   	           beta = normal(0, 5),
#  		   sigma = student_t(10, 0, 5)
#  		   )
#  
#  
#  # Poisson model
#  # y ~ Poisson(pop * exp(mu))
#  # mu = alpha + beta * x + phi
#  # phi ~ CAR(0, Sigma)
#  # y = deaths; x = log(income);
#  # x will be centered: .x = x - mean(x)
#  
#  fit <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + log(income / 1e3),
#      data = georgia,
#      car = car_list,
#      centerx = TRUE,
#      family = poisson()
#      )

## -----------------------------------------------------------------------------
#  car_poisson_file <- "car_poisson.stan"

## -----------------------------------------------------------------------------
#  library(rstan)
#  library(geostan)
#  data(georgia)
#  
#  A <- shape2mat(georgia, "B")
#  car_list <- prep_car_data(A, style = "WCAR")
#  
#  # add data
#  car_list$y <- georgia$deaths.male
#  car_list$const_offset <- log(georgia$pop.at.risk.male)
#  car_list$x <- scale(log(georgia$income / 1e3), center = TRUE, scale = FALSE)
#  car_list$k <- ncol(car_list$x)
#  
#  # compile Stan model from file
#  car_poisson_file <- "car_poisson.stan"
#  car_poisson <- stan_model(car_poisson_file)
#  
#  # sample from model
#  samples <- sampling(car_poisson, data = car_list)

## -----------------------------------------------------------------------------
#  # zero-mean parameterization of the hierarchical CAR model
#  car_list <- prep_car_data(shape2mat(georgia, "B", quiet = TRUE))
#  fit <- stan_car(deaths.male ~ offset(log(pop.at.risk.male)) + log(income / 1e3),
#      data = georgia,
#      car = car_list,
#      centerx = TRUE,
#      family = poisson(),
#      zmp = TRUE
#      )

## -----------------------------------------------------------------------------
#  W <- shape2mat(georgia, "W")
#  fit <- stan_sar(log(income / 1e3) ~ log(population / 1e3),
#                  data = georgia,
#  		type = "SEM",
#                  C = W,
#                  centerx = TRUE,
#                  iter = 1e3)

## -----------------------------------------------------------------------------
#  sar_model_file <- "sar_model.stan"

## -----------------------------------------------------------------------------
#  library(geostan)
#  library(rstan)
#  data(georgia)
#  
#  W <- shape2mat(georgia, "W")
#  sar_list <- prep_sar_data(W)
#  
#  # add data
#  sar_list$y <- log(georgia$income / 1e3)
#  sar_list$x <- scale(log(georgia$population / 1e3), center = TRUE, scale = FALSE)
#  sar_list$k <- ncol(sar_list$x)
#  
#  # compile Stan model from file (SEM)
#  sar_model_file <- "sar_model.stan"
#  sar_model <- stan_model(sar_model_file)
#  
#  # sample from model (SEM)
#  samples <- sampling(sar_model, data = sar_list, iter = 1e3)

