## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  eval = FALSE,
  include = TRUE
)

## ----eval = TRUE, include = TRUE----------------------------------------------
library(lnmixsurv)
library(readr)
library(bayesplot)

color_scheme_set("viridis")

set.seed(25)

data <- simulate_data(
  n = 4000, mixture_components = 3, k = 2,
  percentage_censored = 0.2
)$data |>
  dplyr::rename(y = t, x = cat)

## ----eval = TRUE, include = TRUE----------------------------------------------
start_sequential <- Sys.time()
mod_sequential <- survival_ln_mixture(Surv(y, delta) ~ x, data,
                                      iter = 50, chains = 2,
                                      starting_seed = 5, warmup = 0
)
end_sequential <- Sys.time()

times_sequential <- tibble::tibble(start_sequential = start_sequential,
                                   end_sequential = end_sequential)

## ----eval = TRUE, include = TRUE----------------------------------------------
start_parallel <- Sys.time()
mod_parallel <- survival_ln_mixture(Surv(y, delta) ~ x, data,
                                    iter = 50, chains = 2, cores = 2,
                                    starting_seed = 5, warmup = 0
)
end_parallel <- Sys.time()

times_parallel <- tibble::tibble(start_parallel = start_parallel,
                                 end_parallel = end_parallel)

## ----eval = TRUE--------------------------------------------------------------
mcmc_trace(mod_sequential$posterior, pars = "eta_1")
mcmc_trace(mod_parallel$posterior, pars = "eta_1")

## ----include=FALSE, eval = TRUE-----------------------------------------------
start_parallel <- as.POSIXct(times_parallel$start_parallel)
end_parallel <- as.POSIXct(times_parallel$end_parallel)

start_sequential <- as.POSIXct(times_sequential$start_sequential)
end_sequential <- as.POSIXct(times_sequential$end_sequential)

## ----echo=FALSE, eval = TRUE--------------------------------------------------
print("Sequential")
print(end_sequential - start_sequential)
print("-------------")
print("Parallel")
print(end_parallel - start_parallel)

