#!/usr/bin/env Rscript
# mkuhn, 2023-04-05
# Monte Carlo simulation to estimate the median weights W1, W2 and W3 for the weighted MLE approach (Cousineau, 2009)
#
# the weight estimates (from the Monte Carlo simulation) are stored as list in file `MLEw_mcs.rds`.
# These weight estimates are the basis for weight approximation functions,
# which are stored as internal data of incubate package (see `data-raw/data_internal.R`).
# For this to work, the results file `MLEw_mcs.rds` is expected to be also in directory `data-raw/`.
##

# init -----

suppressPackageStartupMessages(library("future"))
suppressPackageStartupMessages(library("R.utils"))

message(
  "Script to prepare MLE weights to be stored as internal data of incubate package!"
)
message("incubate package installed is: ", packageVersion("incubate"))


TODAY <- Sys.Date()
NOW <- Sys.time()
DEBUG <- FALSE


# command line arguments -----
cmdArgs <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  excludeReserved = FALSE,
  excludeEnvVars = TRUE,
  defaults = list(
    resultsDir = getwd(),
    seed = as.integer(paste0(
      as.integer(TODAY) %% 73,
      format(NOW, format = "%H%M%S")
    )),
    # at most 97 cores
    workers = min(97L, future::availableCores(methods = "system", omit = 4)),
    # mcnrep as string, so user can rely on parse_number!
    mcnrep = "1001"
  )
)


if (any(c('help', 'h') %in% names(cmdArgs))) {
  cat(
    'Run Monte Carlo simulations to estimate the median weights W1, W2 and W3 for weighted maximum likelihood approach (MLEw)\n'
  )
  cat('Reference: Cousineau (2009).\n')
  cat('  --help\t print this help\n')
  cat(
    '  --seed=\t if given, set random seed at the start of the script. Default is date-dependent.\n'
  )
  cat(
    '  --workers=\t number of parallel computations using `future.callr`. The only level of parallelization is for n, the different numbers of observations (and scale for W3).\n'
  )
  cat(
    '  --mcnrep=\t size of Monte Carlo study: number of replications which are then aggregated. Default value is 1001.\n'
  )
  cat(
    '  --resultsDir=\t directory where to save the result files. Defaults to the directory where Rscript is executed.\n'
  )
  cat('  --overwrite/--force\t overwrite data file when it already exists?\n')
  quit(save = 'no')
}

message("Start at ", toString(NOW))

library("readr") #parse_number
library("rlang")
library("tibble")
library("tidyr", warn.conflicts = FALSE)
library("dplyr", warn.conflicts = FALSE)
library("purrr", warn.conflicts = FALSE)
library("matrixStats")

library("future.callr")
library("furrr")

library("ggplot2")
library("cowplot")
theme_set(theme_cowplot(font_size = 15))
library("patchwork")


mySeed <- cmdArgs[["seed"]]
stopifnot(is.numeric(mySeed), length(mySeed) == 1L, mySeed >= 0L)

myWorkers <- cmdArgs[["workers"]]
stopifnot(
  is.numeric(myWorkers),
  length(myWorkers) == 1L,
  is.finite(myWorkers),
  myWorkers >= 1L
)

myMCNrep <- readr::parse_number(cmdArgs[["mcnrep"]])
stopifnot(is.numeric(myMCNrep), length(myMCNrep) == 1L, myMCNrep >= 1L)

myResultsDir <- cmdArgs[["resultsDir"]]
stopifnot(
  is.character(myResultsDir),
  dir.exists(myResultsDir),
  # check read & write permission (first octal information)
  (file.mode(myResultsDir) %>%
    as.character() %>%
    substr(1, 1) %>%
    as.octmode() &
    6) ==
    '6'
)
myOverwrite <- isTRUE(any(
  c("overwrite", "ow", "force") %in% tolower(names(cmdArgs))
))

if (DEBUG) {
  cat(paste(names(cmdArgs), cmdArgs, sep = ": ", collapse = "***"), "\n")
  cat("Overwrite: ", myOverwrite, "\n")
}

# check results file: fail early
resFile <- file.path(myResultsDir, "MLEw_mcs.rds")
if (file.exists(resFile) && !myOverwrite) {
  stop(
    "File ",
    resFile,
    "already exists! You would need to set overwrite flag.",
    call. = FALSE
  )
}


# simulation settings -----

if (mySeed > 0L) {
  set.seed(mySeed)
}
if (myWorkers > 1L) {
  future::plan(strategy = future.callr::callr, workers = myWorkers)
}


# distribution of W1 is Gamma with shape n and scale 1/n
nObs_vctr <- c(
  1:50,
  55,
  60,
  65,
  70,
  75,
  80,
  90,
  100,
  125,
  150,
  200,
  250,
  500,
  750,
  1000,
  1500,
  2000,
  2500,
  3000,
  4000,
  5000,
  7500,
  10000
) |>
  unique()
#shape relevant for W3
shape_vctr <- c(
  .001,
  .005,
  .01,
  .05,
  .1,
  .2,
  .25,
  .5,
  .75,
  1,
  1.25,
  1.5,
  1.75,
  seq.int(2, 8, by = .5),
  9:15,
  20,
  25,
  30,
  40,
  50,
  75,
  100
) |>
  unique()


# aggregation function to get from sampling distribution to the weight estimate
# Cousineau (2009) suggests to use the median!
aggFun <- stats::median
isMedian <- TRUE
stopifnot(is.function(aggFun), "na.rm" %in% formalArgs(aggFun))


# simulate W1 ------------------------

message("Start simulation for W1")

# when 3-param Weibull holds then the mean of z values (where z is Exp(1)) are
# gamma-distributed with parameter shape n and scale 1/n (=rate n)
# hence, the mean of W1 is 1 (independently of n)
W1_mcs <- furrr::future_map_dbl(
  .x = rlang::set_names(nObs_vctr),
  .f = ~ aggFun(stats::rgamma(n = myMCNrep, shape = .x, scale = 1 / .x)),
  .options = furrr_options(seed = TRUE)
)

if (nObs_vctr[[1L]] == 1) {
  if (!dplyr::near(W1_mcs[[1]], log(2), tol = 1e-3)) {
    warning(
      "For n=1, Monte Carlo simulation for W1 deviates more than 1e-3 from the true value ln(2)! (We use ln(2) instead, anyhow.)",
      call. = FALSE
    )
  }
  W1_mcs[[1L]] <- log(2)
} else {
  stop("n=1 has not run!!")
}


# simulate W2 -----------------------------------------

message("Start simulation for W2")
W2_mcs <- furrr::future_map_dbl(
  .x = rlang::set_names(nObs_vctr),
  .f = ~ aggFun(replicate(n = myMCNrep, expr = {
    z <- stats::rexp(n = .x)
    sum(z * log(z)) / sum(z) - mean(log(z))
  })),
  .options = furrr_options(seed = TRUE)
)

if (nObs_vctr[[1L]] == 1) {
  if (abs(W2_mcs[[1]]) > 1e-5) {
    warning("W2 Monte Carlo simulation for n=1 not close to zer0!. We use 0!")
  }
  W2_mcs[[1L]] <- 0
} else {
  stop("n=1 has not run!!")
}

# gather results for W1 & W2 in a combined dataframe
W12_mcs_df <- dplyr::inner_join(
  x = tibble::enframe(W1_mcs, name = "nObs", value = "W1"),
  y = tibble::enframe(W2_mcs, name = "nObs", value = "W2"),
  by = join_by(nObs)
) |>
  dplyr::mutate(nObs = as.integer(nObs))


# simulate W3 --------------------------------------------------------

message("Start simulation for W3")
#currently, W3 is using simulation on log-transform, aggregates and then backtransform via exp.
#+this works for median but for instance not for mean!
stopifnot(
  `aggFun for W3 should be median (as we internally use monotone transformations)` = isMedian
)

# W3 needs corresponding W1
stopifnot(
  `W1_mcs not found` = exists("W1_mcs"),
  `W1_mcs does not match nObs` = length(W1_mcs) == length(nObs_vctr)
)

W3_mcs_df <- tidyr::expand_grid(
  nObs = as.integer(nObs_vctr),
  shape = shape_vctr
) |>
  dplyr::mutate(
    W3 = furrr::future_map2_dbl(
      .x = nObs,
      .y = shape,
      .f = ~ exp(aggFun(
        replicate(n = myMCNrep, expr = {
          z <- stats::rexp(n = .x)
          res <- NA_real_
          # on original (=non-log) scale
          # W1_mcs[as.character(.x)] * sum(z**(-1/.y))/sum(z**((.y-1)/.y))
          try(
            expr = res <- log(W1_mcs[as.character(.x)]) +
              matrixStats::logSumExp(lx = -1 / .y * log(z)) -
              matrixStats::logSumExp(lx = (.y - 1) / .y * log(z)),
            silent = TRUE
          )
          res
        }),
        na.rm = TRUE
      )),
      .options = furrr_options(seed = TRUE)
    )
  )


# check results -----------------------------------------------------------

# W1 is close to be monotonically increasing
if (min(diff(W12_mcs_df$W1)) > -1e-5) {
  warning("W1 not monotonely increasing!")
}
if (min(diff(W12_mcs_df$W2)) > -1e-5) {
  warning("W2 not monotonely increasing!")
}


# save & exit ------

# Monte Carlo simulation results for W1, W2 and W3
MLEw_mcs <- list(
  W12 = W12_mcs_df,
  W3 = W3_mcs_df,
  settings = list(
    date = TODAY,
    host = Sys.info()[["nodename"]],
    R.version = R.version.string,
    incubate = paste("installed: ", utils::packageVersion("incubate")),
    seed = mySeed,
    aggFun = aggFun,
    mcnrep = myMCNrep
  )
)

# clean up
try(expr = rm(W12_mcs_df, W3_mcs_df), silent = FALSE)

# write out results of Monte Carlo simulation
saveRDS(MLEw_mcs, file = resFile)

message(
  "Saved Monte Carlo simulation results for MLE weights to file: ",
  resFile
)


# tear-down
future::plan(future::sequential())

# output the latest warnings:
message("\n\n+++\nThese are warnings from the script:\n+++\n")
warnings()

message("~~ Fine ~~\n")
message("Finished script at ", toString(Sys.time()))
