#!/usr/bin/env Rscript
# mkuhn, 2022-03-22
# explore bootstrap distribution for delay parameter and what are best transformations

# command line arguments --------------------------------------------------

suppressPackageStartupMessages(library("R.utils"))
cmdArgs <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  excludeReserved = FALSE,
  excludeEnvVars = TRUE,
  defaults = list(
    slice = 0,
    workers = 6,
    nrep = 100,
    seed = as.integer(Sys.Date())
  )
)


if (any(c('help', 'h') %in% names(cmdArgs))) {
  cat(
    'Run Monte-Carlo simulations with delayed Exponential or Weibull data for a single group setting.\n'
  )
  cat(
    'A confidence interval is constructed for all involved parameters and its properties "width" and "coverage" are calculted.\n'
  )
  cat(
    'Sample size, delay, scale and scale ratio (between the two groups) and shape use different fixed values that are specified within the script.\n'
  )
  cat('Parameter options are:\n')
  cat('  --help\t print this help\n')
  cat(
    '  --slice=\t run only for first scenarios, or drop first scenarios when negative.\n'
  )
  cat(
    '  --seed=\t if given, set random seed at the start of the script. Default is date-dependent. In the past, we used a fixed seed of 12. \n'
  )
  cat(
    '  --workers=\t number of parallel computations using future.callr and future.apply\n'
  )
  cat(
    '  --nrep=\t number of replicated bootstrap data sets to build confidence intervals for\n'
  )
  cat('  --print/-p only print out scenarios and exit\n')
  quit(save = 'no')
}

mySlice <- cmdArgs[["slice"]]
stopifnot(is.numeric(mySlice))
mySlice <- round(mySlice)

myWorkers <- cmdArgs[["workers"]]
stopifnot(is.numeric(myWorkers), length(myWorkers) == 1L, myWorkers >= 1)

myNrep <- cmdArgs[["nrep"]]
stopifnot(is.numeric(myNrep), length(myNrep) == 1L, myNrep >= 1L)

mySeed <- cmdArgs[['seed']]
stopifnot(is.numeric(mySeed), length(mySeed) == 1L, mySeed >= 0L)

myPrint <- isTRUE(any(c('print', 'p') %in% tolower(names(cmdArgs))))


# init -------------------------------------------------------------------
library('tidyr')
library('dplyr')
library('purrr')
library('incubate')
library('tibble')

library('furrr')
library('future.callr')

#library('tictoc')

# setup -------------------------------------------------------------------

if (mySeed > 0L) {
  set.seed(mySeed) ##used to be: 12
}


future::plan(future.callr::callr(workers = ceiling(myWorkers)))

# vary n, delay, rate for exponential samples
simScenarios <- tidyr::expand_grid(
  n = c(12, 20),
  delay = c(0, 5, 10),
  rate = c(0.1, .5, 1, 2),
  R = c(1e2, 1e3, 1e4)
)

if (!dplyr::near(mySlice, 0)) {
  simScenarios <- simScenarios %>% dplyr::slice_head(n = mySlice)
}

if (myPrint) {
  cat('\n\nScenarios selected:\n')
  print(knitr::kable(simScenarios, format = 'pipe', digits = 2))
  cat('\nNbr of scenarios is:', NROW(simScenarios), '\n')
  quit(save = 'no')
}

shift_delay <- c(.00001, .0001, .001, .01, .1, 1, 2, 5, 10, 20, 40)
lbdVct <- seq(-4, 4, .1)

cat('\n', NROW(simScenarios), "scenarios to make simulations for.\n")


# simulation --------------------------------------------------------------

simScenarios <- simScenarios %>%
  rowwise() %>%
  dplyr::mutate(
    # simulate data & fit model
    simdat = list(purrr::map(
      .x = seq_len(myNrep),
      .f = ~ incubate::delay_model(
        x = incubate::rexp_delayed(n = n, delay = n, rate = rate)
      )
    )),

    # generate bs_data for delay
    simbsdat = list(purrr::map2(
      .x = simdat,
      .y = R,
      .f = ~ incubate:::bsDataStep(.x, bs_data = 'parametric', R = .y)[
        1L,
        ,
        drop = TRUE
      ]
    )),
    # box-cox transformation for different delay-shifts
    #simbc = list({retMat <- do.call(what = rbind,
    #                                args = purrr::map(.x = simbsdat,
    #                                                  .f = ~ {
    #                                                    vapply(X = shift_delay,
    #                                                           FUN = \(sh) {
    #                                                             yval <- .x - min(.x) + sh
    #                                                             fm <- stats::lm.fit(x = matrix(1, nrow = length(yval)), y = yval)
    #                                                             fm$y <- yval
    #                                                             bcList <- MASS::boxcox(fm, lambda = lbdVct, plotit = FALSE)
    #                                                             bcMaxInd <- which.max(bcList$y)
    #
    #                                                         bcList$x[bcMaxInd]
    #                                                             # c(shift = sh,
    #                                                             #   bcMaxLbd = bcList$x[bcMaxInd],
    #                                                             #   border = dplyr::case_when(
    #                                                             #     bcMaxInd == 1 ~ -1,
    #                                                             #     bcMaxInd == length(lbdVct) ~ +1,
    #                                                             #     TRUE ~0))
    #                                                           }, FUN.VALUE = numeric(length = 1L)
    #                                                    )}))
    #colnames(retMat) <- shift_delay
    #cbind(runID = seq_len(length.out = NROW(retMat)), retMat) %>%
    #  tibble::as_tibble() %>%
    #  tidyr::pivot_longer(cols = !runID, names_to = 'delay_shift', names_transform = as.numeric, values_to = 'lambda')}),

    # log-displacement transformation
    simld = list(purrr::map_dbl(
      .x = simbsdat,
      .f = ~ {
        yval <- .x - min(.x) # min is probably influenced by R..
        fm <- stats::lm.fit(x = matrix(1, nrow = length(yval)), y = yval)
        fm$y <- yval
        ldList <- MASS::logtrans(
          fm,
          alpha = c(
            0.0001,
            0.001,
            0.01,
            0.1,
            seq.int(0.25, 5, by = .25),
            seq.int(6, 20, by = 2)
          ),
          plotit = FALSE
        )

        ldMaxInd <- which.max(ldList$y)
        ldList$x[ldMaxInd] #+ min(yval)
      }
    )),
    # drop simulated bootstrap distribution of delay
    simbsdat = NULL
  )

cat('Sim scenarios done\n')


# write out results -------------------------------------------------------

cat('We save the output relative to directory ', getwd(), '\n')

DATE_TAG <- format(Sys.Date(), '%Y-%m-%d')
saveRDS(
  simScenarios,
  file = file.path('results', paste0('simRes_bootstrap_', DATE_TAG, '.rds'))
)


future::plan(future::sequential)

# visualization -----------------------------------------------------------

#library('ggplot2')
#p1 <- simScenarios %>% select(!c(simdat)) %>% unnest(simbc) %>%
#  ggplot(mapping = aes(x = factor(delay_shift), y = value, fill = factor(R))) + geom_boxplot()

# per delay_shift produce an own plot (to judge its consistency in lambda-estimate)
# factor(delay * rate) as x
# n facet in rows
# R: facet in cols
