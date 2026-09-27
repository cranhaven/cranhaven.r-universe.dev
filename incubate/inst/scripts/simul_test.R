#!/usr/bin/env Rscript
# Evaluate test for differences in delayed exponential or Weibull setting
#
# Test delay parameter

cat(
  "\nMC-simulations around statistical significance tests for difference in delay parameters.\n"
)

# init -----

library("incubate")
version_inc <- packageVersion("incubate")
# minimal version check:
#+ 0.7.6 for GOF-Pvalues for restricted & unrestricted model: e.g. gof_mo0 (was gof_mo) and gof_mo1 (new)
#+ 0.9.8 for names for P-values have changed: boot => bootstrap, gof_mo0 => moran, etc
#+ 1.1.9.9000 script is developed as part of the incubate package (not separate as part of the MS)
#+ 1.1.9.9014 ties='density' as default now also for tests
#+ 1.1.9.9016 avoid attributes, use transform() for Pearson/AD GOF tests
#+ 1.3.0.9025: rename logrank P-values to logrank and logrank_pp to avoid confusion with likelihood ratio tests (LRT)
#+ 1.3.0.9037: allow profiling for MPSE and all MLE-methods, at least with single group..
#+ 1.3.0.9055: support random right-censoring in rexp_delayed() and rweib_delayed()
#+ 1.3.0.9077: criterion updated
stopifnot(version_inc >= "1.3.0.9077")
cat('incubate package version: ', toString(version_inc), '\n')

library("tibble")
library("dplyr", warn.conflicts = FALSE)
stopifnot(packageVersion("dplyr") > "1.0.10")
library("purrr", warn.conflicts = FALSE)
library("tidyr", warn.conflicts = FALSE)
suppressPackageStartupMessages(library("R.utils"))


# capture date/time for seed and timestamp
TODAY <- Sys.Date()
NOW <- Sys.time()
DATETIME_TAG <- format(NOW, format = "%Y-%m-%d-%Hh%Mm%Ss")
SEED_DEFAULT <- paste0(as.integer(TODAY), format(NOW, format = "%H%M")) %>%
  as.integer()

stopifnot(
  is.integer(SEED_DEFAULT),
  length(SEED_DEFAULT) == 1,
  SEED_DEFAULT > 11111
)


# command line arguments -----
cmdArgs <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  excludeReserved = FALSE,
  excludeEnvVars = TRUE,
  defaults = list(
    # simulation settings
    dist = "exponential",
    # assumed model for estimation and testing
    model = "exponential",
    scenario = "MS",
    R = 150,
    mcnrep = 100,
    n = -1,
    # technical settings
    resultsDir = getwd(),
    slice = 0,
    seed = SEED_DEFAULT,
    chnkSize = 0,
    workers = 3
  )
)


if (any(c('help', 'h') %in% names(cmdArgs))) {
  cat(
    'Run Monte-Carlo simulations with delayed exponential or Weibull data in a two group setting.\n'
  )
  cat(
    'A test for difference in delay (and sometimes delay+rate) is performed.\n'
  )
  cat(
    'Sample size, delay, scale and scale ratio (between the two groups) and shape use different fixed values (see code in this script).\n'
  )
  cat(
    'Command line parameter options allow to adjust what this script actually does:\n'
  )
  cat('  --help\t print this help\n')
  cat('  --print\t show scenarios to simulate and exit.\n')
  cat(
    '  --resultsDir=\t specify the directory where to put the result files. Defaults to the directory where Rscript is executed.\n'
  )
  cat(
    '  --dist=\t specify distribution that governs the data generation. Default is the exponential distribution.\n'
  )
  cat(
    '  --scenario=\t with respect to the delay in both groups, choose a scenario for the simulation:\n\t\t\tDELAYEQ = no difference in delay,\n\t\t\tDELAYGT = 2nd group y with bigger delay.\n\t\t\tMS = only relevant scenarios shown in manuscript (default)\n\t\t\tALL = all cases\n'
  )
  cat(
    '  --model=\t specify distribution model assumed for analysis. Default is to run exponential model. Say "all" for all delay models (exponential & Weibull).\n'
  )
  cat(
    '  --n=\t\t sample size number to use in the simulation. By default (n=-1) only smallest sample size is used. n=0 will use all forseen values of n.\n'
  )
  cat('  --scaleSimple\t use only standard value for scale and scale-ratio\n')
  cat('  --dropMLEw\t drop MLEw method\n')
  cat('. --doGOF\t also perform goodness-of-fit tests\n')
  cat(
    '  --cens\t apply also random right-censoring during the simulation study\n'
  )
  cat(
    '  --slice=\t if given, pick only this number of first scenarios for simulations. If negative, scenarios taken from the tail.\n'
  )
  cat(
    '  --seed=\t if given, set random seed at the start of the script. Default is date-dependent.\n'
  )
  cat(
    '  --chnkSize=\t chunk size to write out results having processed so many scenarios. Default is no chunking (=0).\n'
  )
  cat(
    '  --workers=\t number of parallel computations using `future.callr` and `future.apply`. The only level of parallelization is across the MC-replications for each simulation setting.\n'
  )
  cat(
    '  --R=\t\t number of samples within parametric bootstrap test: it determines the resolution for our P-value, e.g.,\n\t\t R=100 will allow for P-values at per-cent resolution\n'
  )
  cat(
    '  --mcnrep=\t size of Monte-Carlo study: it is the number of replicated bootstrap data sets on which statistical tests are done.\n'
  )
  quit(save = 'no')
}

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

myDist <- cmdArgs[["dist"]]
stopifnot(is.character(myDist), length(myDist) == 1L, nzchar(myDist))
myDist <- match.arg(
  arg = tolower(myDist),
  choices = c("exponential", "weibull")
)
isExponDat <- isTRUE(myDist == "exponential")
stopifnot(isExponDat || isTRUE(myDist == "weibull"))

myModel <- cmdArgs[["model"]]
stopifnot(is.character(myModel), length(myModel) == 1L, nzchar(myModel))
myModel <- match.arg(
  arg = tolower(myModel),
  choices = c("all", "exponential", "weibull")
)

myWorkers <- cmdArgs[["workers"]]
stopifnot(is.numeric(myWorkers), length(myWorkers) == 1L, myWorkers >= 1L)
USE_FUTURE <- myWorkers > 1L

myChnkSize <- cmdArgs[["chnkSize"]]
stopifnot(is.numeric(myChnkSize), length(myChnkSize) == 1L)

myR <- cmdArgs[["R"]]
stopifnot(is.numeric(myR), length(myR) == 1L, myR >= 1L)

myMCNrep <- cmdArgs[["mcnrep"]]
stopifnot(is.numeric(myMCNrep), length(myMCNrep) == 1L, myMCNrep >= 1L)

mySlice <- cmdArgs[["slice"]]
stopifnot(!is.null(mySlice), is.numeric(mySlice), length(mySlice) == 1L)
mySlice <- ceiling(mySlice)

mySeed <- cmdArgs[["seed"]]
stopifnot(is.numeric(mySeed), length(mySeed) == 1L, mySeed >= 0L)

myScenario <- cmdArgs[["scenario"]]
stopifnot(
  !is.null(myScenario),
  is.character(myScenario),
  length(myScenario) == 1L,
  nzchar(myScenario)
)
myScenario <- match.arg(
  arg = toupper(myScenario),
  choices = c("DELAYEQ", "DELAYGT", "MS", "ALL")
)

myN <- cmdArgs[["n"]]
stopifnot(!is.null(myN), is.numeric(myN), length(myN) == 1L)
myN <- ceiling(myN)

cmdArgsLo <- tolower(names(cmdArgs))
myPrint <- isTRUE(any(c("print", "p") %in% cmdArgsLo))
myDropMLEw <- isTRUE(any("dropmlew" %in% cmdArgsLo))
myDoGOF <- isTRUE(any("dogof" %in% cmdArgsLo))
myScaleSimple <- isTRUE(any(c("scalesimple", "scale", "scales") %in% cmdArgsLo))
myCens <- isTRUE(any(c("cens", "censoring") %in% cmdArgsLo))


# set up simulation setting -----

if (mySeed > 0L) {
  set.seed(mySeed)
  cat("Set seed to ", mySeed, "\n")
}

# choose the sample sizes
nVctr <- if (myN > 0) myN else c(11, 15, 20, 50) ## 100  8, 12, 20, 75

simSetting <- tidyr::expand_grid(
  n_x = nVctr,
  delay_x = 5,
  delay_y = c(5, 7, 9, 11, 13, 15), #, 20),
  scale_x = c(5, 10), #c(1, 2, 5),
  scale_ratio = c(2, 1, .5),
  # shape values according to distribution
  #+effectively filter for distribution
  shape = if (isExponDat) 1 else c(.5, 2),
  cens = c(0, 0.1, 0.2, 0.3)
)

# avoid duplicates:
# by convention, group y is not less delayed than group x
simSetting <- simSetting %>%
  # symmetry
  dplyr::filter(delay_y >= delay_x) %>%
  # enough expected number of observations
  dplyr::filter(cens >= 0, cens < 1, n_x * (1 - cens) > 5) %>%
  # use equally sized groups
  dplyr::mutate(n_y = n_x, .after = n_x)

# default: no censoring (cens = 0)
if (!myCens) {
  simSetting <- simSetting %>%
    dplyr::slice_min(cens)
}

# default is to use only the smallest sample size
# (this n is typically used in presentations as it gives nice power curves for chosen difference difference in delay)
if (myN < 0) {
  simSetting <- simSetting %>%
    dplyr::slice_min(n_x)
}

if (myScaleSimple) {
  simSetting <- simSetting %>%
    dplyr::filter(dplyr::near(scale_x, 10), dplyr::near(scale_ratio, 1))
}


# filter for target scenario!
# use all capital letters (see definition of myScenario)
simSetting <- switch(
  myScenario,
  DELAYEQ = {
    simSetting %>%
      dplyr::filter(dplyr::near(delay_x, delay_y))
    # # for both distributions:
    # dplyr::near(scale_x, 10), dplyr::near(scale_ratio, 1))
  },

  DELAYGT = {
    simSetting %>%
      dplyr::filter(delay_y > delay_x + 1e-11)
  },

  MS = {
    #filter only relevant scale_ratio combinations
    # for exponential:
    # [G1] scale_x = 10 & scale_ratio = 1  (rate_x = .1, rate_ratio = 1)
    # [G2] scale_x =  5 & scale_ratio = 1  (rate_x = .2, rate_ratio = 1)
    # [G3] scale_x =  5 & scale_ratio =  2 [i.e., scale_y = 10]  (rate_x = .2, rate_ratio = .5 [i.e., rate_y = .1])
    # [G4] scale_x = 10 & scale_ratio = .5 [i.e., scale_y =  5]  (rate_x = .1, rate_ratio = 2  [i.e., rate_y = .2])
    # for weibull:
    # dd=0, k=.5|2, scale_x = 10 & scale_ratio = 1
    # dd=5, k=.5|2, scale_x = 10 & scale_ratio = 1

    # filter based on scale parameters
    # this contains the cases which are needed in the manuscript
    local({
      simFilterMS <- if (isExponDat) {
        simSetting %>%
          dplyr::filter(
            dplyr::near(scale_ratio, 1) |
              (dplyr::near(scale_x, 5) & dplyr::near(scale_ratio, 2)) |
              (dplyr::near(scale_x, 10) & dplyr::near(scale_ratio, .5))
          )
      } else {
        tibble::tibble(scale_x = 10, scale_ratio = 1)
      }

      simSetting %>%
        dplyr::semi_join(y = simFilterMS, by = colnames(simFilterMS))
    })
  },

  # ALL = no-op, keep everything! (also unused border cases)
  ALL = {
    simSetting
  },
  stop("Unknown target!", call. = FALSE)
)


# slicing in simulation settings (from head or from tail depending on sign)
if (!dplyr::near(mySlice, 0)) {
  simSetting <- local({
    sliceF <- if (mySlice > 0) dplyr::slice_head else dplyr::slice_tail

    simSetting %>%
      sliceF(n = abs(mySlice))
  })
} #fi mySlice

if (myPrint) {
  print(knitr::kable(simSetting, format = "pipe", digits = 2))
  cat("\n")
  cat(NROW(simSetting), " simulation scenarios in total.\n")
  cat("Each scenario is covered by ", myMCNrep, "MC-data replications.\n")
  if (myDropMLEw) {
    cat("We don't do MLEw.\n")
  }
  cat(
    'Bootstrap tests with R=',
    myR,
    'parametric bootstrap samples (P-value resolution).\n'
  )
  if (mySeed > 0) {
    cat('Seed was set initially to', mySeed, '\n')
  } else {
    cat('Seed was **not** set!\n')
  }
  cat("Results directory is ", myResultsDir, "\n")

  quit(save = "no")
} #fi myPrint


# set up parallel computing ----
if (USE_FUTURE) {
  library("future.callr")
  library("future.apply")

  future::plan(strategy = future.callr::callr, workers = myWorkers)
  # two level future
  # future::plan(list(
  #   tweak(future.callr::callr, workers = 2L),
  #   tweak(multicore, workers = 4L)
  # ))
} #fi USE_FUTURE


# functions -----

#' Run Monte-Carlo simulations to test difference in delay using an exponential model for a given simulation setting
#'
#' A fixed set of estimation methods are used.
#' Uses parallel computation (future_replicate) to go through the (=nrep) MC-simulations.
#' Each bootstrap test is also future-aware (and would pick up a nested future-plan setting)
#' @param DGPsetting numeric. a row from `simSetting`. It encodes parameters that specify the data generating process for both groups
#' @param dropMLEw logical. Should we drop MLEw?
#' @return dataframe. P-values in the different Monte-Carlo runs.
doMCSim <- function(DGPsetting, dropMLEw = FALSE) {
  # settings from the environment:
  stopifnot(exists("myMCNrep"), exists("myR"))
  stopifnot(exists("isExponDat"), is.logical(isExponDat))

  stopifnot(is.numeric(DGPsetting), length(DGPsetting) == 8L)
  stopifnot(is.logical(dropMLEw), length(dropMLEw) == 1L)
  dropMLEw <- isTRUE(dropMLEw)

  n_x <- DGPsetting[[1]]
  n_y <- DGPsetting[[2]]
  delay_x <- DGPsetting[[3]]
  delay_y <- DGPsetting[[4]]

  scale_x <- DGPsetting[[5]]
  scale_ratio <- DGPsetting[[6]]
  shape <- DGPsetting[[7]]
  cens <- DGPsetting[[8]]

  # do we test for parameters combined?
  testParamCombined <- scale_ratio != 1
  scale_y <- scale_x * scale_ratio

  # different estimation methods
  estimMethods <- tidyr::expand_grid(
    method = c(c("MPSE", "MLEn", "MLEc"), if (!dropMLEw) "MLEw"),
    profiled = c(FALSE, TRUE),
    R = as.integer(myR),
    model = if (myModel == "all") c("exponential", "weibull") else myModel
  ) %>%
    # all MLE-methods use only profiled variant, MPSE uses both, profiled & unprofiled
    dplyr::filter(method == 'MPSE' | profiled) %>%
    dplyr::rowwise()
  #cat("estimMethods contains ", paste(unique(estimMethods$method), collapse = ", "), "\n")

  # run MC-replicates for each estimation method in turn
  #+for the specified data generating process setting (`DGPsetting`)
  testDiffList <- future.apply::future_replicate(
    n = myMCNrep,
    future.packages = c("dplyr", "purrr", "incubate", if (cens > 0) "survival"),
    future.seed = TRUE,
    expr = {
      # generate data
      x <- y <- 1 #dummy init
      if (isExponDat) {
        stopifnot(dplyr::near(shape, 1L))
        x <- rexp_delayed(
          n = n_x,
          delay1 = delay_x,
          rate1 = 1 / scale_x,
          cens = cens
        )
        y <- rexp_delayed(
          n = n_y,
          delay1 = delay_y,
          rate1 = 1 / scale_y,
          cens = cens
        )
      } else {
        # weibull, same shape for both groups x and y
        x <- rweib_delayed(
          n = n_x,
          delay1 = delay_x,
          scale1 = scale_x,
          shape1 = shape,
          cens = cens
        )
        y <- rweib_delayed(
          n = n_y,
          delay1 = delay_y,
          scale1 = scale_y,
          shape1 = shape,
          cens = cens
        )
      }

      # run all estimation methods over the generated data
      estimMethods %>%
        dplyr::mutate(
          testDiffRes = list({
            te_diff <- te_diff2 <- NULL
            # test_diff might also use parallel computations depending on future-settings
            try(
              expr = {
                # test difference in delay1 in exponential model
                te_diff <- test_diff(
                  x = x,
                  y = y,
                  distribution = model,
                  param = "delay1",
                  method = method,
                  profiled = profiled,
                  R = R,
                  type = "all",
                  # log-rank test only once (e.g. MPSE, profiled)
                  doLogrank = method == "MPSE" && profiled,
                  doGOF = myDoGOF
                ) %>%
                  suppressWarnings()
                # bootstrap P-value for combined test for difference in parameters delay+rate
                #+only if the scale (=1/rate for exponential) is indeed different betw groups
                if (!is.null(te_diff) && testParamCombined) {
                  try(
                    expr = {
                      te_diff2 <- test_diff(
                        x = x,
                        y = y,
                        distribution = model,
                        param = c(
                          "delay1",
                          if (model == "exponential") "rate1" else "scale1"
                        ),
                        method = method,
                        profiled = profiled,
                        R = R,
                        type = "bootstrap",
                        doLogrank = FALSE,
                        doGOF = FALSE
                      ) %>%
                        suppressWarnings()
                    }, #yrt inner
                    silent = TRUE
                  )
                } #fi
              }, #yrt outer
              silent = TRUE
            )

            # results dataframe in long format:
            #+test and pvalue columns
            res_i <- NULL
            if (!is.null(te_diff)) {
              res_i <- tibble::enframe(
                unlist(te_diff$P),
                name = "test",
                value = "pvalue"
              ) %>%
                tibble::add_column(
                  param = te_diff$param,
                  R_eff = length(te_diff$testDist),
                  .before = 1
                )
              if (!is.null(te_diff2)) {
                res_i <- tibble::add_row(
                  res_i,
                  param = paste(te_diff2$param, collapse = "+"),
                  R_eff = length(te_diff2$testDist),
                  test = "bootstrap",
                  pvalue = purrr::pluck(
                    te_diff2,
                    "P",
                    "bootstrap",
                    .default = NA_real_
                  )
                )
              } #fi te_diff2
            } #fi te_diff

            res_i
          })
        ) %>%
        # compact testDiff-list column: drop entries that did not work out!
        dplyr::filter(!is.null(testDiffRes), is.list(testDiffRes)) |>
        # drop column R (but keep R_eff)
        dplyr::select(!all_of("R")) |>
        tidyr::unnest(testDiffRes)

      # # extract all P-values/R_eff in long format from each row in estimMethods-df!
      # #+dplyr::reframe (beta in v1.1.0) allows to summarize with more than one row
      # if (NROW(res_h) > 0) {
      #   res_h %>%
      #     dplyr::reframe(model, method, profiled, R,
      #                    R_eff = length(testDiffObj$testDist),
      #                    tibble::enframe(unlist(testDiffObj$P),
      #                                    name = "test", value = "pvalue"))
      # } else {
      #   NULL
      # }
    }, #future expr
    simplify = FALSE
  )

  # drop NULLs (just in case)
  testDiffList <- purrr::compact(testDiffList)

  # bind together into a single long tibble
  dplyr::bind_rows(testDiffList, .id = "run")
} #fn doMCSim


#' Run MC-simulations for each scenario sequentially (row-by-row)
#' @param simSetDF dataframe containing simulation scenarios
#' @param ... further arguments passed to `doMCSim` (currently not used!)
#' @returns tibble of simulations settings with results added
applyMCSims <- function(simSetDF, ...) {
  simSetDF %>%
    dplyr::mutate(
      .,
      results = apply(
        as.matrix(.),
        MARGIN = 1L,
        FUN = doMCSim,
        dropMLEw = myDropMLEw,
        ...
      )
    )
}


#' Add meta data to dataframe
#'
#' The special `comment` attribute is used to store the deparsed meta data list.
#' @param da simulation data
#' @param timeTag time stamp to be added in meta data comment
#' @returns simulation data with meta data added as comment
addMetaData <- function(da, timeTag) {
  # add comment as text
  comment(da) <- list(
    seed = mySeed,
    R = myR,
    mcnrep = myMCNrep,
    workers = myWorkers,
    chnkSize = myChnkSize,
    host = Sys.info()[["nodename"]],
    rversion = R.version.string,
    incubate = as.character(packageVersion("incubate")),
    date = TODAY,
    time = timeTag
  ) %>%
    #paste(names(.), ., sep = '=', collapse = ',')
    deparse()

  da
}


# run & save ----

cat("We started at ***", DATETIME_TAG, "***\n")
rdsBaseName <- paste0("simRes_test_", DATETIME_TAG)
rdsName <- file.path(myResultsDir, paste0(rdsBaseName, ".rds"))

if (myChnkSize < 1L || NROW(simSetting) <= myChnkSize) {
  # no chunking
  simSetting <- applyMCSims(simSetDF = simSetting) %>%
    addMetaData(timeTag = DATETIME_TAG)

  saveRDS(simSetting, file = rdsName)
} else {
  # work in chunks
  rowIdx <- seq_len(NROW(simSetting))
  # how many chunks?
  chnkNbr <- (length(rowIdx) %/% myChnkSize) + 1L
  stopifnot(chnkNbr > 1L, chnkNbr <= 999999L)
  # stripe over the scenarios
  rowIdxLst <- split(
    rowIdx,
    f = rep_len(x = seq_len(chnkNbr), length.out = length(rowIdx))
  )
  stopifnot(length(rowIdxLst) == chnkNbr)

  for (i in seq_along(rowIdxLst)) {
    simSetting_chnk <- simSetting %>%
      dplyr::slice(rowIdxLst[[i]]) %>%
      applyMCSims() %>%
      addMetaData(timeTag = DATETIME_TAG)
    #simSetting_chnk <- applyMCSims(simSetDF = simSetting_chnk)

    message("Writing out chunk ", i, " to RDS-file..")
    saveRDS(
      simSetting_chnk,
      file = file.path(
        myResultsDir,
        paste0(rdsBaseName, "_", sprintf("%06d", i), ".rds")
      )
    )
  } #rof

  # merge chunked output!
  chnkFileNames <- list.files(
    path = myResultsDir,
    pattern = paste0('^', rdsBaseName, '_[[:digit:]]+[.]rds$'),
    full.names = TRUE
  )
  if (length(chnkFileNames)) {
    # re-create complete simSetting data (in chunked order)
    simSetting <- purrr::map(.x = chnkFileNames, .f = readRDS) %>%
      dplyr::bind_rows() %>%
      addMetaData(timeTag = DATETIME_TAG)

    saveRDS(simSetting, file = rdsName)

    if (
      file.exists(rdsName) &&
        !inherits(try(infoRDS(rdsName), silent = TRUE), "try-error")
    ) {
      message(
        "Removing ",
        length(chnkFileNames),
        " intermediate chunked RDS-files!"
      )
      try(file.remove(chnkFileNames))
    } else {
      cat(
        "Failed to merge and save ",
        length(chnkFileNames),
        " intermediate RDS-files!\n"
      )
      cat(
        "Please check these intermediate chunked RDS-files and try to merge and cleanup for yourself!\n"
      )
    } #esle
  } else {
    warning("Did not find chunked RDS-output.", call. = FALSE)
  }
} #esle chunking


# example for a visualization of test results
# simSetting %>%
#   filter(dplyr::near(n, 10), dplyr::near(delay_x, 5), dplyr::near(rate_x, .1)) %>%
#   unnest(cols = P) %>%
#   ggplot(mapping = aes(x = P, col = method)) +
#   geom_freqpoly(bins = 12) + xlim(0,1) +
#   coord_trans(y = "log1p") +
#   facet_grid(rows = vars(delay_y), cols = vars(rate_ratio), labeller = label_both) +
#   labs(x = "P-value", title = "Test Results under different group effects", subtitle = "**n = 10**, delay~x~ = 5")

# teardown ----

# output the latest warnings:
cat("\n+++\nThese are warnings from the script:\n+++\n")
warnings()

if (USE_FUTURE && isNamespaceLoaded("future")) {
  future::plan(strategy = future::sequential)
}

cat("It is ***", toString(Sys.time()), "***\n")
cat("\n\n~fine~\n")
