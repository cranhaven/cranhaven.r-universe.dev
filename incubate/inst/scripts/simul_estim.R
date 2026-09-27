#!/usr/bin/env Rscript
# Evaluate parameter estimation in delayed Weibull setting
# We look at bias and precision of estimation.

cat("\nMC-simulations around parameter estimation in a single group setting\n")


# init -----
library("incubate")
version_inc <- packageVersion("incubate")
# minimal version check:
# v1.3.0.9084 fix likelihood for data with right-censored observations (mkuhn, 2025-01-20)
# v1.3.0.9092 fix in rexp_delayed and rweib_delayed with cens= (mkuhn, 2025-03-31)
# v1.3.0.9101 added cousineauGH weights
stopifnot(version_inc >= "1.3.0.9101")
cat('(incubate v', toString(version_inc), ')\n', sep = "")

library("tibble")
library("dplyr", warn.conflicts = FALSE)
stopifnot(packageVersion("dplyr") > "1.0.10")
library("purrr", warn.conflicts = FALSE)
library("tidyr", warn.conflicts = FALSE)
suppressPackageStartupMessages(library("R.utils"))
# we use future_replicate
#+(even when using sequential plan [=no parallelization])
library("future.apply")


# capture date/time for seed and time stamp
TODAY <- Sys.Date()
NOW <- Sys.time()
DATETIME_TAG <- format(NOW, format = "%Y-%m-%d-%Hh%Mm%Ss")
# base name for output file
OUTPUT_BASENAME <- paste0("simRes_estim_", DATETIME_TAG)
DELAY_V <- 300

SEED_DEFAULT <- paste0(as.integer(TODAY), format(NOW, format = "%H%M")) |>
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
    #dist="weibull",
    ## assumed model for estimation
    #model="weibull",
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
    'Run Monte-Carlo simulations with delayed Weibull data for a single group.\n'
  )
  cat(
    'Parameters are estimated repeatedly so that estimation performance can be assessed.\n'
  )
  cat(
    'Sample size, scale and shape use different fixed values (see code in this script).\n'
  )
  cat(
    "Delay is a nuisance parameter and stays fixed at value ",
    DELAY_V,
    ".\n",
    sep = ""
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
    '  --n=\t\t sample size number to use in the simulation. By default (n=-1) only a single small sample size is used. n=0 will use a whole set of preconfigured sample size values.\n'
  )
  cat(
    '  --cens\t apply also random right-censoring during the simulation study\n'
  )
  cat(
    '  --slice=\t if given, pick only this number of first scenarios for simulations. If negative, scenarios are taken from the tail.\n'
  )
  cat(
    '  --seed=\t if given, set random seed at the start of the script. Default depends on current date-time.\n'
  )
  cat(
    '  --chnkSize=\t chunk size to write out results having processed so many scenarios. Default is no chunking (=0).\n'
  )
  cat(
    '  --workers=\t number of parallel computations using `future.callr` and `future.apply`. The only level of parallelization is across the MC-replications for each simulation setting.\n'
  )
  cat(
    '  --mcnrep=\t size of Monte-Carlo study: it is the number of simulated data sets and parameter estimates.\n'
  )
  quit(save = 'no')
}

myResultsDir <- cmdArgs[["resultsDir"]]
stopifnot(
  is.character(myResultsDir),
  dir.exists(myResultsDir),
  # check read & write permission (first octal information)
  (file.mode(myResultsDir) |>
    as.character() |>
    substr(1, 1) |>
    as.octmode() &
    6) ==
    '6'
)

myWorkers <- cmdArgs[["workers"]]
stopifnot(is.numeric(myWorkers), length(myWorkers) == 1L, myWorkers >= 1L)
if (myWorkers > 1L && !requireNamespace("future.callr", quietly = TRUE)) {
  cat("Please install package future.callr to have parallel computing work!\n")
  myWorkers <- 1L
}
USE_FUTURE <- myWorkers > 1L

myChnkSize <- cmdArgs[["chnkSize"]]
stopifnot(is.numeric(myChnkSize), length(myChnkSize) == 1L)

myMCNrep <- cmdArgs[["mcnrep"]]
stopifnot(is.numeric(myMCNrep), length(myMCNrep) == 1L, myMCNrep >= 1L)

mySlice <- cmdArgs[["slice"]]
stopifnot(!is.null(mySlice), is.numeric(mySlice), length(mySlice) == 1L)
mySlice <- ceiling(mySlice)

mySeed <- cmdArgs[["seed"]]
stopifnot(is.numeric(mySeed), length(mySeed) == 1L, mySeed >= 0L)

myN <- cmdArgs[["n"]]
stopifnot(!is.null(myN), is.numeric(myN), length(myN) == 1L)
myN <- ceiling(myN)

cmdArgsLo <- tolower(names(cmdArgs))
myPrint <- isTRUE(any(c("print", "p") %in% cmdArgsLo))
myCens <- isTRUE(any(c("cens", "censoring") %in% cmdArgsLo))


# set up simulation setting -----

if (mySeed > 0L) {
  set.seed(mySeed)
  cat("Set seed to ", mySeed, "\n")
}

# choose sample sizes: is there a specific sample size given?
# default (myN=-1) is to use only a small sample size:
#+it gives nice power curves for chosen difference difference in delay
nVctr <- switch(
  EXPR = paste0("S", sign(myN)),
  `S-1` = 8,
  S0 = c(8, 16, 32, 50),
  S1 = myN,
  stop("Unexpected input for --n")
)

stopifnot(is.numeric(nVctr), all(nVctr > 0))

simSetting <- tidyr::expand_grid(
  nObs = nVctr,
  delay = DELAY_V,
  #scale as nuisance parameter
  scale = 100, #c(5, 10), #c(1, 2, 5),
  #shape taken from Cousineau
  shape = c(.5, 1, 1.5, 2, 2.5),
  #cens = 0: all observed (=no censoring)
  cens = c(0, 0.1, 0.2, 0.3)
)

# sanity/health checks
simSetting <- simSetting |>
  # enough expected number of observations
  dplyr::filter(cens >= 0, cens < 1, nObs * (1 - cens) > 5)

# default: no censoring (cens = 0)
if (!myCens) {
  simSetting <- simSetting |>
    dplyr::slice_min(cens)
}


# slicing in simulation scenarios
#+using head or tail depending on sign)
if (!dplyr::near(mySlice, 0)) {
  simSetting <- local({
    sliceF <- if (mySlice > 0) {
      dplyr::slice_head
    } else {
      dplyr::slice_tail
    }

    simSetting |>
      sliceF(n = abs(mySlice))
  })
} #fi mySlice


if (myPrint) {
  print(knitr::kable(simSetting, format = "pipe", digits = 2))
  cat("\n")
  cat(NROW(simSetting), " simulation scenarios in total.\n")
  cat("Each scenario is covered by ", myMCNrep, "MC-data replications.\n")
  if (mySeed > 0) {
    cat("Seed was set initially to", mySeed, "\n")
  } else {
    cat("Seed was **not** set!\n")
  }
  cat("Results directory is ", myResultsDir, "\n")

  quit(save = "no")
} #fi myPrint


# set up parallel computing ----

if (USE_FUTURE) {
  library("future.callr")

  future::plan(strategy = future.callr::callr, workers = myWorkers)
} #fi USE_FUTURE


# functions -----

#' Run Monte-Carlo simulations to assess estimation properties for Weibull model
#' parameter in a given simulation setting
#'
#' A fixed set of estimation methods are used. Uses parallel computation
#' (future_replicate) to go through the (=nrep) MC-simulations.
#' @param DGPsetting numeric. data generation process. a row from `simSetting`.
#'   It encodes parameters that specify the data generating process for both
#'   groups
#' @param N_mcrep numeric. Number of MC-simulation runs
#' @return dataframe, estimation results per MC-run.
doMCSim <- function(DGPsetting, N_mcrep) {
  # settings from the environment:
  stopifnot(length(N_mcrep) == 1, is.numeric(N_mcrep), N_mcrep >= 1)
  stopifnot(is.numeric(DGPsetting), length(DGPsetting) == 5L)

  nObs <- DGPsetting[[1]]
  delay <- DGPsetting[[2]]

  scaleV <- DGPsetting[[3]]
  shape <- DGPsetting[[4]]
  cens <- DGPsetting[[5]]

  estimMethods <- tibble::tribble(
    ~method,
    ~profiled,
    ~weight,
    "MLEn",
    TRUE,
    NA_character_,
    "MLEw",
    TRUE,
    "sdist_median"
  )

  # Cousineau2009-weights are only available for n<=16
  if (nObs <= 16) {
    estimMethods <- estimMethods |>
      tibble::add_case(
        method = "MLEw",
        profiled = TRUE,
        weight = "cousineau2009"
      )
  } #fi

  # CousineauGH-weights are only available for n<=100
  if (nObs <= 100) {
    estimMethods <- estimMethods |>
      tibble::add_case(method = "MLEw", profiled = TRUE, weight = "cousineauGH")
  } #fi
  estimMethods <- estimMethods |>
    dplyr::mutate(software = "incubate", .before = 1) |>
    dplyr::rowwise()

  # run MC-replicates for each estimation method in turn
  #+for the specified data generating process setting (`DGPsetting`)
  estimList <- future.apply::future_replicate(
    n = ceiling(N_mcrep),
    future.packages = c(
      "dplyr",
      "purrr",
      "incubate",
      "tibble",
      if (cens > 0) "survival"
    ),
    future.globals = c("nObs", "delay", "scaleV", "shape", "cens"),
    future.seed = TRUE,
    expr = {
      # generate Weibull data
      x <- rweib_delayed(
        n = nObs,
        delay1 = delay,
        scale1 = scaleV,
        shape1 = shape,
        cens = cens
      ) |>
        sort.int()

      estimMethods |>
        dplyr::mutate(
          estimRes = list({
            fm_w <- coef_w <- NULL

            try(
              expr = {
                fm_w <- delay_model(
                  x = {{ x }},
                  y = NULL,
                  distribution = "weibull",
                  twoPhase = FALSE,
                  method = method,
                  control = list(
                    profiled = profiled,
                    MLEw_weight = if (is.na(weight)) NULL else weight
                  )
                )
              },
              silent = TRUE
            )

            if (!is.null(fm_w)) {
              coef_w <- fm_w |>
                coef() |>
                tibble::enframe(name = "param", value = "value")
            } #fi
            coef_w
          })
        ) |>
        # drop scenarios that did not work out!
        dplyr::filter(!is.null(estimRes), is.list(estimRes)) |>
        tidyr::unnest(estimRes) |>
        tidyr::nest(.key = "estim") |>
        tibble::add_column(data = list(x), .before = 1)
    }, #future expr
    simplify = FALSE
  )

  # drop NULLs (just in case)
  estimList <- purrr::compact(estimList)

  # bind together into a single long tibble
  dplyr::bind_rows(estimList, .id = "run")
} #fn doMCSim


#' Run MC-simulations for each scenario sequentially (row-by-row)
#' @param simSetDF dataframe containing simulation scenarios
#' @returns tibble of simulations settings with list-column `results`
applyMCSims <- function(simSetDF) {
  simSetDF |>
    dplyr::mutate(
      results = apply(
        as.matrix(simSetDF),
        MARGIN = 1,
        FUN = doMCSim,
        N_mcrep = myMCNrep
      )
    )
}


#' Add meta data to dataframe
#'
#' The special `comment` attribute is used to store the deparsed meta data list.
#' @param resDat simulation results data
#' @param timeTag time stamp to be added in meta data comment
#' @returns simulation data with meta data added as comment
addMetaData <- function(resDat, timeTag) {
  # add comment as text
  comment(resDat) <- list(
    seed = mySeed,
    mcnrep = myMCNrep,
    workers = myWorkers,
    chnkSize = myChnkSize,
    host = Sys.info()[["nodename"]],
    rversion = R.version.string,
    incubate = as.character(packageVersion("incubate")),
    date = TODAY,
    time = timeTag
  ) |>
    #paste(names(.), ., sep = '=', collapse = ',')
    deparse()

  resDat
}


#' Saves results data to disk.
#' @param resDat simulation results data
#' @param chnkIdx file chunk ID or `NULL` if not chunked file
#' @returns name of results file (invisibly). side effect: writes out data to disk
writeOutData <- function(resDat, chnkIdx = NULL) {
  stopifnot(nzchar(myResultsDir))
  stopifnot(nzchar(OUTPUT_BASENAME), length(OUTPUT_BASENAME) == 1)
  stopifnot(
    exists("DATETIME_TAG"),
    nzchar(DATETIME_TAG),
    length(DATETIME_TAG) == 1
  )
  stopifnot(is.data.frame(resDat))

  outpFile <- NULL

  if (is.null(chnkIdx)) {
    resDat_u <- resDat |>
      tidyr::unnest(cols = results)

    message("Writing out simulation data to file..")
    # save simulation data (per setting and per run)
    resDat_u |>
      dplyr::select(!all_of("estim")) |>
      addMetaData(timeTag = DATETIME_TAG) |>
      saveRDS(
        file = file.path(myResultsDir, paste0(OUTPUT_BASENAME, "_data.rds"))
      )

    message("Writing out simulation results to file..")
    # save estim results (per setting and per run)
    outpFile <- file.path(myResultsDir, paste0(OUTPUT_BASENAME, ".rds"))
    resDat_u |>
      dplyr::select(!all_of("data")) |>
      addMetaData(timeTag = DATETIME_TAG) |>
      saveRDS(file = outpFile)
  } else {
    stopifnot(is.numeric(chnkIdx), length(chnkIdx) == 1, chnkIdx >= 1)
    chnkIdx <- trunc(chnkIdx)

    message("Writing out chunk ", chnkIdx, " to file..")
    outpFile <- file.path(
      myResultsDir,
      paste0(OUTPUT_BASENAME, "_", sprintf("%06d", chnkIdx), ".rds")
    )
    resDat |>
      addMetaData(timeTag = DATETIME_TAG) |>
      saveRDS(file = outpFile)
  }

  # return
  invisible(outpFile)
} #fn writeOutData


# run & save ----

cat("We started at ***", DATETIME_TAG, "***\n")

if (myChnkSize < 1L || NROW(simSetting) <= myChnkSize) {
  # no chunking
  applyMCSims(simSetDF = simSetting) |>
    writeOutData()
} else {
  # work in chunks
  rowIdx <- seq_len(NROW(simSetting))
  # how many chunks?
  chnkNbr <- (length(rowIdx) %/% myChnkSize) + 1L
  stopifnot(chnkNbr > 1L, chnkNbr <= 999999L)
  # stripe over the scenarios
  rowIdxLst <- split(
    x = rowIdx,
    f = rep_len(x = seq_len(chnkNbr), length.out = length(rowIdx))
  )
  stopifnot(length(rowIdxLst) == chnkNbr)

  for (i in seq_along(rowIdxLst)) {
    simSetting |>
      dplyr::slice(rowIdxLst[[i]]) |>
      applyMCSims() |>
      writeOutData(chnkIdx = i)
  } #rof

  # merge chunked output!
  chnkFileNames <- list.files(
    path = myResultsDir,
    pattern = paste0('^', OUTPUT_BASENAME, '_[[:digit:]]+[.]rds$'),
    full.names = TRUE
  )
  if (length(chnkFileNames)) {
    # re-create complete simulation results data (in chunked order)
    resOutputFN <- purrr::map(.x = chnkFileNames, .f = readRDS) |>
      dplyr::bind_rows() |>
      writeOutData()

    # clean up intermediate chunked result files
    if (
      file.exists(resOutputFN) &&
        !inherits(try(infoRDS(resOutputFN), silent = TRUE), "try-error")
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


# teardown ----

# output the latest warnings:
cat("\n+++\nThese are warnings from the script:\n+++\n")
warnings()
dplyr::last_dplyr_warnings(n = 4)

if (USE_FUTURE && isNamespaceLoaded("future")) {
  future::plan(strategy = future::sequential)
}

cat("It is ***", toString(Sys.time()), "***\n")
cat("\n\n~fine~\n")
