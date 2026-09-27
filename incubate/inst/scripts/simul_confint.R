#!/usr/bin/env Rscript
# Evaluate bootstrap confidence intervals for one-group situation
# either MPSE or MLE based fits

# init -----

cat("It is ***", toString(Sys.time()), "***\n")

library("incubate")
incubate_ver <- toString(packageVersion("incubate"))
cat('incubate version ', incubate_ver, '\n')
# mkuhn, 2021-12-09: v0.7.2 is needed for new name of bs_data: 'ordinary' instead of 'simple' and boot-package implementation
# mkuhn, 2021-12-15: v0.7.3 is needed for bs_infer = 't'
# mkuhn, 2022-03-17: v0.9 is needed for 2-step confint
# mkuhn, 2022-03-18: v0.9.1 is needed for bs_infer=lognormal & logquantile
# mkuhn, 2022-03-22: v0.9.4 is needed for bs_infer=lognormal & logquantile with log-shift parameter
# mkuhn, 2022-03-24: v0.9.5 logshift internally redefined (should not have impact here, though)
# mkuhn, 2022-03-31: v0.9.6 is needed for smoothing for delay
# mkuhn, 2022-06-??: v1.1.9 new meaning for SMD_factor
# mkuhn, 2022-07-02: v1.2.1.9001 is first to have name-change MPSE
# mkuhn, 2022-07-13: v1.2.1.9010 sets ties='density' as new default for single group fits
# mkuhn, 2025-01-20: v1.3.0.9084 fix likelihood for data with right-censored observations
stopifnot(utils::compareVersion(incubate_ver, "1.3.0.9084") >= 0L)


library("dplyr", warn.conflicts = FALSE)
library("purrr")
library("stringr")
library("tidyr")
library("tibble")
suppressPackageStartupMessages(library("R.utils"))


TODAY <- Sys.Date()

# command line arguments -----
cmdArgs <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  excludeReserved = FALSE,
  excludeEnvVars = TRUE,
  defaults = list(
    resultsDir = getwd(),
    seed = as.integer(TODAY),
    dist = "exponential",
    method = "MPSE",
    bs_data = "parametric",
    smd_factor = "all",
    bs_infer = "all",
    implement = "own",
    slice = 0,
    chnkSize = 0,
    workers = 6,
    R = 150,
    mcnrep = 100
  )
)


if (any(c('help', 'h') %in% names(cmdArgs))) {
  cat(
    'Run Monte-Carlo simulations with delayed Exponential or Weibull data for a single group setting.\n'
  )
  cat(
    'A confidence interval is constructed for all involved parameters and its properties "width" and "coverage" are calculated.\n'
  )
  cat(
    'Sample size, scale and shape use different fixed values that are specified within the script. Delay is fixed to a single value.\n'
  )
  cat('Parameter options are:\n')
  cat('  --help\t print this help\n')
  cat(
    '  --resultsDir=\t specify the directory where to put the result files. Defaults to the directory where Rscript is executed.\n'
  )
  cat(
    '  --single/--1\t flag to use only single scenario: n=10, delay=5, shape=.5 for Weibull, scale=5, level=.95\n'
  )
  cat(
    '  --dist=\t specify underlying distribution. It can be either exponential (default) or Weibull (but not both at the same time).\n'
  )
  cat(
    '  --method=\t specify the estimation method that underlies the confidence interval. MPSE (default) or MLE0 or all.\n'
  )
  cat(
    '  --seed=\t if given, set random seed at the start of the script. Default is date-dependent. In the past, we used a fixed seed of 12. \n'
  )
  cat(
    '  --bs_data=\t with respect to bootstrap data options, choose a scenario\n'
  )
  cat('  --smd_factor=\t factor for smoothing of delay\n')
  cat(
    '  --bs_infer=\t with respect to bootstrap inference options, choose a scenario\n'
  )
  cat('  --implement=\t which implementation: own, boot, all\n')
  cat(
    '  --slice=\t if given, pick only this number of scenarios for the simulation. Or drop the absolute number from tail if negative. It uses base::head\n'
  )
  cat(
    '  --chnkSize=\t chunk size to write out results having processed so many scenarios\n'
  )
  cat(
    '  --workers=\t number of parallel computations using future.callr and future.apply\n'
  )
  cat(
    '  --R=\t\t number of bootstrap samples for the confidence interval: it determines the reliabiltiy of the CI, e.g., R=1000 will start to be stable\n'
  )
  cat(
    '  --mcnrep=\t number of replicated bootstrap data sets to build confidence intervals for: it determines the precision of coverage/width mean value.\n'
  )
  cat('  --print\t show scenarios to simulate and exit.\n')
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

myDist <- cmdArgs[["dist"]]
stopifnot(is.character(myDist), length(myDist) == 1L)
myDist <- match.arg(
  arg = tolower(myDist),
  choices = c("exponential", "weibull")
)

myMethod <- cmdArgs[["method"]]
stopifnot(is.character(myMethod), length(myMethod) == 1L)
myMethod <- match.arg(
  arg = toupper(myMethod),
  choices = c('MPSE', 'MLEn', 'ALL')
)

myWorkers <- cmdArgs[["workers"]]
stopifnot(is.numeric(myWorkers), length(myWorkers) == 1L, myWorkers >= 1L)

myChnkSize <- cmdArgs[["chnkSize"]]
stopifnot(is.numeric(myChnkSize), length(myChnkSize) == 1L)

myR <- cmdArgs[["R"]]
stopifnot(is.numeric(myR), length(myR) == 1L, myR >= 1L)

myMCNrep <- cmdArgs[["mcnrep"]]
stopifnot(is.numeric(myMCNrep), length(myMCNrep) == 1L, myMCNrep >= 1L)

mySlice <- cmdArgs[["slice"]]
stopifnot(
  !is.null(mySlice),
  is.numeric(mySlice),
  is.finite(mySlice),
  length(mySlice) == 1L
)
mySlice <- round(mySlice)

myBS_data <- cmdArgs[["bs_data"]]
stopifnot(!is.null(myBS_data), is.character(myBS_data), length(myBS_data) == 1L)
myBS_data <- match.arg(
  arg = tolower(myBS_data),
  choices = c('ordinary', 'parametric', 'all')
)

mySMD_factor <- cmdArgs[["smd_factor"]]
stopifnot(is.character(mySMD_factor), length(mySMD_factor) == 1L)
mySMD_factor <- tolower(mySMD_factor)

myBS_infer <- cmdArgs[["bs_infer"]]
stopifnot(
  !is.null(myBS_infer),
  is.character(myBS_infer),
  length(myBS_infer) == 1L
)
myBS_infer <- match.arg(
  arg = tolower(myBS_infer),
  choices = c(
    'normal',
    'normal0',
    'lognormal',
    'quantile',
    'quantile0',
    'logquantile',
    't0',
    't',
    'all'
  )
)

myImpl <- cmdArgs[["implement"]]
stopifnot(!is.null(myImpl), is.character(myImpl), length(myImpl) == 1L)
myImpl <- match.arg(arg = tolower(myImpl), choices = c('own', 'boot', 'all'))

mySeed <- cmdArgs[['seed']]
stopifnot(is.numeric(mySeed), length(mySeed) == 1L, mySeed >= 0L)


myPrint <- isTRUE(any(c('print', 'p') %in% tolower(names(cmdArgs))))
mySingle <- isTRUE(any(c('single', '1') %in% tolower(names(cmdArgs))))


# set up simulation setting -----

if (mySeed > 0L) {
  set.seed(mySeed) ##used to be: 12
}

simSetting <- tidyr::expand_grid(
  n = c(5, 8, 10, 12, 20), #, 50, 100), # low n most interesting
  delay = 5, #c(5, 10),
  scale = c(1, 2, 5),
  shape = unique(case_when(
    myDist == 'exponential' ~ 1,
    myDist == 'weibull' ~ c(.5, 2)
  )),
  method = c('MPSE', 'MLEn'),
  bs_data = c('parametric', 'ordinary'),
  smd_factor = c(0, .1, 0.25, 0.5, 0.75, 1, 2),
  implement = c('own', 'boot'),
  # bootstrap inference properties come last
  bs_infer = c(
    'quantile0',
    'quantile',
    'logquantile_.00001',
    'logquantile_.001',
    'logquantile_.1',
    'logquantile_.25',
    'logquantile_.5',
    'logquantile_1',
    'logquantile_2',
    'logquantile_3',
    'logquantile_5',
    'logquantile_10',
    'logquantile_15',
    'logquantile_20',
    'logquantile_25',
    'normal0',
    'normal',
    'lognormal_.00001',
    'lognormal_.001',
    'lognormal_.1',
    'lognormal_.25',
    'lognormal_.5',
    'lognormal_1',
    'lognormal_2',
    'lognormal_3',
    'lognormal_5',
    'lognormal_10',
    'lognormal_15',
    'lognormal_20',
    'lognormal_25'
  ), #, 't0', 't'),
  level = c(.9, .95, .99)
) %>%
  # boot package offers no CIs based on t-quantiles
  dplyr::filter(!(bs_infer %in% c('t', 't0', 'normal0') & implement == 'boot'))
# mkuhn, quick filter
# dplyr::filter(method == 'MPSE', ! bs_infer %in% c('quantile0', 'normal0', 't0'))

if (myMethod != 'ALL') {
  simSetting <- simSetting %>%
    dplyr::filter(method == myMethod)
}

# first select the inference methods
if (myBS_infer != 'all') {
  simSetting <- simSetting |>
    dplyr::filter(bs_infer == myBS_infer)
}

# nest simSetting: pack all bs_infer together
simSetting <- simSetting |>
  dplyr::nest_by(
    n,
    delay,
    scale,
    shape,
    method,
    bs_data,
    smd_factor,
    implement,
    .key = 'bs_infer'
  )


if (myBS_data != 'all') {
  simSetting <- simSetting |>
    dplyr::filter(bs_data == myBS_data)
}

if (mySMD_factor != 'all') {
  mySMD_factor <- suppressWarnings(as.numeric(mySMD_factor))
  if (is.finite(mySMD_factor)) {
    simSetting <- simSetting |>
      dplyr::mutate(smd_factor = mySMD_factor) |>
      dplyr::distinct()
  } else {
    stop('Could not parse the value given for smd_factor=')
  }
}

if (myImpl != 'all') {
  simSetting <- simSetting |>
    dplyr::filter(implement == myImpl)
}


if (mySingle) {
  # scenario does not involve level and bs_infer
  simSetting <- simSetting %>%
    #n=10, delay=5, shape=.5 for Weibull, scale=5
    dplyr::filter(
      dplyr::near(n, 12L),
      dplyr::near(delay, 5L),
      dplyr::near(
        shape,
        case_when(
          myDist == 'exponential' ~ 1,
          myDist == 'weibull' ~ .5,
          TRUE ~ 99
        )
      ),
      dplyr::near(scale, 5L),
      method == 'MPSE'
    )
}

if (!dplyr::near(mySlice, 0L)) {
  # use base::head to ignore rowwise()
  simSetting <- head(simSetting, n = mySlice)
}

if (NROW(simSetting) == 0L) {
  cat('No scenarios selected for simulation.\n')
  quit(save = 'no')
}

if (myPrint) {
  print(knitr::kable(
    dplyr::select(simSetting, !bs_infer),
    format = 'pipe',
    digits = 2
  ))
  cat(
    '\nIn total,',
    NROW(simSetting),
    'simulation scenarios.\nEach scenario is covered by ',
    myMCNrep,
    'data replications.\n'
  )
  cat('A confidence interval has R=', myR, 'parametric bootstrap samples.\n')
  cat('Results directory is set to ', myResultsDir, '\n')
  quit(save = 'no')
}


# set up parallel computing ----
if (myWorkers > 1L) {
  library("future.callr")
  library("future.apply")

  future::plan(strategy = future.callr::callr, workers = myWorkers)
}

# functions -----

#' Monte-Carlo simulation for a single simulation setting.
#' Uses parallel computation (using `future_replicate`) to go through the MC-simulations. This is the first level of parallelisation.
#' Each bootstrap test is also future-aware (which listens on the 2nd layer of future topology).
#' @param dist character. delay-distribution to simulate data from
#' @param bs_data character. data generation in bootstrap
#' @param implement character. flag to choose between implementation via boot-package or own future-aware code
#' @param bsInferDF dataframe. inference scenarios to build confidence interval for, defined by bs_infer & level
#' @param agg flag. Aggregate values over runs
#' @return dataframe. coverage and width in the different Monte-Carlo runs.
simfun <- function(
  dist,
  n,
  delay,
  scale,
  shape,
  method,
  bs_data,
  smd_factor,
  implement,
  bsInferDF,
  agg = TRUE
) {
  stopifnot(
    is.data.frame(bsInferDF),
    'level' %in% names(bsInferDF),
    is.numeric(bsInferDF$level),
    all(bsInferDF$level > 0L & bsInferDF$level < 1L)
  )

  bsInferDF$ID <- seq_len(NROW(bsInferDF))
  agg <- isTRUE(agg)

  paramDF <- tibble::enframe(
    if (myDist == 'exponential') {
      c(delay = delay, rate = 1 / scale)
    } else {
      c(delay = delay, scale = scale, shape = shape)
    },
    name = 'param',
    value = 'truth'
  )

  ciList <- future.apply::future_replicate(
    n = myMCNrep,
    expr = {
      # generate data
      x <- if (myDist == 'exponential') {
        stopifnot(dplyr::near(shape, 1L))
        rexp_delayed(n = n, delay = delay, rate = 1 / scale)
      } else {
        stopifnot(myDist == 'weibull')
        rweib_delayed(n = n, delay = delay, scale = scale, shape = shape)
      }

      ciObj <- NULL
      try(
        expr = {
          # fitting a delay model assuming the correct underlying model
          dm <- delay_model(x = x, distribution = myDist, method = method)

          # confint's bsDataStep is future-aware and can use parallelization
          bsDatObj <- incubate:::bsDataStep(
            dm,
            bs_data = bs_data,
            R = myR,
            useBoot = implement == 'boot',
            smd_factor = smd_factor
          )

          # bootstrap inference
          ciObj <- purrr::map_dfr(
            .x = bsInferDF$ID,
            .f = ~ {
              # hack: have different options for log-shift (in case of log-transformation)
              bs_inf <- bsInferDF$bs_infer[.x]
              log_shift <- NA_real_
              if (startsWith(bs_inf, 'log')) {
                bs_inf_split <- stringr::str_split(
                  bs_inf,
                  pattern = stringr::fixed('_')
                )[[1L]]
                bs_inf <- bs_inf_split[[1L]]
                if (length(bs_inf_split) == 2L) {
                  log_shift <- as.numeric(bs_inf_split[[2L]])
                }
              }
              confint(
                dm,
                level = bsInferDF$level[.x],
                bs_data = bsDatObj,
                bs_infer = bs_inf,
                logshift_delay = log_shift,
                useBoot = implement == 'boot'
              )
            } |>
              tibble::as_tibble(rownames = 'param') |>
              purrr::set_names(nm = c('param', 'lower', 'upper')) |>
              dplyr::inner_join(x = paramDF, y = _, by = 'param') |>
              dplyr::mutate(width = upper - lower) %>%
              dplyr::rowwise(param) |>
              dplyr::mutate(
                covered = dplyr::between(truth, left = lower, right = upper)
              ) |>
              dplyr::ungroup() |>
              dplyr::select(param, width, covered) |>
              tidyr::pivot_longer(
                cols = c(width, covered),
                names_to = 'quality',
                values_to = 'value'
              ),
            .id = 'bsInferDFRow'
          ) |>
            dplyr::mutate(bsInferDFRow = as.numeric(bsInferDFRow)) |>
            dplyr::inner_join(
              y = bsInferDF,
              by = dplyr::join_by(bsInferDFRow == 'ID')
            ) |>
            dplyr::select(!bsInferDFRow)
        },
        silent = TRUE
      )

      ciObj
    },
    simplify = FALSE,
    future.seed = TRUE
  )

  ciList <- ciList |>
    # drop NULLs
    purrr::compact() |>
    # bind results together
    dplyr::bind_rows(.id = 'run') |>
    # mkuhn, 2022-07-13: check for finite value
    dplyr::filter(is.finite(value))

  if (agg) {
    ciList <- ciList |>
      summarize(
        nagg = n(),
        meanv = mean(value, na.rm = TRUE),
        medianv = median(value, na.rm = TRUE),
        .by = c(param, quality, bs_infer, level)
      ) |>
      # mean of coverage, median of width
      mutate(
        mv = if_else(quality == 'covered', true = meanv, false = medianv)
      ) |>
      dplyr::select(!c(meanv, medianv))
  }

  ciList
}

#' run MC-simulations for each scenario sequentially (row-by-row)
#' @param agg flag. Aggregate values over runs
#' @return extended tibble with results and environment meta information
applySimFun <- function(simSetDF, agg = TRUE) {
  stopifnot(length(dplyr::groups(simSetDF)) > 0L)

  simSetDF |>
    #rowwise() %>%
    # encapsulate with list(..) to get a list column (and not a complaint)
    mutate(
      ci_res = list(simfun(
        dist = myDist,
        n = n,
        delay = delay,
        scale = scale,
        shape = shape,
        method = method,
        bs_data = bs_data,
        smd_factor = smd_factor,
        implement = implement,
        bsInferDF = bs_infer,
        agg = agg
      )),
      # drop bs_infer (as it is fused into ci_res)
      bs_infer = NULL
    ) |>
    ungroup()
  # mutate(incubate = as.character(packageVersion('incubate')),
  #        Rversion = R.version.string,
  #        Host = Sys.info()[["nodename"]],
  #        Time = format(Sys.time(), format = "%Y-%m-%d_%Hh%M"))
}


# run & save ----

addMetaData <- function(da, timeTag) {
  comment(da) <- list(
    seed = mySeed,
    R = myR,
    mcnrep = myMCNrep,
    workers = myWorkers,
    chnkSize = myChnkSize,
    host = Sys.info()[["nodename"]],
    rversion = R.version.string,
    incubate = as.character(packageVersion('incubate')),
    date = TODAY,
    time = timeTag
  ) %>%
    #paste(names(.), ., sep = '=', collapse = ',')
    deparse
  da
}

AGG <- TRUE
DATETIME_TAG <- format(Sys.time(), format = "%Y-%m-%d-%Hh%Mm%Ss")
rdsBaseName <- paste0("simRes_confint_", DATETIME_TAG, "_agg"[AGG])
rdsName <- file.path(myResultsDir, paste0(rdsBaseName, ".rds"))

if (myChnkSize < 1L || NROW(simSetting) <= myChnkSize) {
  # no chunking
  simSetting <- applySimFun(simSetDF = simSetting, agg = AGG) |>
    addMetaData(timeTag = DATETIME_TAG)
  saveRDS(simSetting, file = rdsName)
} else {
  # work in chunks
  rowIdx <- seq_len(NROW(simSetting))
  chunk_nbr <- (length(rowIdx) %/% myChnkSize) + 1L
  stopifnot(chunk_nbr > 1L, chunk_nbr <= 999999L)
  # stripe over the scenarios
  rowIdxLst <- split(
    rowIdx,
    f = rep_len(x = seq_len(chunk_nbr), length.out = length(rowIdx))
  )
  stopifnot(length(rowIdxLst) == chunk_nbr)
  for (i in seq_along(rowIdxLst)) {
    #slice does not work on rowwise tibbles (as it works *per group*)
    simSetting_chk <- applySimFun(
      simSetDF = simSetting[rowIdxLst[[i]], ],
      agg = AGG
    )
    rdsChkName <- file.path(
      myResultsDir,
      paste0(rdsBaseName, "_", sprintf("%06d", i), ".rds")
    )
    cat("Write out chunk ", i, "\n")
    saveRDS(
      simSetting_chk %>% addMetaData(timeTag = DATETIME_TAG),
      file = rdsChkName
    )
  } #rof

  # merge chunked output!
  chkFileNames <- list.files(
    path = myResultsDir,
    pattern = paste0('^', rdsBaseName, '_[[:digit:]]+[.]rds$'),
    full.names = TRUE
  )
  if (length(chkFileNames)) {
    chkFiles <- purrr::map(.x = chkFileNames, .f = readRDS)

    saveRDS(
      chkFiles |>
        dplyr::bind_rows() |>
        addMetaData(timeTag = DATETIME_TAG),
      file = rdsName
    )

    if (
      file.exists(rdsName) &&
        (!exists('infoRDS') ||
          !inherits(try(infoRDS(rdsName), silent = TRUE), "try-error"))
    ) {
      message(
        "Removing ",
        length(chkFileNames),
        " intermediate chunked RDS-files!"
      )
      try(file.remove(chkFileNames))
    } #fi remove chk RDS files
  } else {
    warning("Failed to find chunked RDS-output.")
  }
} #esle chunking


# teardown ----

# output the latest warnings:
cat("\n+++\nThese are warnings from the script:\n+++\n")
warnings()

if (myWorkers > 1L) {
  future::plan(strategy = future::sequential)
}


cat("It is ***", toString(Sys.time()), "***\n")
cat("\n\n~fine~\n")
