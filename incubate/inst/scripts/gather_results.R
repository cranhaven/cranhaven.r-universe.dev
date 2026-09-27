#!/usr/bin/env Rscript
# mkuhn, 2022-07-07
# Gather individual simulation results from temporary files.
# For results of type="test", merging of previous results is supported.

# setup --------------------------------------------------------------------

library("incubate")
cat("incubate v", toString(packageVersion("incubate")), "\n", sep = "")

library("rlang")
library("dplyr", warn.conflicts = FALSE)
library("tidyr", warn.conflicts = FALSE)
library("purrr", warn.conflicts = FALSE)
library("glue")
suppressPackageStartupMessages(library("R.utils"))
stopifnot(packageVersion("purrr") > "1.0.0") #for list_flatten()


cmdArgs <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  excludeReserved = FALSE,
  excludeEnvVars = TRUE,
  defaults = list(
    resultsDir = file.path(getwd(), "results"),
    resultsTag = "MS",
    type = "test"
  )
)

if (any(c('help', 'h') %in% names(cmdArgs))) {
  cat(
    'Gather Monte-Carlo simulation results from temporary RDS result files and save it as a common list.\n'
  )
  cat('Temporary data files have a date tag in their name.\n')
  cat('Parameter options are:\n')
  cat('  --help\t print this help\n')
  cat(
    '  --resultsDir=\t specify the directory where to find and also where to put the result files. Defaults to sub-directory "results" of working directory.\n'
  )
  cat(
    '  --resultsTag=\t specify a name suffix for results file. Default is "MS".\n'
  )
  cat(
    '  --type=\t what type of results to gather? "test" (default) or "confint" or "estim"\n'
  )
  cat(
    '  --removeTemp\t flag to clean temporary results file after they have been saved.\n'
  )

  quit(save = 'no')
}

myRemoveTemp <- isTRUE(any(c('r', 'removeTemp') %in% names(cmdArgs)))

myResultsDir <- cmdArgs[["resultsDir"]]
stopifnot(is.character(myResultsDir), dir.exists(myResultsDir))

myResultsTag <- cmdArgs[["resultsTag"]]
stopifnot(
  is.character(myResultsTag),
  length(myResultsTag) == 1L,
  nzchar(myResultsTag)
)

myType <- cmdArgs[["type"]]
stopifnot(is.character(myType), length(myType) == 1L, nzchar(myType))
myType <- match.arg(
  arg = tolower(myType),
  choices = c("test", "confint", "estim")
)

if (myType == "confint") {
  cat("Please check this script first to set up run namespace properly!\n")
  q(save = "no")
}

indOffset <- 0L
RES_FILEN <- c(
  res = file.path(
    myResultsDir,
    paste0('simRes_', myType, '_', myResultsTag, '.rds')
  ),
  data = file.path(
    myResultsDir,
    paste0('simRes_', myType, '_', myResultsTag, '_data.rds')
  )
)

# temporary results files have a date tag in their file name
# end with s (for seconds)
FPATTERN_DATE <- "_2\\d{3}-\\d{2}-\\d{2}-\\d+.+s"
simResFileNames <- list.files(
  myResultsDir,
  pattern = paste0("simRes_", myType, FPATTERN_DATE, "[.]rds$"),
  full.names = TRUE
)

# check for simRes-data files as well
#+(currently only for type=estim)
simResDataFileNames <- list.files(
  myResultsDir,
  pattern = paste0("simRes_", myType, FPATTERN_DATE, "_data[.]rds$"),
  full.names = TRUE
)

# flag: do we have result data files?
haveResDataFiles <- length(simResDataFileNames) > 0

if (haveResDataFiles) {
  stopifnot(myType == "estim")
  #XXX merge data files!
}

if (!length(simResFileNames)) {
  cat(
    "No matching temporary result files for ",
    myType,
    " were found in sub-directory ",
    myResultsDir,
    "!\n"
  )
  q(save = "no")
}


cat(
  length(simResFileNames),
  " temporary result files found in sub-directory ",
  sQuote(myResultsDir),
  "!\n",
  sep = ""
)
cat("\n  * ")
cat(paste(simResFileNames, collapse = "\n  * "))
cat("\n")
if (haveResDataFiles) {
  if (length(simResFileNames) != length(simResDataFileNames)) {
    cat("But we found different number of temporary data result files!\n")
    q(save = "no")
  } else {
    cat("We found same number of temporary data result files!\n")
    #XXX check same time stamps betw res and res-DATA files?!
  } #esle
} #fi

cat("Is that correct and proceed? [y/N] ")
# check if we have the right collection of temporary files
if (
  readLines(con = "stdin", n = 1L) |>
    substr(1, 1) |>
    tolower() !=
    "y"
) {
  cat("Quitting upon your request..\n")
  q(save = "no")
} #fi


# create namespace for the different runs as vector
RUN_NS <- tidyr::crossing(L1 = LETTERS, L2 = LETTERS) |>
  dplyr::mutate(L = paste0(L1, L2), .keep = "none") |>
  dplyr::pull(L)


# previous results --------------------------------------------------------

# look at previous results (already saved)
resPrev <- NULL

# prepare index offset (if there are previous results)
if (file.exists(RES_FILEN[["res"]])) {
  resPrev <- readRDS(RES_FILEN[["res"]])

  if (!is.list(resPrev) || !is_named(resPrev)) {
    # no proper previous results!
    cat('\nResults file is not a (named) list! We start over from scratch!\n')
    resPrev <- NULL
  } else {
    # results data found!
    cat(glue("There have been {length(resPrev)} entries saved already!"), "\n")

    if (haveResDataFiles) {
      cat(
        "\nSorry, re-using previous results when also result DATA files are present is currently not supported!\n"
      )
      q(save = "no")
    } #fi

    # run-numbers only for tests (currently not for confint)
    if (myType == "test") {
      indOffset <- max(
        0L,
        which(
          RUN_NS %in%
            purrr::map_chr(
              resPrev,
              .f = ~ {
                substr(
                  .x[['run']][[1L]],
                  start = 1L,
                  stop = nchar(RUN_NS[[1L]])
                )
              }
            )
        ),
        na.rm = TRUE
      )
      cat(glue("Offset index from saved results is {indOffset}."), "\n")
    } else {
      cat(
        "This type ",
        myType,
        "is not (yet) properly supported for finding index offset from previous results!\n"
      )
      q(save = "no")
    }
  } #fi resPrev
} #fi RES_FILEN[["res"]]


# read in temporary results --------------------------------------------------

#' Read in temporary result file, process the results
#' @param rdsFN file name of RDS-file containing results
#' @param idx index number of given rds filename
#' @param isDataFile flag: is it a results DATA file?
#' @returns named list containing the unnested data
readResultFile <- function(rdsFN, idx, isDataFile = FALSE) {
  stopifnot(
    exists("indOffset"),
    is.numeric(indOffset),
    length(indOffset) == 1,
    indOffset >= 0
  )
  stopifnot(!missing(idx), is.numeric(idx), length(idx) == 1, idx >= 1)

  rdsF <- readRDS(rdsFN)
  rdsFC <- comment(rdsF)
  # meta-data from comment
  mdList <- if (!is.null(rdsFC)) {
    eval(parse(text = rdsFC))
  } else {
    list(host = "???", time = "202???")
  }
  stopifnot(is.list(mdList), all(c('host', 'time') %in% names(mdList)))

  resName <- paste(mdList[['host']], mdList[['time']], sep = '||')

  resCntnt <- if (isDataFile) {
    rdsF
  } else {
    unnestVar <- switch(
      myType,
      confint = "ci_res",
      test = "results",
      estim = "estim",
      stop("Unknown type ", sQuote(myType))
    )
    stopifnot(all(unnestVar %in% names(rdsF)))
    rdsF |>
      tidyr::unnest(cols = all_of(unnestVar))
  }

  if ("run" %in% names(resCntnt) && myType != "confint") {
    stopifnot(indOffset + idx <= length(RUN_NS))
    resCntnt <- resCntnt |>
      # prepend run namespace, like 'AS'
      dplyr::mutate(run = paste0(RUN_NS[[indOffset + idx]], run))
  } #fi

  # pass on comment to unnested dataframe
  comment(resCntnt) <- rdsFC

  rlang::list2(!!resName := resCntnt)
} #fn


resCandidates <- purrr::imap(.x = simResFileNames, .f = readResultFile) |>
  # drop imap's additional list level
  purrr::list_flatten()

resDataCandidates <- purrr::imap(
  .x = simResDataFileNames,
  .f = readResultFile,
  isDataFile = TRUE
) |>
  # drop imap's additional list level
  purrr::list_flatten()


# save all results ---------------------------------------------------

if (is.null(resPrev)) {
  cat('\nStart with fresh results from scratch!\n')
  saveRDS(resCandidates, file = RES_FILEN[["res"]])
  saveRDS(resDataCandidates, file = RES_FILEN[["data"]])
} else {
  if (haveResDataFiles) {
    cat(
      "Sorry, merging results with previous results is not supported when we have previous DATA result files as well!\n"
    )
    q(save = "no")
  }

  # check for duplicates
  resDuplicates <- intersect(names(resPrev), names(resCandidates))
  if (length(resDuplicates)) {
    cat(
      glue(
        "These temporary result dataframes are already stored in the {myResultsTag}-results file:\n  * ",
        "{paste(resDuplicates, collapse = '\n  * ')}",
        .trim = FALSE
      ),
      "\n\n"
    )
    cat(
      'Please clean up temporary results files that are already saved in result list, first!\n'
    )
    q(save = 'no')
  }
  cat("\nAdd result candidates to existing result list!\n")
  saveRDS(c(resPrev, resCandidates), file = RES_FILEN[["res"]])
}

if (myRemoveTemp) {
  cat("\n")
  cat("About to remove temporary result files!\n")
  fnRmv <- file.remove(simResFileNames)

  if (sum(fnRmv)) {
    cat("Successfully removed ", sum(fnRmv), " temporary result RDS-files.\n")
    cat("\n  * ")
    cat(paste(simResFileNames[fnRmv], collapse = "\n  * "))
  } else {
    cat("No temporary result RDS-files were removed!\n")
  } #esle

  if (haveResDataFiles) {
    fnRmv <- file.remove(simResDataFileNames)
    if (sum(fnRmv)) {
      cat(
        "Successfully removed ",
        sum(fnRmv),
        " temporary result DATA files.\n"
      )
      cat("\n  * ")
      cat(paste(simResDataFileNames[fnRmv], collapse = "\n  * "))
    } else {
      cat("No temporary result DATA files were removed!\n")
    } #esle
  }
  cat("\n")
} #fi

cat("\n~fine~\n")
