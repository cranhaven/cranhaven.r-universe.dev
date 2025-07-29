#' summary.nprcgenekeeprErr Summary function for class nprcgenekeeprErr
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Object of class summary.nprcgenekeeprErr
#'
#' @rdname summary
#' @method summary nprcgenekeeprErr
#' @param object object of class nprcgenekeeprErr and class list
#' @param ... additional arguments for the \code{summary.default} statement
#' @importFrom stringi stri_c stri_length
## ##  rmsutilityr get_and_or_list
#' @export
#' @examples
#' errorList <- qcStudbook(nprcgenekeepr::pedOne,
#'   minParentAge = 0,
#'   reportChanges = TRUE,
#'   reportErrors = TRUE
#' )
#' summary(errorList)
summary.nprcgenekeeprErr <- function(object, ...) {
  errorLst <- object
  stopifnot(inherits(errorLst, "nprcgenekeeprErr"))
  txt <- ""
  txt <- addErrTxt(
    txt,
    errorLst$missingColumns,
    "Error: The missing column is",
    "Error: The missing columns are"
  )
  if (length(errorLst$missingColumns) > 0L) {
    txt <- stri_c(
      txt,
      " The required columns are: ",
      get_and_or_list(getRequiredCols()),
      ".\n"
    )
  }
  if (length(errorLst$invalidDateRows) > 5L) {
    manyErrorsTxt <- stri_c(
      "There are ",
      length(errorLst$invalidDateRows),
      " rows having an ",
      "invalid date. The first five records having bad dates are on rows ",
      get_and_or_list(errorLst$invalidDateRows[1L:5L]),
      ".\n"
    )
    txt <- stri_c(txt, manyErrorsTxt)
  } else if (length(errorLst$invalidDateRows) > 0L) {
    txt <- addErrTxt(
      txt,
      errorLst$invalidDateRows,
      "Error: There is one row having an invalid date. It is",
      stri_c(
        "Error: There are ",
        length(errorLst$invalidDateRows),
        " rows having an invalid date. The rows having an ",
        "invalid date are"
      )
    )
  }
  txt <- addErrTxt(
    txt,
    errorLst$sireAndDam,
    "Error: The animal listed as both a sire and dam is",
    "Error: The animals listed as both sire and dam are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$femaleSires,
    "Error: The animal listed as a sire and also listed as a female is",
    "Error: The animals listed as sires and also listed as females are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$maleDams,
    "Error: The animal listed as a dam and also listed as a male is",
    "Error: The animals listed as dams and also listed as males are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$duplicateIds,
    "Error: The animal listed more than once is",
    "Error: The animals listed more than once are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$caseChange,
    "Change: The column where case was changed is",
    "Change: The columns where case was changed are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$spaceRemoved,
    "Change: The column where space was removed is",
    "Change: The columns where space was removed are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$periodRemoved,
    "Change: The column where period was removed is",
    "Change: The columns where period was removed are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$underScoreRemoved,
    "Change: The column where underscore was removed is",
    "Change: The columns where underscore was removed are"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$egoToId,
    "Change: The column changed from",
    "Change: The columns changed from"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$egoidToId,
    "Change: The column changed from",
    "Change: The columns changed from"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$sireIdToSire,
    "Change: The column changed from",
    "Change: The columns changed from"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$damIdToDam,
    "Change: The column changed from",
    "Change: The columns changed from"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$birthdateToBirth,
    "Change: The column changed from",
    "Change: The columns changed from"
  )
  txt <- addErrTxt(
    txt,
    errorLst$changedCols$deathdateToDeath,
    "Change: The column changed from",
    "Change: The columns changed from"
  )
  if (stri_length(txt) > 0L) {
    txt <-
      stri_c(txt, "\nPlease check and correct the pedigree file.\n")
  }
  if (length(errorLst$failedDatabaseConnection) > 0L) {
    txt <-
      stri_c(txt, "\n", errorLst$failedDatabaseConnection, "\n")
  }
  txt <- list(txt = txt, sp = errorLst$suspiciousParents)

  class(txt) <- "summary.nprcgenekeeprErr"
  txt
}
#' @rdname summary
#' @return object of class summary.nprcgenekeeprGV
#' @method summary nprcgenekeeprGV
#' @importFrom stringi stri_c
#' @export
#' @examples
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' breederPed <- qcStudbook(examplePedigree,
#'   minParentAge = 2L,
#'   reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' focalAnimals <- breederPed$id[!(is.na(breederPed$sire) &
#'   is.na(breederPed$dam)) &
#'   is.na(breederPed$exit)]
#' ped <- setPopulation(ped = breederPed, ids = focalAnimals)
#' trimmedPed <- trimPedigree(focalAnimals, breederPed)
#' probands <- ped$id[ped$population]
#' ped <- trimPedigree(probands, ped,
#'   removeUninformative = FALSE,
#'   addBackParents = FALSE
#' )
#' geneticValue <- reportGV(ped,
#'   guIter = 50L, # should be >= 1000L
#'   guThresh = 3L,
#'   byID = TRUE,
#'   updateProgress = NULL
#' )
#' trimmedGeneticValue <- reportGV(trimmedPed,
#'   guIter = 50L, # should be >= 1000L
#'   guThresh = 3L,
#'   byID = TRUE,
#'   updateProgress = NULL
#' )
#' summary(geneticValue)
#' summary(trimmedGeneticValue)
summary.nprcgenekeeprGV <- function(object, ...) {
  gvReport <- object
  stopifnot(inherits(gvReport, "nprcgenekeeprGV"))
  rpt <- gvReport[["report"]]
  # Not currently including kmat; may add later
  kmat <- gvReport[["kinship"]] # nolint: object_usage_linter
  f <- gvReport[["total"]]
  mf <- gvReport[["nMaleFounders"]]
  ff <- gvReport[["nFemaleFounders"]]
  fe <- gvReport[["fe"]]
  fg <- gvReport[["fg"]]
  txt <- "The genetic value report"
  txt <- c(txt, stri_c("Individuals in Pedigree: ", nrow(rpt)))
  txt <-
    c(
      txt,
      stri_c(
        "Male Founders: ",
        mf,
        "\nFemale Founders: ",
        ff,
        "\nTotal Founders: ",
        f
      )
    )
  txt <- c(txt, stri_c("Founder Equivalents: ", round(fe, 2L)))
  txt <-
    c(txt, stri_c("Founder Genome Equivalents: ", round(fg, 2L)))
  txt <-
    c(txt, stri_c("Live Offspring: ", sum(rpt$livingOffspring)))
  txt <-
    c(txt, stri_c(
      "High Value Individuals: ",
      nrow(rpt[rpt$value == "High Value", ])
    ))
  txt <-
    c(txt, stri_c(
      "Low Value Individuals: ",
      nrow(rpt[rpt$value == "Low Value", ])
    ))
  class(txt) <- "summary.nprcgenekeeprGV"
  txt
}
