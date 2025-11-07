new_ParsedModel <- function(params, preds, mFormula, kFormula) {
  stopifnot(is.character(params)) # character vector of parameters
  stopifnot(is.character(preds)) # character vector of predictors
  stopifnot(is.character(mFormula) && length(mFormula) == 1) # string containing the mean function
  stopifnot(is.character(kFormula) && length(kFormula) == 1) # string containing the covariance function

  structure(
    list(
      params = params,
      preds = preds,
      mFormula = mFormula,
      kFormula = kFormula
    ),
    class = "ParsedModel"
  )
}

validate_parseModel <- function(mFormula, kFormula, myData) {
  stopifnot(is.character(mFormula) && length(kFormula) == 1) # string containing the mean function
  stopifnot(is.character(mFormula) && length(kFormula) == 1) # string containing the covariance function
  stopifnot(class(myData) == "StanData") # data set
}

betterRegMatches <- function(modString, resGreg, value = " ") {
  stopifnot(length(resGreg) == 1)
  resGreg <- resGreg[[1]]
  for (i in seq_len(length(resGreg))) {
    substr(modString, resGreg[i], resGreg[i] + attributes(resGreg)$match.length[i] - 1) <- paste0(rep(value, attributes(resGreg)$match.length[i]), collapse = "")
  }
  modString
}

extractParamsPreds <- function(myFormula, myData) {
  ## constants
  specialChar <- "#"
  splitters <- c("[[:alnum:]]*\\(", "\\(", "\\)", "\\+", "\\-", "\\*", "/",
                 "%", "<", "<=", ">", ">=", "==", "!=", "!", "&&", "||",
                 "\\^", ","
                 )

  ## detect and remove reserved characters used for functions, operators etc.
  # detect
  regExp <- paste0(splitters, collapse = "|")
  grepRes <- gregexpr(regExp, myFormula)

  # remove
  newFormula <- myFormula
  if (grepRes[[1]][1] != -1) { # any match
    newFormula <- betterRegMatches(newFormula, grepRes)
  }

  ## detect and remove all variables that are in the predictor matrix and thus not parameters
  # detect
  dataNames <- colnames(myData$X[[1]])
  dataNames <- c(dataNames, paste0(dataNames, specialChar)) # add special char
  regExp <- paste0(dataNames, "|", collapse = "") # looking for all chars
  regExp <- substr(regExp, 1, nchar(regExp) - 1)
  regExp <- paste0("(?<=^| )(", regExp, ")(?=$| )", collapse = "") # only look for stuff with whitespace before and after
  predPos <- gregexpr(regExp, newFormula, perl = TRUE)
  preds <- regmatches(newFormula, predPos)[[1]]

  # remove
  if (length(preds) > 0) {
    preds <- gsub(specialChar, "", preds)
    preds <- unique(preds)
    newFormula <- betterRegMatches(newFormula, predPos)
  }

  ##  extract remaining tokens as parameters
  newFormula <- gsub("^[[:space:]]+", "", newFormula) # remove spaces at the beginning
  params <- strsplit(newFormula, "[[:space:]]+")[[1]]
  isnotNumber <- suppressWarnings(is.na(as.double(params)))
  params <- params[isnotNumber]
  params <- unique(params)

  if (predPos[[1]][1] == -1) {
    predPos <- c()
  } else {
    predPos <- predPos[[1]]
  }
  list(params = params, preds = preds, predPos = predPos)
}

createStanFormula <- function(preds, myFormula, myData) {
  # constants
  replaceTemplate <- "X[i,timeIndex,predIndex]"
  secondTimePointChar <- "#"

  toReplace <- preds
  forReplacing <- preds
  newFormula <- myFormula
  for (i in seq_len(length(toReplace))) {
    thePred <- substr(myFormula, toReplace[i], toReplace[i] + attr(toReplace, "capture.length")[i] - 1)


    # find correct time indexn
    isSecond <- grepl(secondTimePointChar, thePred)
    if (isSecond) {
      timeIndex <- "k"
      thePredRaw <- gsub(secondTimePointChar, "", thePred)
    } else {
      timeIndex <- "j"
      thePredRaw <- thePred
    }

    # find correct predictor index
    predIndex <- which(colnames(myData$X[[1]]) == thePredRaw)

    # replace stuff
    replacer <- gsub("timeIndex", timeIndex, replaceTemplate)
    replacer <- gsub("predIndex", predIndex, replacer)
    newFormula <- paste0(substr(newFormula, 1, forReplacing[i] - 1), replacer, substr(newFormula, forReplacing[i] + attr(forReplacing, "capture.length")[i], nchar(newFormula)))
    forReplacing <- forReplacing + nchar(replacer) - attr(forReplacing, "capture.length")[i]
  }
  newFormula
}

parseFormula <- function(myFormula, myData) {
  paramsPreds <- extractParamsPreds(myFormula, myData)
  newFormula <- createStanFormula(paramsPreds$predPos, myFormula, myData)
  list(params = paramsPreds$params, preds = paramsPreds$preds, newFormula = newFormula)
}

parseModel <- function(mFormula, kFormula, myData) {
  validate_parseModel(mFormula, kFormula, myData)
  meanRes <- parseFormula(mFormula, myData)
  covRes <- parseFormula(kFormula, myData)
  allParams <- union(meanRes$params, covRes$params)
  allPreds <- union(meanRes$preds, covRes$preds)
  new_ParsedModel(allParams, allPreds, meanRes$newFormula, covRes$newFormula)
}
