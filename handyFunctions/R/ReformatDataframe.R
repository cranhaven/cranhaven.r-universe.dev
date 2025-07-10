
#' Return reformatted data.frame with standard col names
#'
#' @param rawDataFrame Raw data.frame input
#' @param cols Specific col names or indexes what you want to reformat (default: TRUE, use all cols)
#' @param rawSep Raw odd separation symbol in col names of raw data.frame.
#' Note: it supports regEx (regular expression), so "." means all possible symbols.
#' If you want to use the "." dot notation, please use "[.]".
#'
#' @param sep Separation symbol in col names of modified data.frame
#'
#' @return A modified data.frame with col names separated by your given delimitator
#' @export
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' modified_people <- modifyColNames(people,rawSep = "[.][.]")
#'
modifyColNames <- function(rawDataFrame, cols = TRUE, rawSep = "..", sep = "_") {
  # rawDataFrame <<- rawDataFrame
  modifiedDataFrame <- rawDataFrame
  rawColNames <- colnames(rawDataFrame)
  rawColNamesIndex <- checkCols(rawDataFrame, cols)
  if (rawColNamesIndex[1] == FALSE) {
    return(FALSE)
  }
  rawColNamesModified <- rawColNames[rawColNamesIndex]
  # print(rawColNames)
  modifiedColNames <- unlist(lapply(rawColNamesModified, function(x) {
    temp <- stringr::str_trim(stringr::str_replace_all(x, rawSep, " "))
    # temp=strsplit(x,rawSep,fixed = T)[[1]];
    return(stringr::str_replace_all(temp, " ", sep))
  }))
  # print(modifiedColNames)

  colnames(modifiedDataFrame)[rawColNamesIndex] <- modifiedColNames
  # print(modifiedDataFrame)
  return(modifiedDataFrame)
}

#' check the validation and return index of cols given from input in rawDataFrame
#'
#' @param rawDataFrame raw data.frame
#' @param cols specific cols given from input
#'
#' @return return validation (only FALSE if invaild cols input) or index of cols
#' @export
#'
#' @importFrom methods is
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' checkCols(people, c("..name", "..sex"))
#' # OR
#' checkCols(people, c(1, 2))
checkCols <- function(rawDataFrame, cols) {
  rawColNames <- colnames(rawDataFrame)
  if (is(cols,"character") | is(cols,"numeric") & length(cols) > 1) {
    if (is(cols,"character")) {
      ColNamesIndex <- unlist(lapply(cols, function(x) {
        which(rawColNames == x)
      }))
    } else {
      ColNamesIndex <- cols
    }
  } else if (cols == T | cols == "TRUE" | cols == "T") {
    ColNamesIndex <- 1:length(colnames(rawDataFrame))
  } else {
    message("Error, cols is not legal!")
    return(FALSE)
  }
  return(ColNamesIndex)
}

#' Return suggested dtype of vector input
#'
#' @param vector vector/list input
#'
#' @return Return suggested dtypes of vector
#' @export
#'
#' @examples
#' library(handyFunctions)
#' vector <- c(1, 2, 3, "", NA, "  ", "four", "NA", 5)
#' checkDtype(vector)
#'
checkDtype <- function(vector) {
  # vector = temp[,1]
  # validVector = unlist(vector)
  # N = length(vector)
  # vector = c('sd ds','sds sd','ad sd')
  vector <- c(1, 2, 3, "", NA, "  ", "four", "NA", 5)
  vector <- as.character(unlist(vector))
  check_sum <- sum(unlist(lapply(vector, function(x) {
    !is.na(as.numeric(x))
  })), na.rm = T)
  check_nonsum <- sum(unlist(lapply(vector, function(x) {
    is.na(as.numeric(x))
  })), na.rm = T)
  check_blank <- sum(unlist(lapply(vector, function(x) {
    !is.na(stringr::str_match(x, "^ +$"))
  })), na.rm = T)
  check_null <- sum(unlist(lapply(vector, function(x) {
    x == ""
  })), na.rm = T)
  check_possNA <- sum(unlist(lapply(vector, function(x) {
    x == "NA"
  })), na.rm = T)
  check_NA <- sum(unlist(lapply(vector, function(x) {
    is.na(x)
  })), na.rm = T)

  N_num <- check_sum
  N_char <- check_nonsum - check_NA - check_possNA - check_null - check_blank
  if (N_char > 0) {
    possDtype <- "char"
  } else if (N_char == 0) {
    possDtype <- "num"
  }
  # if (max(N_num,N_char)<N){
  #   poss = FALSE
  # } else {
  #   poss = TRUE
  # }
  return(possDtype)
}


#' Return suggested appropriate dtypes for each column in rawDataFrame
#'
#' @param rawDataFrame Raw data.frame
#' @param cols Specify cols which you want to change its dtypes when custom is FALSE (default: TRUE, for all cols)
#' @param dtype Specify indexed matched dtypes whcih you want to update when custom is FALSE (default: FALSE, for automatically update)
#' @param custom Option whether set to auto/custom , you can specify your custom dtypes for cols given when setting to TRUE (default: FALSE, for auto)
#'
#' @return Return a new data.frame with appropriate dtypes suggested for each cols
#' @export
#'
#' @importFrom methods is
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' modifyColTypes(people)
#'
modifyColTypes <- function(rawDataFrame, cols = TRUE, dtype = FALSE, custom = FALSE) {
  # get col names
  allColNames <- colnames(rawDataFrame)
  ColNamesIndex <- checkCols(rawDataFrame, cols)
  if (ColNamesIndex[1] == FALSE) {
    return(FALSE)
  }
  # ColNamesIndex =
  if (custom == T) {
    if (dtype == FALSE) {
      message("Error, dtype is FALSE:\n You must specify the data type of the given cols!")
      return(FALSE)
    } else if (is(dtype, "character") & length(dtype) >= 1) {
      if (length(dtype) == length(cols)) {
        for (i in 1:length(ColNamesIndex)) {
          if (!is.na(stringr::str_match(dtype[i], "num"))) {
            rawDataFrame[, ColNamesIndex[i]] <- as.numeric(rawDataFrame[, ColNamesIndex[i]])
          } else if (!is.na(stringr::str_match(dtype[i], "char"))) {
            rawDataFrame[, ColNamesIndex[i]] <- as.character(rawDataFrame[, ColNamesIndex[i]])
          } else {
            message(paste0("Error,", i, "-indexed value in dtype is not legal, which should contain num or char!"))
            return(FALSE)
          }
        }
      } else {
        message("Error, dtype is not match with cols:\n The indexes of dtype must be matched with that of cols!")
        return(FALSE)
      }
    }
  }
  if (custom == F) {
    for (i in 1:length(ColNamesIndex)) {
      if (checkDtype(rawDataFrame[, i]) == "num") {
        rawDataFrame[, ColNamesIndex[i]] <- as.numeric(rawDataFrame[, ColNamesIndex[i]])
      } else if (checkDtype(rawDataFrame[, i]) == "char") {
        rawDataFrame[, ColNamesIndex[i]] <- as.character(rawDataFrame[, ColNamesIndex[i]])
      } else {
        warning(paste0("Warning, the ", i, "-indexed col (", allColNames[ColNamesIndex[i]], ") cannot be detected as approriate dtype!"))
        return(FALSE)
      }
    }
  }

  return(rawDataFrame)
}

#' Return reformatted data.frame with standard row names
#'
#' @param rawDataFrame Raw data.frame input
#' @param rows Specific row names or indexes what you want to reformat (default: TRUE, use all row)
#' @param rawSep Raw odd separation symbol in row names of raw data.frame.
#' Note: it supports regEx (regular expression), so "." means all possible symbols.
#' If you want to use the "." dot notation, please use "[.]".
#' @param sep Separation symbol in row names of modified data.frame
#'
#' @return A modified data.frame with row names separated by your given delimitator
#' @export
#'
#' @importFrom methods is
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' modifyRowNames(people)
#'
modifyRowNames <- function(rawDataFrame, rows = TRUE, rawSep = "..", sep = "_") {
  # rawDataFrame <<- rawDataFrame
  modifiedDataFrame <- rawDataFrame
  rawRowNames <- rownames(rawDataFrame)
  if (is(rows, "character") | is(rows,"numeric") & length(rows) > 1) {
    rawRowNamesIndex <- unlist(lapply(rows, function(x) {
      which(rawRowNames == x)
    }))
  } else if (rows == T | rows == "TRUE" | rows == "T") {
    rawRowNamesIndex <- 1:length(rownames(rawDataFrame))
  }
  rawRowNamesModified <- rawRowNames[rawRowNamesIndex]
  modifiedRowNames <- unlist(lapply(rawRowNamesModified, function(x) {
    temp <- stringr::str_trim(stringr::str_replace_all(x, rawSep, " "))
    # temp=strsplit(x,rawSep,fixed = T)[[1]];
    return(stringr::str_replace_all(temp, " ", sep))
  }))

  rownames(modifiedDataFrame)[rawRowNamesIndex] <- modifiedRowNames
  # print(modifiedDataFrame)
  return(modifiedDataFrame)
}


#' Reformat dataframe with the all modifiers simultaneously (colNames, rowNames and dtypes)
#'
#' @param rawDataFrame raw data.frame
#' @param rawRowSep raw separation deliminator of row names in raw data.frame
#' @param rowSep the new separation deliminator of row names
#' @param rawColSep raw separation deliminator of col names in raw data.frame
#' @param colSep the new separation deliminator of col names
#' @param changeDtype if change the dtypes of cols
#'
#' @return A modified data.frame with applied to above all modifiers
#' @export
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' unifyDataframe(people,rawColSep = "[.][.]")
#'
unifyDataframe <- function(rawDataFrame, rawRowSep = "..", rowSep = "_", rawColSep = "..", colSep = "_", changeDtype = TRUE) {
  modifiedDataFrame <- modifyColNames(rawDataFrame, cols = T, rawSep = rawColSep, sep = colSep)
  modifiedDataFrame <- modifyRowNames(modifiedDataFrame, rows = T, rawSep = rawColSep, sep = colSep)
  if (changeDtype == T) {
    modifiedDataFrame <- modifyColTypes(modifiedDataFrame)
  }
  return(modifiedDataFrame)
}


#' Return specific-indexed vector according to given delimitator/separator by splitting one col in data.frame
#'
#' @param data vector or data.frame input
#' @param col the col names or indexes if data.frame input
#' @param sep separation deliminator
#' @param index the index of symbol which you want
#' @param fixed logical. If TRUE match split exactly, otherwise use regular expressions, detailed info can be seen in \href{/library/base/help/strsplit}{strsplit}.
#'
#' @return specific-indexed vector or factor
#' @export
#'
#' @importFrom methods is
#'
#' @examples
#' library(handyFunctions)
#' data(people)
#' splitCol(people, col = 1, sep = " ", index = 2)
#'
splitCol <- function(data, col = FALSE, sep, index, fixed = TRUE) {
  level <- NULL
  isFactor <- FALSE

  if (is(data, "data.frame")) {
    if (col == FALSE) {
      message("Error: please specify the col!")
      return(FALSE)
    } else {
      inputVector <- data[, col]
    }
  } else if (is(data, "character")) {
    inputVector <- data
  } else if (is(data, "factor")) {
    # warning("The class of ",substitute(data), 'is factor!\n It has been changed into character dtype!')
    # inputVector = as.character(data)
    inputVector <- data
  } else {
    inputVector <- NULL
    message(paste0("Error: ", substitute(data), " is NULL!"))
    return(FALSE)
  }

  if (is(inputVector, "factor")) {
    isFactor <- TRUE
    level <- levels(inputVector)
    inputVector <- as.character(inputVector)
  }

  outputVector <- unlist(lapply(inputVector, function(x) {
    strsplit(x, sep, fixed)[[1]][index]
  }))
  if (isFactor == T) {
    return(factor(outputVector, levels = level))
  } else {
    return(outputVector)
  }
}
