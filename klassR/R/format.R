## to do:
# level input need and run level check first but the formatted input is need to check level....FIX

#' Format vector for industry codes
#' @param x - vector of character
#' @param classification - classification number
#' @return vector of character
#' @keywords internal
formattering <- function(x, classification) {
  # Check for missing values
  miss <- sum(is.na(x) | x == "")
  if (miss != 0) {
    warning(c("Number of NA: ", miss))
  }

  # Check and format
  if (classification == 6) {
    x_formatted <- formattering_nace(x)
  }
  if (classification == 131) {
    x_formatted <- formattering_kommune(x)
  }

  x_formatted
}


#' Format vector for industry codes
#' @param x Character vector
#' @return Formatted charcter vector
#' @keywords internal
formattering_nace <- function(x) {
  # Specify position of the .
  dot <- 3

  # Identify number missing punctuation
  x_nopunc <- tm::removePunctuation(x)
  x_over2 <- nchar(x_nopunc) >= 3
  x_over2[is.na(x_over2)] <- FALSE
  mangler_dot <- sum(!grepl("\\.", x[x_over2]))

  if (mangler_dot != 0) {
    warning(c("Number missing .: ", mangler_dot))
  }

  mangler_dot <- length(x) - sum(grepl("\\.", x))

  # Format
  x_formatted <- sub(paste0("(?<=.{", dot - 1, "})"), ".", x_nopunc, perl = TRUE)
  x_formatted[!x_over2] <- x_nopunc[!x_over2]

  x_formatted
}

#' Format vector for kommune codes
#' @param x - vector of character type for kommune codes
#' @return vector of character
#' @keywords internal
formattering_kommune <- function(x) {
  # Check for letters
  if (any(grepl("^[A-Za-z]+$", x))) {
    stop("Letters detected in kommune codes. Please check and rerun.")
  }

  # Find dropped leading zeros
  mangler0 <- sum(nchar(x) < 4, na.rm = T)

  # Fix if any
  if (mangler0 > 0) {
    x_formatted <- formatC(as.numeric(x), width = 4, flag = "0")
    warning(c("Number missing leading 0: ", mangler0))
  } else {
    x_formatted <- x
  }
  x_formatted
}
