#' Conversion to character
#'
#' @param x a number or vector of numbers
#' @keywords internal
#' @return x converted to a string or vector of strings.
MakeChar <- function(x) {
  if (length(x) == 1) {
    xnew <- as.character(x)
  }
  if (length(x) > 1) {
    xnew <- sapply(x, FUN = MakeChar)
  }
  return(xnew)
}


#' Match and convert a classification
#'
#' @param x Input vector of classification codes. Vector must match "code" column from a call to get_klass().
#' @param classification Classification number
#' @param date String for the required date of the classification. Format must be "yyyy-mm-dd". For an inverval, provide two dates as a vector. If blank, will default to today's date.
#' @param variant The classification variant to fetch (if a variant is wanted).
#' @param correspond ID number for target in correspondence table. For correspondence between two dates within the same classification, use correspond = TRUE.
#' @param language Default "nb" for Norwegian (Bokmål). Also "nn" (Nynorsk) and "en" (English available for some classifications)
#' @param output_level Desired output level
#' @param output String describing output. May be "name" (default), "code" or "both".
#' @param format Logical for whther to run formatting av input vector x (Default = TRUE), important to check if formatting is in one level.
#'
#' @return A vector or data frame is returned with names and/or code of the desired output level.
#' @export
#'
#' @examples
#' data(klassdata)
#' kommune_names <- apply_klass(
#'   x = klassdata$kommune,
#'   classification = 131,
#'   language = "en",
#'   format = FALSE
#' )
apply_klass <- function(x,
                        classification,
                        date = NULL,
                        variant = NULL,
                        correspond = NULL,
                        language = "nb",
                        output_level = NULL,
                        output = "name",
                        format = TRUE) {
  # Check and standardise variables
  classification <- MakeChar(classification)
  if (is.null(x)) {
    stop("The input vector is empty.")
  }
  x <- MakeChar(x)

  if (is.null(date)) {
    date <- Sys.Date()
  }

  type <- ifelse(is.null(correspond), "vanlig", "kor")
  type <- ifelse(isTRUE(correspond), "change", type)
  type <- ifelse(is.null(variant), type, "variant")

  # Fetch classification table
  klass_data <- get_klass(
    classification,
    date = date,
    correspond = NULL,
    variant = variant,
    language = language,
    output_level = NULL
  )

  # Extract correspondence table
  if (type == "kor") {
    cor_table <- get_klass(
      classification,
      date = date,
      correspond = correspond,
      language = language
    ) # , output_level = output_level)

    new_table <- get_klass(
      classification = correspond,
      date = date,
      correspond = NULL,
      language = language
    ) # , output_level = output_level)
  }
  if (type == "change") {
    cor_table <- get_klass(
      classification = classification,
      date = date,
      correspond = TRUE,
      language = language,
      output_level = NULL
    )
  }

  # Formattering - only for nace and municipality
  if (format == TRUE & classification %in% c("6", "131")) {
    x_formatted <- formattering(x, classification = classification)
  } else {
    x_formatted <- x
  }

  # Check input data for level
  input_level <- levelCheck(x = x_formatted, klass_data = klass_data) # implies all are same level!
  if (is.null(output_level)) {
    output_level <- input_level
  }

  if (!all(input_level == output_level) &
    type %in% c("kor", "change")) {
    stop("Level changes and time changes/correspondence concurrently is not programmed.")
  }

  # Run level function
  if (!all(input_level == output_level) & is.null(correspond)) {
    x_level <- Levels(
      input_level = input_level,
      output_level = output_level,
      klass_data = klass_data
    )
  }

  if (all(input_level == output_level)) {
    x_level <- klass_data[klass_data$level == input_level, ]
    x_level[, paste("level", input_level, sep = "")] <- x_level$code
    x_level[, paste("name", input_level, sep = "")] <- x_level$name
  }


  # run matching
  levelcode <- paste("level", input_level, sep = "")
  if (type %in% c("vanlig", "variant")) {
    m <- match(x_formatted, x_level[, levelcode]) ### sjekk rekkefolge
  }

  if (type == "kor") {
    m1 <- match(x_level[, levelcode], cor_table[, "sourceCode"])
    m2 <- match(x_formatted, cor_table[, "sourceCode"])
    m3 <- match(cor_table[, "targetCode"], new_table[, "code"]) ## ?
  }

  if (type == "change") {
    m1 <- match(x_formatted, x_level[, levelcode])
    m2 <- match(x_formatted, cor_table$sourceCode)
  }

  # Choose format output
  if (type %in% c("vanlig", "variant")) {
    if (output == "code") {
      vars <- paste("level", output_level, sep = "")
    }
    if (output == "name") {
      vars <- paste("name", output_level, sep = "")
    }
    if (output == "both") {
      vars <- paste(c("level", "name"), output_level, sep = "")
    }
    out <- x_level[m, vars]
  }
  if (type == "kor") {
    if (output == "code") {
      vars <- "targetCode"
      vars2 <- "code"
    }
    if (output == "name") {
      vars <- "targetName"
      vars2 <- "name"
    }
    if (output == "both") {
      vars <- c("targetCode", "targetName")
      vars2 <- c("code", "name")
    }
    out <- cor_table[m2, vars]
  }
  if (type == "change") {
    if (output == "code") {
      vars <- "targetCode"
      vars2 <- "code"
    }
    if (output == "name") {
      vars <- "targetName"
      vars2 <- "name"
    }
    if (output == "both") {
      vars <- c("targetCode", "targetName")
      vars2 <- c("code", "name")
    }
    out <- ifelse(!is.na(m2), cor_table[m2, vars], klass_data[m1, vars2])
  }
  return(out)
}


#' @rdname apply_klass
#' @param klass Deprecated; use `classification` instead.
#' @export
ApplyKlass <- function(x,
                       klass,
                       date = NULL,
                       variant = NULL,
                       correspond = NULL,
                       language = "nb",
                       output_level = NULL,
                       output = "name",
                       format = TRUE) {
  # .Deprecated("apply_klass") # add in for future versions
  apply_klass(
    x = x,
    classification = klass,
    date = date,
    variant = variant,
    correspond = correspond,
    language = language,
    output_level = output_level,
    output = output,
    format = format
  )
}
