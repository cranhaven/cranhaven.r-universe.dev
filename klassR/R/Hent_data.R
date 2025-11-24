#' Internal function to check date
#' @param date Date
#' @keywords internal
#' @return NULL
CheckDate <- function(date) {
  dcheck <- try(as.Date(date, format = "%Y-%m-%d"))
  if (!inherits(dcheck, "Date") | is.na(dcheck)) {
    stop("An incorrect date format was given. Please use format 'YYYY-mm-dd'.")
  }
  if (dcheck < "1500-01-01") {
    stop("An incorrect date format was given. Please use format 'YYYY-mm-dd'.")
  }
}


#' Internal function to create URL address
#'
#' @param classification Classification number
#' @param correspond Target number for correspondence table
#' @param variant_name The name of the variant of the classification
#' @param type String describing type. "vanlig" for normal classification and "kor" for correspondence. Default = "vanlig"
#' @param fratil True/False for whether a date interval is to be used. Default = False
#' @param date Date(s) for classification
#' @param output_level_coding Coding for output level
#' @param language_coding Coding for language
#' @keywords internal
#'
#' @return String url adress
MakeUrl <- function(classification, correspond = NULL, correspondID = NULL,
                    variant_name = NULL,
                    type = "vanlig",
                    fratil = FALSE, date = NULL,
                    output_level_coding = NULL,
                    language_coding = NULL) {
  # Standard classification/codelist
  if (type == "vanlig" & fratil == TRUE) {
    coding <- paste0("/codes?from=", date[1], "&to=", date[2])
  }
  if (type == "vanlig" & fratil == FALSE) {
    coding <- paste0("/codesAt?date=", date)
  }


  # For correspondence tables
  if (type == "kor" & fratil == TRUE) {
    coding <- paste("/corresponds?targetClassificationId=", MakeChar(correspond), "&from=", date[1], "&to=", date[2], sep = "")
  }
  if (type == "kor" & fratil == FALSE) {
    coding <- paste("/correspondsAt?targetClassificationId=", MakeChar(correspond), "&date=", date, sep = "")
  }
  if (type == "korID") {
    coding <- paste0("correspondencetables/", MakeChar(correspondID))
    # Tables with given ID can not be combined with other parameters in the call
    classification <- ""
    language_coding <- ""
    output_level_coding <- ""
  }

  # For time changes
  if (type == "change") {
    coding <- paste0("/changes?from=", date[1], "&to=", date[2])
  }

  # For fetching a variant
  if (type == "variant" & fratil == TRUE) {
    coding <- paste0("/variant?variantName=", variant_name, "&from=", date[1], "&to=", date[2])
  }
  if (type == "variant" & fratil == FALSE) {
    coding <- paste0("/variantAt?variantName=", variant_name, "&date=", date)
  }

  # For future times
  idag <- Sys.Date()
  if ((idag < date[1]) & (type != "korID")) {
    message("The date you selected is in the future. You may be viewing a future classification that is not currently valid")
    coding <- paste0(coding, "&includeFuture=True")
  } else if ((length(date) > 1) & (type != "korID")) {
    if (idag < date[2]) {
      message("The date you selected is in the future. You may be viewing a future classification that is not currently valid")
      coding <- paste0(coding, "&includeFuture=True")
    }
  }

  # Whether classifications is needed in url
  if (type == "korID") {
    classifics <- ""
  } else {
    classifics <- "classifications/"
  }


  # Paste together to an URL
  url <- paste(GetBaseUrl(),
    classifics,
    classification,
    coding,
    output_level_coding,
    language_coding,
    sep = ""
  )
  return(url)
}


#' Check connection
#' Function to check that a connection to data.ssb.no is able to be established
#' @param url String url address for connection to check
#' @keywords internal
#' @return Nothing is returned but a error or warning message is return if no connection is available
check_connect <- function(url) {
  tryget <- tryCatch(
    httr::GET(url = url),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  if (!inherits(tryget, "response")) {
    message(tryget)
    return(invisible(NULL))
  } else if (httr::http_error(tryget$status_code)) {
    message(paste("Connection failed with error code", tryget$status_code))
    return(invisible(NULL))
  }
  tryget
}


#' Stop quietly function
#' Stop from a function without an error. Used for stopping when no internet
#' @keywords internal
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


#' Get variant name
#' Internal function for fetching the variant name based on the number
#' @param variant The variant number
#' @keywords internal
get_variant_name <- function(variant) {
  # Check variant url and that it exists
  url <- paste0(GetBaseUrl(), "variants/", variant)
  variant_url <- check_connect(url)
  if (is.null(variant_url)) stop_quietly()

  # Extract text with variant name
  variant_text <- httr::content(variant_url, "text", encoding = "UTF-8") ####
  if (grepl("variant not found", variant_text)) {
    stop("The variant ", variant, " was not found.")
  }
  variant_name_full <- jsonlite::fromJSON(variant_text, flatten = TRUE)$name
  # Earlier have just taken first word before numbers but this is not working so use whole name
  # name_sm <- strsplit(variant_name_full, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = T)[[1]][1]

  # Substitute for spaces, and norwegian characters
  variant_name_full <- gsub(" ", "%20", variant_name_full)
  variant_name_full <- gsub("\u00E6", "%C3%A6", variant_name_full)
  variant_name_full <- gsub("\u00F8", "%C3%B8", variant_name_full)
  variant_name_full <- gsub("\u00E5", "%C3%A5", variant_name_full)
  variant_name_full <- gsub("\\(", "%28", variant_name_full)
  variant_name_full <- gsub("\\)", "%29", variant_name_full)

  variant_name_full
}


#' Get json file from Url - alternative version
#'
#' @param url String url address
#' @param check Logical parameter on whether to check if the url exists
#' @keywords internal
#' @return text in json format
GetUrl2 <- function(url, check = TRUE) {
  # Fetch contents from classfication
  if (check) {
    hent_klass <- check_connect(url)
  } else {
    hent_klass <- httr::GET(url = url)
  }
  if (is.null(hent_klass)) {
    return(invisible(NULL))
  }
  klass_text <- httr::content(hent_klass, "text", encoding = "UTF-8") ## deserialisering with httr function
  return(klass_text)
}

#' Fetch Statistics Norway classification data using API
#'
#' @param classification Number/string of the classification ID/number. (use klass_list() to find this)
#' @param date String for the required date of the classification. Format must be "yyyy-mm-dd". For an inverval, provide two dates as a vector. If blank, will default to today's date.
#' @param correspond Number/string of the target classification for correspondence table (if a correspondence table is requested).
#' @param correspondID ID number of the correspondence table to retrieve. Use as an alternative to correspond.
#' @param variant The classification variant to fetch (if a variant is wanted).
#' @param output_level Number/string specifying the requested hierarchy level (optional).
#' @param language Two letter string for the requested language output. Default is Bokmål ("nb"). Nynorsk ("nn") and English ("en") also available for some classification.)
#' @param output_style String variable for the output type. Default is "normal". Specify "wide" for a wide formatted table output.
#' @param notes Logical for if notes should be returned as a column. Default FALSE
#' @param quiet Logical for whether to suppress the printing of the API address. Default TRUE.
#'
#' @return The function returns a data frame of the specified classification/correspondence table. Output variables include:
#' code, parentCode, level, and name for standard lists. For correspondence tables variables include:
#' sourceCode, sourceName, targetCode and targetName. For date correspondence tables variables include:
#' oldCode, oldName, newCode and newName. For "wide" output, code and name with level suffixes is specified.
#' For date ranges, validFromInRequestedRange and validToInRequestedRange give the dates for the classification. Variable ChangeOccured gives the
#' effective date for classification change in classification change tables.
#' @export
#'
#' @examples
#' # Get classification for occupation classifications
#' head(get_klass(classification = "7"))
#' # Get classification for occupation classifications in English
#' head(get_klass(classification = "7", language = "en"))
get_klass <- function(classification,
                      date = NULL,
                      correspond = NULL,
                      correspondID = NULL,
                      variant = NULL,
                      output_level = NULL,
                      language = "nb",
                      output_style = "normal",
                      notes = FALSE,
                      quiet = TRUE) {
  # create type of classification for using later
  type <- ifelse(is.null(correspond) & is.null(correspondID), "vanlig", "kor")
  type <- ifelse(isTRUE(correspond), "change", type)
  type <- ifelse(is.null(correspond) & type == "kor", "korID", type)
  type <- ifelse(is.null(variant), type, "variant")

  # check classification is a character
  if (is.null(correspondID)) {
    classification <- MakeChar(classification)
  } else {
    classification <- ""
  }


  # dato sjekking
  if (!is.null(date[1]) & (!is.null(correspondID))) {
    message("Note: Correspondence tables provided using an ID do not har a date attached. Date is being ignored.")
  }
  if (is.null(date[1])) date <- Sys.Date()

  # Create variables fratil (whether to and from dates should be used) and ver
  if (length(date) == 1 & !is.numeric(date)) {
    fratil <- FALSE # om det er dato fra og til
    ver <- FALSE # om det skal finne en versjon (not implemented yet)
    CheckDate(date)
  }

  if (length(date) == 1 & is.numeric(date)) {
    fratil <- FALSE
    ver <- TRUE
  }

  # if two dates are provided, check both and check order. Swap if neccessary
  if (length(date) == 2) {
    fratil <- TRUE
    ver <- FALSE
    CheckDate(date[1])
    CheckDate(date[2])
    if (difftime(as.Date(date[2]), as.Date(date[1])) < 0) {
      date <- date[c(2, 1)]
      dateswap <- TRUE
    } else {
      dateswap <- FALSE
    }
    date[2] <- as.character(as.Date(date[2], format = "%Y-%m-%d") + 1)
  }
  if (length(date) > 2) stop("You have provided too many dates.")

  # Check levels
  if (is.null(output_level)) {
    output_level_coding <- ""
  } else {
    output_level_coding <- paste("&selectLevel=", output_level, sep = "")
  }

  # Set language coding
  language_coding <- paste("&language=", language, sep = "")

  # Create url and collect data
  if (type == "variant") {
    if (is.numeric(variant) | grepl("^[0-9]", variant)) {
      variant_name <- get_variant_name(variant)
    } else {
      variant_name <- gsub(" ", "%20", variant)
    }
    if (!is.null(output_level)) {
      print("Selecting an output level for a variant isn't currently supported. All levels will be returned")
    }
  }
  url <- MakeUrl(
    classification = classification, correspond = correspond, correspondID = correspondID,
    variant_name = variant_name,
    type = type,
    fratil = fratil, date = date, output_level_coding = output_level_coding,
    language_coding = language_coding
  )
  if (!quiet) {
    print(paste("Fetching class from:", url))
  }
  if (type == "kor") {
    klass_text <- GetUrl2(url, check = FALSE)
    # sjekk at det finnes
    targetswap <- FALSE
    if (grepl("no correspondence table", klass_text)) {
      targetswap <- TRUE
      url <- MakeUrl(
        classification = correspond, correspond = classification, type = type, fratil = fratil, date = date,
        output_level_coding = output_level_coding, language_coding = language_coding
      )
      klass_text <- GetUrl2(url)
      if (grepl("no correspondence table", klass_text)) {
        stop(
          "No correspondence table found between classes ", classification, " and ", correspond, " for the date ", date,
          "For a list of valid correspondence tables use the function correspond_list()"
        )
      }
      if (is.null(klass_text)) stop_quietly()
    }
  } else {
    klass_text <- GetUrl2(url)
  }
  if (is.null(klass_text)) stop_quietly()

  if (grepl("not found", klass_text)) {
    stop("No classification table was found for classification number ", classification, ".
    Please try again with a different classification number.
    For a list of possible classification's use the function list_klass() or list_family()")
  }
  if (grepl("not published in language", klass_text)) {
    stop("The classification requested was not found for language = ", gsub(".*=", "", language_coding))
  }
  if (grepl("does not have a variant named", klass_text)) {
    stop("The variant ", variant, " was not found for classification number ", classification)
  }

  if (type %in% c("vanlig", "variant")) {
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codes
    klass_data <- klass_data[, c("code", "parentCode", "level", "name")]
  }
  if (type == "kor") {
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$correspondenceItems
    if (length(klass_data) == 0) {
      stop(
        "No correspondence table found between classes ", classification, " and ", correspond, " for the date ", date,
        "For a list of valid correspondence tables use the function correspond_list()"
      )
    }
    if (targetswap) {
      klass_data <- klass_data[, c("targetCode", "targetName", "sourceCode", "sourceName")]
    } else {
      klass_data <- klass_data[, c("sourceCode", "sourceName", "targetCode", "targetName")]
    }
    names(klass_data) <- c("sourceCode", "sourceName", "targetCode", "targetName")
  }
  if (type == "korID") {
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$correspondenceMaps
  }
  if (type == "change") {
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codeChanges
    if (!is.data.frame(klass_data)) stop("No changes found for this classification.")
    if (dateswap) {
      klass_data <- klass_data[, c("newCode", "newName", "oldCode", "oldName", "changeOccurred")]
    } else {
      klass_data <- klass_data[, c("oldCode", "oldName", "newCode", "newName", "changeOccurred")]
    }
    names(klass_data) <- c("sourceCode", "sourceName", "targetCode", "targetName", "changeOccurred")
  }
  if (type %in% c("variant", "vanlig") & isTRUE(notes)) {
    klass_data$notes <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codes$notes
  }

  if (type %in% c("variant", "vanlig") & isTRUE(fratil)) {
    klass_data$validFromInRequestedRange <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codes$validFromInRequestedRange
    klass_data$validToInRequestedRange <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codes$validToInRequestedRange
  }

  if (output_style == "wide" & is.null(output_level) & is.null(correspond) & is.null(correspondID)) {
    # get maximum level
    maxlength <- max(klass_data$level)
    minlength <- min(klass_data$level)

    # check several levels exist
    if (maxlength == minlength) {
      warning("Only one level was detected. Classification returned with output_style normal. ")
      return(as.data.frame(klass_data))
    }

    # create most detailed level dataframe
    wide_data <- klass_data[klass_data$level == maxlength, c("code", "name")]
    names(wide_data) <- c(paste0("code", maxlength), paste0("name", maxlength))

    # loop through and match the other levels
    for (i in (as.numeric(maxlength) - 1):minlength) {
      m1 <- match(wide_data[, paste0("code", (i + 1))], klass_data$code)
      m2 <- match(klass_data$parentCode[m1], klass_data$code)
      tmp <- data.frame(klass_data$parentCode[m1], klass_data$name[m2])
      names(tmp) <- c(paste0("code", i), paste0("name", i))

      # paste in wide format beside previous
      wide_data <- cbind(wide_data, tmp)
    }

    # rename as klass_data for return
    klass_data <- wide_data
  }

  as.data.frame(klass_data)
}


#' @rdname get_klass
#' @param klass Deprecated; use `classification` instead.
#' @export
GetKlass <- function(klass,
                     date = NULL,
                     correspond = NULL,
                     correspondID = NULL,
                     variant = NULL,
                     output_level = NULL,
                     language = "nb",
                     output_style = "normal",
                     notes = FALSE,
                     quiet = TRUE) {
  # .Deprecated("get_klass") # Add in for future versions
  get_klass(
    classification = klass,
    date = date,
    correspond = correspond,
    correspondID = correspondID,
    variant = variant,
    output_level = output_level,
    language = language,
    output_style = output_style,
    notes = notes,
    quiet = quiet
  )
}
