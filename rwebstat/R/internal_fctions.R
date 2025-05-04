# create url string to request webstat API : dataset list
make_url_dataset_list <- function(language = "fr", base_url = "https://api.webstat.banque-france.fr/webstat-", format = "csv") {
  api_base_url <- paste(base_url, language, "/v1/", sep = "")
  api_fun <- "catalogue/"
  format <- paste0("?format=", format)
  w_url <- paste0(
    api_base_url,
    api_fun, format
  )
  return(w_url)
}

# create url string to request webstat API : structure
make_url_structure <- function(dataset_name, language = "fr", base_url = "https://api.webstat.banque-france.fr/webstat-", option = "light") {
  api_base_url <- paste(base_url, language, "/v1/", sep = "")
  api_fun <- "datastructure/"
  format <- "?format=json"
  detail <- paste0("&detail=", option)
  w_url <- paste0(api_base_url, api_fun, dataset_name, format, detail)
}

# create url string to request webstat API : series_list
make_url_series_list <- function(dataset_name, language = "fr", base_url = "https://api.webstat.banque-france.fr/webstat-", format = "csv") {
  if (missing(dataset_name)) {
    stop("Dataset is missing")
  }
  # Build API URL "w_url" for the request
  api_base_url <- paste(base_url, language, "/v1/", sep = "")
  api_fun <- "catalogue/"
  format <- paste0("?format=", format)
  w_url <- paste0(
    api_base_url,
    api_fun, dataset_name, format
  )
  return(w_url)
}

# create url string to request webstat API : data
make_url_data <- function(dataset_name, series_name = NA, startPeriod = NA, endPeriod = NA,
                          firstNObs = NA, lastNObs = NA, language = "fr", format = NULL,
                          base_url = "https://api.webstat.banque-france.fr/webstat-") {
  if (language != "en" & language != "fr") {
    stop("language must be either 'fr' or 'en'")
  }
  if (missing(dataset_name)) {
    stop("Dataset is missing")
  }
  if (!is.na(firstNObs) & !is.na(lastNObs)) {
    stop("firstNObs and lastNObs cannot be used together in the same request")
  }
  if (is.null(format)) {
    stop("Please specify format : csv or json")
  }

  api_base_url <- paste(base_url, language, "/v1/", sep = "")
  api_fun <- "data/"
  format_url <- paste0("?format=", format)
  opt_params <- c(
    series_name, startPeriod, endPeriod, firstNObs,
    lastNObs
  )
  opt_txt <- c(
    NA, "startPeriod=", "endPeriod=", "firstNObservations=",
    "lastNObservations="
  )
  na_loc <- which(is.na(opt_params))
  w_url <- paste0(api_base_url, api_fun, dataset_name, "/")
  if (!is.na(series_name)) {
    w_url <- paste0(w_url, series_name)
  }
  w_url <- paste0(w_url, format_url)
  opt_params <- opt_params[2:5]
  opt_txt <- opt_txt[2:5]
  na_loc <- which(is.na(opt_params))
  w_url <- paste(w_url, paste0(opt_txt[-na_loc], opt_params[-na_loc], collapse = "&"), sep = "&")
  return(w_url)
}

# get request with a url string and your client id string
get_data <- function(url, client_ID, ...) {
  req <- GET(url, add_headers(`x-ibm-client-id` = client_ID), ...)

  # stop with graceful message if http status is not 200 (as much as we can)
  if (req$status_code != 200) {
    switch(as.character(req$status_code),
      "502" = {
        stop(paste(url, "\nTime Out : please request a smaller set"))
      },
      "429" = {
        stop(paste(content(req, encoding = "UTF-8")$httpMessage, "\n", content(req, encoding = "UTF-8")$moreInformation))
      },
      "501" = {
        stop(paste(url, "\nFormat not yet implemented."))
      },
      "500" = {
        stop(paste(url, "\nInternal error. Please try again."))
      },
      "400" = {
        stop(paste(url, "\nIncorrect format value."))
      },
      "401" = {
        stop(paste(url, "\nInvalid client_ID. Please check that the string contained in the client_ID or webstat_client_ID variable is correct (format: ''123456ab-ab12-12cd-12cd-123456789abc'') and that your account is registered to the Webstat API you are trying to access."))
      },
      "404" = {
        stop(paste(url, "\nData not found."))
      },
      {
        stop(content(req, encoding = "UTF-8")$message)
      }
    )
  }
  return(req)
}

# parse request according to format and language
parse_request <- function(req, format, language) {
  if (format == "json") {
    out <- parse_json(req)
  } else if (format == "csv") {
    out <- parse_csv(req, language)
  } else {
    stop("format must be csv or json")
  }
  return(out)
}

# parse a json format serie request
parse_json_unit <- function(main_content, dataset_name) {

  # json's keys
  main_content <- main_content$ObservationsSerie
  serie_key <- main_content$seriesKey
  title <- main_content$title
  titleCompl <- main_content$titleCompl
  freq <- main_content$frequency
  obs <- main_content$observations

  # no observations case
  if (length(obs) == 0) {
    void_df <- data.frame(as.POSIXct("1900-01-01"), NA)
    names(void_df) <- c("date", "value")
    attr(void_df, "metadata") <- list(dataset_name = dataset_name, title = title, freq = freq, titleCompl = titleCompl)
    names(void_df)[which(names(void_df) == "value")] <- serie_key
    rownames(void_df) <- NULL
    attr(void_df, "metadata") <- cbind(names(attr(void_df, "metadata")), do.call(rbind, attr(void_df, "metadata")))
    rownames(attr(void_df, "metadata")) <- NULL
    colnames(attr(void_df, "metadata")) <- c("Metadata", serie_key)
    return(void_df)
  }

  # test observation's names
  not_in_obs <- setdiff(names(obs[[1]][[1]]), c("periodId", "periodFirstDate", "periodName", "value"))
  if (length(not_in_obs) > 0) {
    print("The following property are not in the observation :")
    print(not_in_obs)
  }

  # rbind all observations together
  df <- do.call(rbind, unlist(x = obs, recursive = F))
  df <- data.frame(df, row.names = NULL)

  # POSIXct conversion
  schema <- "%d-%m-%Y %H:%M:%S"
  df$date <- as.POSIXct(unlist(df$periodFirstDate), format = schema)
  df <- df[c("date", "value")]

  # fill "NULL" with NA
  df$value[df$value == "NULL"] <- NA

  # fill names and attributes
  attr(df, "metadata") <- list(dataset_name = dataset_name, title = title, freq = freq, titleCompl = titleCompl)
  attr(df, "metadata") <- lapply(attr(df, "metadata"), function(x) if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  })
  names(df)[which(names(df) == "value")] <- serie_key
  rownames(df) <- NULL
  attr(df, "metadata") <- cbind(names(attr(df, "metadata")), do.call(rbind, attr(df, "metadata")))
  rownames(attr(df, "metadata")) <- NULL
  colnames(attr(df, "metadata")) <- c("Metadata", serie_key)
  return(df)
}

# parse a json format request
parse_json <- function(req) {
  json_list <- content(req)
  dataset_name <- json_list$dataset

  # apply json_unit to each serie requested
  df <- lapply(json_list$seriesObs, parse_json_unit, dataset_name = dataset_name)

  # if there is only one serie requested we return the dataframe (observations & metadatas)
  if (length(df) == 1) {
    df[[1]][, 2] <- unlist(df[[1]][, 2])
    return(df[[1]])
  }

  # else we have to align and sort serie's periods to create a clean dataframe
  meta_titles <- w_meta(df[[1]])[, 1]
  dates_list <- lapply(df, function(x) {
    return(x$date)
  })
  dates_vect <- do.call("c", dates_list)
  dates_vect <- sort(unique(dates_vect))
  results_values <- data.frame(dates_vect)
  result_meta <- data.frame(meta_titles)
  series_names <- c()
  for (i in 1:length(df)) {
    values <- rep(NA, dim(results_values)[1])
    values[results_values[, 1] %in% df[[i]]$date] <- unlist(df[[i]][, 2])
    results_values <- cbind(results_values, values)
    result_meta <- cbind(result_meta, w_meta(df[[i]])[, 2])
    series_names <- cbind(series_names, names(df[[i]])[2])
  }

  # fill names and attributes
  attr(results_values, "metadata") <- result_meta
  names(results_values) <- c("date", series_names)
  names(attr(results_values, "metadata")) <- c("Metadata", series_names)
  attr(results_values, "metadata") <- data.frame(apply(attr(results_values, "metadata"), 2, as.character), stringsAsFactors = F)
  return(results_values)
}

# parse a csv format request
# content's dates in csv format are already sorted
parse_csv <- function(req, language) {
  cont_csv <- content(req, as = "text", encoding = "UTF-8")
  if (req$status_code != 200) {
    stop(cont_csv)
  }
  if (language == "en") {
    sep_lg <- ","
    locale_lg <- "."
  }
  else {
    sep_lg <- ";"
    locale_lg <- ","
  }
  meta_data_df <- readr::read_delim(
    file = cont_csv, delim = sep_lg,
    skip = 0, col_names = FALSE, n_max = 3
  )

  data_serie <- readr::read_delim(cont_csv,
    delim = sep_lg, na = "-",
    col_names = FALSE,
    locale = readr::locale(decimal_mark = locale_lg)
  )

  data_serie <- data_serie[4:dim(data_serie)[1], ]

  # fill names and attributes
  meta_data <- as.data.frame(meta_data_df, row.names = NULL)
  colnames(meta_data) <- c("Titles", meta_data[3, -1])
  data_serie[data_serie == ""] <- NA
  colnames(data_serie) <- c("date", make.names(meta_data[3, 2:ncol(meta_data)]))
  meta_data <- meta_data[-3, ]
  attr(data_serie, "metadata") <- meta_data
  data_serie <- as.data.frame(data_serie)
  names(attr(data_serie, "metadata")) <- attr(data_serie, "metadata")[3, ]
  attr(data_serie, "metadata") <- attr(data_serie, "metadata")[-3, ]
  return(data_serie)
}


# set your client ID into client_ID global variable
set_client_id <- function() {
  if (!exists(x = "client_ID", envir = .GlobalEnv)) {
    client_ID <- "\"|'"
  }
  while (length(grep('\"|\'', client_ID)) > 0) {
    message("We suggest you save your Webstat client ID in a global variable called webstat_client_ID.\nSomething like:\nwebstat_client_ID <- \"1234abcd-12ab-12ab-12ab-123456abcdef\"\nFor now, please enter your Webstat API clientID (without \" or '.) :")
    client_ID <- readline()
    return(client_ID)
  }
}

# check if client_ID exists, if not set it (with previous function set_client_id)
check_client_id <- function(client_ID) {
  # check client_ID
  if (missing(client_ID)) {
    if (exists("webstat_client_ID")) {
      client_ID <- webstat_client_ID
    } else {
      client_ID <- set_client_id()
    }
    webstat_client_ID <- NULL
  }
  return(client_ID)
}

# replace odd characters in a vector
rid_odd_character <- function(char_vect = "", odd_char = "\uFEFF", replacement = "") {
  str_replace_all(char_vect, "\uFEFF", replacement)
}

# set csv separator wrt the language choosen
set_sep_lg <- function(language = "fr") {
  if (language == "en") {
    sep_lg <- ","
  } else if (language == "fr") {
    sep_lg <- ";"
  } else {
    stop("language must be \"fr\" or \"en\"")
  }
  return(sep_lg)
}
