#' Add metadata attributes to a data frame
#'
#' Adds variable labels and value labels to a data frame based on a metadata
#' dictionary. This is particularly useful for preparing datasets for use with
#' packages like \code{haven} or for exporting to formats like SPSS or Stata.
#'
#' @param data A data frame containing the raw dataset.
#' @param metadata A data frame that serves as a metadata dictionary. It must contain
#'   at least the columns: \code{variable_name}, \code{label}, and \code{type}. Optionally,
#'   it may include a \code{valueset} column for categorical variables, which should be
#'   a list column with data frames containing \code{value} and \code{label} columns.
#' @param ... Additional arguments (currently unused).
#' @param set_data_types Logical; if \code{TRUE}, attempts to coerce column data types
#'   to match those implied by the metadata. (Note: currently not fully implemented.)
#'
#' @return A `tibble` with the same data as \code{data}, but with added attributes:
#'   - Variable labels (via the \code{label} attribute)
#'   - Value labels (as a \code{haven::labelled} class, if applicable)
#'
#' @details
#' The function first checks the structure of the \code{metadata} using an internal helper.
#' Then, for each variable listed in \code{metadata}, it:
#' - Adds a label using the \code{label} attribute
#' - Converts values to labelled vectors using \code{haven::labelled()} if a \code{valueset} is provided
#'
#' If value labels are present, the function tries to align data types between the data
#' and the valueset (e.g., converting character codes to integers if necessary).
#'
#' @export
#'
#'
#' @examples
#' data <- data.frame(
#'   sex = c(1, 2, 1),
#'   age = c(23, 45, 34)
#' )
#'
#' metadata <- data.frame(
#'   variable_name = c("sex", "age"),
#'   label = c("Gender", "Age in years"),
#'   type = c("categorical", "numeric"),
#'   valueset = I(list(
#'     data.frame(value = c(1, 2), label = c("Male", "Female")),
#'     NULL
#'   ))
#' )
#'
#' labelled_data <- add_metadata(data, metadata)
#' str(labelled_data)
#'

add_metadata <- function(data, metadata, ..., set_data_types = FALSE) {

  if(is.character(data)) { data <- read_metadata(data) }

  column_names <- names(data)

  dictionary <- check_metadata_structure(metadata, column_names)

  with_valueset_col <- "valueset" %in% names(dictionary)

  variable_names <- dictionary$variable_name

  for(i in seq_along(variable_names)) {

    variable_name_i <- variable_names[i]

    if(!(variable_name_i %in% column_names)) next

    attr(data[[variable_name_i]], "label") <- dictionary$label[i]

    if(!with_valueset_col) next
    valueset <- dictionary$valueset[i][[1]]

    if(length(valueset) == 0) next

    labels <- valueset$value

    is_int <- rlang::is_integer(data[[variable_name_i]])
    if(is_int & !rlang::is_integer(labels) & grepl("^\\d{1,}$", labels[1])) {
      labels <- as.integer(labels)
    }

    if(!is_int & grepl("^\\d{1,}$", data[[variable_name_i]][1]) & rlang::is_integer(labels)) {
      data[[variable_name_i]] <- as.integer(data[[variable_name_i]])
    }

    names(labels) <- valueset$label

    data[[variable_name_i]] <- haven::labelled(
      x = data[[variable_name_i]],
      labels = labels,
      label = dictionary$label[i]
    )
  }

  return(dplyr::tibble(data))
}


#' Extract data dictionary from RCDF object
#'
#' This function retrieves the data dictionary embedded in the RCDF object
#'
#' @param data Object of class \code{rcdf}.
#'
#' @return A data frame that serves as a metadata dictionary. It must contain
#'   at least the columns: \code{variable_name}, \code{label}, and \code{type}. Optionally,
#'   it may include a \code{valueset} column for categorical variables, which should be
#'   a list column with data frames containing \code{value} and \code{label} columns.
#'
#' @export
#'
#' @examples
#' dir <- system.file("extdata", package = "rcdf")
#' rcdf_path <- file.path(dir, 'mtcars.rcdf')
#' private_key <- file.path(dir, 'sample-private-key.pem')
#'
#' rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
#' data_dictionary <- get_data_dictionary(rcdf_data)
#' names(data_dictionary)

get_data_dictionary <- function(data) {
  attributes(data)$metadata$dictionary
}


check_metadata_structure <- function(data, cols) {

  required_cols <- c("variable_name", "label", "type")
  required_cols_which <- which(required_cols %in% names(data))

  if (length(required_cols_which) < length(required_cols)) {
    stop("Invalid column names specified.")
  }

  data |>
    dplyr::filter(!is.na(variable_name)) |>
    dplyr::distinct(variable_name, .keep_all = TRUE) |>
    convert_to_na() |>
    dplyr::select(
      variable_name,
      label,
      type,
      dplyr::any_of(c("input_data", "valueset", "labels"))
    ) |>
    dplyr::filter(variable_name %in% cols)
}


convert_to_na <- function(data) {
  data |>
    dplyr::mutate_if(is.character, stringr::str_trim) |>
    dplyr::mutate_if(
      is.character,
      ~ dplyr::if_else(. == '', NA_character_, stringr::str_squish(.))
    )
}


read_metadata <- function(path) {

  data <- NULL

  if(grepl("\\.json$", path)) {
    data <- jsonlite::read_json(path, simplifyVector = TRUE)
  } else if (grepl("\\.csv$", path)) {
    data <- utils::read.csv(path)
  } else if (grepl("\\.xlsx$", path)) {
    data <- openxlsx::read.xlsx(path)
  } else if (grepl("\\.parquet$", path)) {
    data <- arrow::open_dataset(path)
  }

  data

}


#' Extract metadata from an RCDF file
#'
#' Retrieves a specific metadata value from a \code{.rcdf} file.
#'
#' @param path Character string. The file path to the \code{.rcdf} file.
#' @param key Character string. The metadata key to extract from the file.

#' @return The value associated with the specified metadata key, or \code{NULL} if the key does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming "example.rcdf" is a valid RCDF file in the working directory:
#' get_rcdf_metadata("example.rcdf", "creation_date")
#' }

get_rcdf_metadata <- function(path, key) {

  if(!fs::file_exists(path)) {
    stop(glue::glue("Specified RCDF file does not exist: {path}"))
  }

  if(!grepl("\\.rcdf$", path)) {
    stop(glue::glue("Not a valid RCDF file: {path}"))
  }

  meta <- extract_rcdf(path)
  meta[[key]]

}

