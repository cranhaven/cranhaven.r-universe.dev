#' Getting parameters data from tables files (Excel sheet, csv)
#'
#' @param file Excel or csv file path (including name of the file)
#' @param sheet_name Name of an Excel sheet (useless for csv files)
#' @param num_na Replacement value for numerical NA values (default: NA)
#' @param char_na Replacement value for character NA values (default: "")
#' @param file_path `r lifecycle::badge("deprecated")` `file_path` is no
#'   longer supported, use `file` instead.
#'
#' @details After data are loaded, numerical and string NA values are
#' replaced respectively with num_na or char_na
#'
#' @return A tibble of parameters
#' @export
#'
#' @examples
#'
#' usm_xl_file <- download_usm_xl(
#'   file = "inputs_stics_example.xlsx",
#'   verbose = FALSE
#' )
#' read_params_table(usm_xl_file, sheet = "USMs")
#' usm_csv_file <- download_usm_csv(
#'   file = "inputs_stics_example_USMs.csv",
#'   verbose = FALSE,
#'   stics_version = "V9.2"
#' )
#' read_params_table(file = usm_csv_file)
read_params_table <- function(file, sheet_name = NULL,
                              num_na = "NA",
                              char_na = "NA",
                              file_path = lifecycle::deprecated()) {
  if (lifecycle::is_present(file_path)) {
    lifecycle::deprecate_warn("1.0.0",
                              "read_params_table(file_path)",
                              "read_params_table(file)")
  } else {
    file_path <- file # to remove when we update inside the function
  }

  # files extension list
  files_ext_lst <- c("csv", "xls", "xlsx")

  # Getting file extension, and checking its validity
  file_ext <- tools::file_ext(file_path)
  if (isFALSE(file_ext %in% files_ext_lst)) {
    stop(paste0('"', file_ext, '"'), ": is not a valid extension")
  }

  # Detecting if the sheet exists in XL file
  # dfault value for csv file
  sheet_exists <- TRUE
  if (isFALSE("csv" == file_ext)) {
    xl_sheets <- readxl::excel_sheets(file_path)
    sheet_exists <- sheet_name %in% xl_sheets
  }
  # If the sheet name is not provided
  if (!length(sheet_exists)) sheet_exists <- FALSE


  # Unknown sheet in XL file
  if (isFALSE(sheet_exists)) {
    warning(paste(
      sheet_name,
      "unknown or not set sheet name!\n",
      "Check in list:\n",
      paste(xl_sheets, collapse = ", ")
    ))
    return()
  }

  # Reading file according to its format
  switch(file_ext,
    csv = {
      out_table <- utils::read.csv2(
        file = file_path,
        header = TRUE,
        sep = ";",
        stringsAsFactors = FALSE,
        na.strings = "",
        strip.white = TRUE,
        colClasses = "character"
      )
    },
    {
      out_table <- readxl::read_excel(file_path,
                                      sheet = sheet_name,
                                      trim_ws = TRUE,
                                      col_types = "text")
    }
  )

  # Converting if necessary to tibble object
  out_table <- tibble::as_tibble(out_table)

  # Replacing NA with empty string
  # Casting logical columns to character type
  out_table <- out_table %>%
    dplyr::mutate_if(is.logical, as.character) %>%
    rep_character_na(char_na)

  out_table <- replace_na(out_table, replacement = num_na)

  # Replacing numerical NA with a specific value
  out_table <- replace_na(out_table, replacement = char_na)

  out_table
}

rep_character_na <- function(in_df, replacement = "") {
  replace_na(in_df, replacement = replacement)
}


rep_numeric_na <- function(in_df, replacement = as.numeric(NA)) {
  replace_na(in_df, replacement = replacement)
}

replace_na <- function(in_df, replacement) {
  rep_type <- class(replacement)
  if (!rep_type %in% c("numeric", "character")) {
    stop(rep_type, ": unknown replacement value type")
  }

  # Getting columns ids according to rep_type
  switch(rep_type,
    numeric = {
      idx_type_col <- unlist(lapply(in_df, is.numeric), use.names = FALSE)
    },
    character = {
      idx_type_col <- unlist(lapply(in_df, is.character), use.names = FALSE)
    }
  )


  # Nothing to be replaced
  if (isFALSE(any(idx_type_col))) {
    return(in_df)
  }

  idx_col_has_na <- unlist(lapply(in_df, function(x) {
    any(is.na(x))
  }),
  use.names = FALSE
  )

  to_be_treated <- which(idx_type_col & idx_col_has_na)

  # Nothing to be replaced
  if (!length(to_be_treated)) {
    return(in_df)
  }

  for (i in to_be_treated) {
    in_df[is.na(in_df[, i]), i] <- replacement
  }


  in_df
}
