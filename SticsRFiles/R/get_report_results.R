
#' Extracting data from the STICS report file
#'
#' @param workspace Path of the directory containing the STICS report file
#' to read.
#' @param file_name A report file name among "mod_rapport.sti" (default),
#' "mod_rapportA.sti", "mod_rapportP.sti"
#' @param usm Vector of USM names. Optional, if not provided, the function
#' returns the results for all USMs.
#' @param var_list vector of output variables names to filter
#' (optional, see `get_var_info()` to get the names of the variables)
#' @param usm_name `r lifecycle::badge("deprecated")` `usm_name` is no
#' longer supported, use `usm` instead.
#'
#' @details The data may be filtered using `usm_name` vector of usm names and
#' and/or `var_list` vector of variables names. In the returned data.frame,
#' variables names respect the same syntax as in the get_sim output.
#'
#' @return A data.frame
#'
#' @export
#'
#' @examples
#' path <- get_examples_path(file_type = "sti")
#' get_report_results(workspace = path)
#'
#' get_report_results(workspace = path, usm = c("DurumWheat", "grass"))
#'
#' get_report_results(workspace = path, var_list = c("masec(n)", "QNplante"))
#'
#' get_report_results(workspace = path, usm = c("DurumWheat", "grass"))
#'
#' get_report_results(workspace = path)
#'
#' get_report_results(workspace = path, file_name = "mod_rapportA.sti")
#'
#'
get_report_results <- function(workspace,
                               file_name = "mod_rapport.sti",
                               usm = NULL,
                               var_list = NULL,
                               usm_name = lifecycle::deprecated()) {
  if (lifecycle::is_present(usm_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_report_results(usm_name)",
                              "get_report_results(usm)")
  } else {
    usm_name <- usm # to remove when we update inside the function
  }

  files_name <- c("mod_rapport.sti", "mod_rapportA.sti", "mod_rapportP.sti")
  usm_col <- "P_usm"
  usm_patt <- paste0("^", usm_col)

  # Checking file name
  if (!file_name %in% files_name) stop("Unknown report file name:", file_name)

  # Reading file lines
  file_lines <- readLines(file.path(workspace, file_name))

  # Checking if multiple headers in file are all identical
  h_idx <- grep(pattern = usm_patt, x = file_lines)
  if (length(h_idx)) {
    h <- utils::read.table(
      text = file_lines[h_idx],
      sep = ";",
      fill = TRUE,
      strip.white = TRUE,
      na.strings = "",
      stringsAsFactors = FALSE
    )

    if (any(is.na(h)))
      stop("Headers strings are not homogeneous in report file!")
  } else {
    warning("Not any header in report file (no col names in ouput)!")
  }

  # Getting data into a data.frame
  data_idx <- grep(pattern = usm_patt, x = file_lines, invert = TRUE)

  if (!length(data_idx)) stop("Not any data in report file!")

  df <- utils::read.table(
    text = file_lines[data_idx],
    sep = ";",
    strip.white = TRUE,
    na.strings = "",
    stringsAsFactors = FALSE
  )



  # If report header is present
  # otherwise column are named V1 to Vncol
  if (length(h_idx)) {
    col_names <- unlist(h[1, ], use.names = FALSE)
    df <- df[, seq_along(col_names)]
    names(df) <- col_names
  } else {
    col_names <- names(df)
  }

  # To be homogenous with get_sim, get_file_int
  colnames(df) <- var_to_col_names(col_names)

  # Filtering usms
  if (!base::is.null(usm_name)) {
    df <- df %>% dplyr::filter(df[[usm_col]] %in% usm_name)
  }

  # Filtering variables
  if (!base::is.null(var_list)) {
    df <- df %>% dplyr::select(c(col_names[1:11], var_to_col_names(var_list)))
  }

  return(df)
}
