#' @title Generating observation data files from a data.frame
#'
#' @param df A data frame containing the values of the observations to use
#' (see Details).
#' @param out_dir Path of the directory where to generate the file(s).
#' @param usms_list An optional list of usms names to be used for selecting
#' which files to generate from the obs_table
#' @param obs_table `r lifecycle::badge("deprecated")` `obs_table` is no
#'   longer supported, use `df` instead.
#' @param out_path `r lifecycle::badge("deprecated")` `out_path` is no
#'   longer supported, use `out_dir` instead.
#'
#' @details
#'  `df` is a `data.frame` with the following format:
#'
#'
#' |usm_name       |  ian| mo| jo| jul|densite | lai(n)| masec(n)|   azomes|
#' |:--------------|----:|--:|--:|---:|:-------|------:|--------:|--------:|
#' |USM_2017_T1_CI | 2017|  9|  6| 249|NA      |     NA|     0.31| 27.07395|
#' |USM_2017_T1_CI | 2017|  9| 20| 263|NA      |     NA|     0.60| 27.90000|
#' |USM_2018_T1    | 2017| 10| 20| 293|NA      |    0.1|       NA|       NA|
#' |USM_2018_T1    | 2018|  5| 15| 482|NA      |    1.2|       NA|       NA|
#'
#' * `usm_name` column contains usms names which are used as output .obs
#'    files names
#' * `ian`, `mo`, `jo` and `jul` are mandatory
#'    (year, month, day and julian date)
#' * Other columns one per variable contain observations values or NA
#'
#'  @seealso \code{\link{get_var_info}} for getting variable right syntax or
#'  searching a variable name.
#'
#' @return A return logical status indicating if any error when writing
#' files (FALSE), TRUE when no errors.
#'
#' @examples
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#' obs_df <- read_params_table(file = xl_path, sheet_name = "Obs")
#' gen_obs(df = obs_df, out_dir = "/path/to/dest/dir")
#'
#' @export
#'
gen_obs <- function(df,
                    out_dir,
                    usms_list = NULL,
                    obs_table = lifecycle::deprecated(),
                    out_path = lifecycle::deprecated()) {
  if (lifecycle::is_present(obs_table)) {
    lifecycle::deprecate_warn("1.0.0", "gen_obs(obs_table)", "gen_obs(df)")
  } else {
    obs_table <- df # to remove when we update inside the function
  }
  if (lifecycle::is_present(out_path)) {
    lifecycle::deprecate_warn("1.0.0", "gen_obs(out_path)", "gen_obs(out_dir)")
  } else {
    out_path <- out_dir # to remove when we update inside the function
  }
  # Checking if out_path exists
  if (!dir.exists(out_path)) {
    warning(paste("The directory does not exist", out_path))
    return(invisible(FALSE))
  }

  # Finding usm names column
  usm_idx <- grep("usm", tolower(colnames(obs_table)))
  if (length(usm_idx) > 1) {
    stop("Multiple usms names columns !")
  }

  # Getting usms names
  usm_names <- unique(obs_table[[usm_idx]])

  # Treating a usms_list input
  if (!base::is.null(usms_list))
    usm_names <- usm_names[usm_names %in% usms_list]

  nb_usms <- length(usm_names)

  # For storing files paths when not generated
  bad_files <- vector(mode = "character", length = nb_usms)

  # Loop over usms names and files generation
  for (i in 1:nb_usms) {
    # Setting usm name
    usm_name_tmp <- usm_names[i]

    # Setting the output file path
    out_file_path <- file.path(out_path, paste0(usm_name_tmp, ".obs"))

    # Selecting data for the current usm, eliminating all NA values columns
    usm_df <- obs_table %>%
      dplyr::filter(obs_table[[usm_idx]] %in% usm_name_tmp) %>%
      dplyr::select_if(~ !all(is.na(.)))

    # Writing the file and
    # storing file path when writing error
    if (!gen_obs_(usm_df, out_file_path)) {
      bad_files[i] <- out_file_path
    }
  }

  # if any error while writing files
  if (!all(bad_files == "")) {
    warning(paste("The file has not been generated:", bad_files))
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}

#' Write STICS obs data from data.frame
#'
#' @description Write STICS obs data from data.frame/tibble
#'
#' @param obs_table data.frame to write
#' @param file_path Path to the file to write to
#'
#' @examples
#' \dontrun{
#' # Getting observations data
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#'
#' # Loading and filtering data for usm "USM_2017_T1_CI"
#' obs_df <- read_params_table(file = xl_path, sheet_name = "Obs") %>%
#'   dplyr::filter(usm_name %in% "USM_2017_T1_CI")
#'
#' # Generating the csv file
#' gen_obs_(obs_df, "USM_2017_T1_CI.obs")
#' }
#'
#' @return A logical value if the file generation succeeded (TRUE)
#'  or not (FALSE)
#'
#' @keywords internal
#'
#' @noRd
#'
gen_obs_ <- function(obs_table, file_path) {


  # Checking file path
  dir_name <- dirname(file_path)
  if (!dir.exists(dir_name)) {
    warning(paste("Directory does not exist:", dir_name))
    return(invisible(FALSE))
  }

  # Removing unwanted columns !
  date_plt_idx <- grep("date|^plant$|usm", tolower(colnames(obs_table)))
  if (length(date_plt_idx)) {
    obs_table <- obs_table[, -date_plt_idx]
  }

  # Checking date columns & variables columns
  patt_str <- "^ian$|^mo$|^jo$|^jul$"
  obs_var_df <- obs_table[, -grep(patt_str, colnames(obs_table)), drop = FALSE]
  obs_date_df <- obs_table[, grep(patt_str, colnames(obs_table)), drop = FALSE]

  if (!dim(obs_var_df)[2] || dim(obs_date_df)[2] < 4) {
    warning("Missing columns for dates,",
            " or no observation variables values to write !")
    return(invisible(FALSE))
  }

  # Ordering date components columns in a data.frame
  obs_table <- data.frame(obs_date_df, obs_var_df)

  # TODO: see what is the purpose of _sd ending tag !
  # Linked to associated plants ???

  # Back to STICS variables names syntax !
  colnames(obs_table) <- col_names_to_var(colnames(obs_table))

  ret <- try(utils::write.table(obs_table,
                                file_path,
                                sep = ";",
                                na = "-999.99",
                                row.names = FALSE,
                                quote = FALSE))

  # Checking if any error writing the file
  if (methods::is(ret, "try-error")) {
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
