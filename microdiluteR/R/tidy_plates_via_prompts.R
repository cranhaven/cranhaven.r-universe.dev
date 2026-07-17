#' @title Read raw photometry data and add meta data based on user input
#' @description Most old photometer devices save the data in plain text files. If there was ever analysis software, 
#' this is often no longer available due to increasing technical requirements or proprietary software 
#' should generally be avoided. Especially for broth microdilution assays, it is necessary to measure 
#' the photometer plates at several points in time, which means that the same samples are represented 
#' in several files with their corresponding values. Usually the data is then merged manually, which can 
#' lead to mistakes and takes up unnecessary time. In this case, the `tidy_plates()` function provides a convenient 
#' way to read in the raw files and, based on user input, add metadata on the validity of the samples, 
#' as well as treatment groups and concentration levels.
#' @param input_data The folder path to the files containing the raw photometer data. Data files should 
#' be given as plain text files and with timepoint identifiers in their file names (e.g. "file_T0.txt" or 
#' "file_t0.txt").
#' @param direction A character vector specifying the orientation of the plate layout.
#' It can be either "horizontal" or "vertical".
#' @param ... Additional arguments to be passed to \code{\link{read_plates}}.
#' @return A tidy data frame containing absorption values and meta data (validity of samples as well as 
#' treatment and concentration level information).
#' @seealso
#' \code{\link{tidy_single_plate}}, \code{\link{tidy_plates_via_params}}
#' @export
tidy_plates_via_prompts <- function(input_data,
                                    direction = c("horizontal","vertical"),
                                    ...) {

  # Read data
  raw_data_list <- read_plates(input_data, ...)
  
  # Initialize empty list to store tidy data
  tidy_list <- list()
  
  # Generate list of groups
  group_list <- ask_group_list(file_list = names(raw_data_list))

  # Generate list of experiments
  experiment_list <- ask_experiment_list(file_list = names(raw_data_list))

  # Ask user for preferred validity method
  validity_method <- ask_validity_method()

  # Check user-preferred validity method
  if (tolower(validity_method) == "threshold") {
    threshold <- ask_threshold()
  } else if (tolower(validity_method) == "samples") {
    invalid_samples <- ask_invalid_samples()
  } else {
    stop("Invalid validity method. Please use either 'threshold' or 'samples'.")
  }

  # Generate direction-specific list for treatments
  treatment_list <- ask_treatment_list(direction)

  # Generate direction-specific list for concentrations
  concentration_list <- ask_concentration_list(direction)

  # Iterate over each file
  for (i in seq_along(raw_data_list)) {
    raw_data = raw_data_list[[i]]
    # Get row and column names separately
    row_names <- rownames(raw_data)
    col_names <- colnames(raw_data)

    # Check validity of each cell in the data matrix
    tidy_data <- validate_cells(raw_data, row_names, col_names, validity_method, threshold, invalid_samples)

    # Add treatment information
    tidy_data <- add_treatment(tidy_data, treatment_list, ask_treatment_list = FALSE)

    # Add concentration information
    tidy_data <- add_concentration(tidy_data, concentration_list, ask_concentration_list = FALSE)

    # Extract file name, group identifier, experiment name, and time points from file name
    timepoint <- str_extract(names(raw_data_list)[i], regex("[tT][0-9]"))
    file_name <- grep("[tT][0-9]", basename(names(raw_data_list)[i]), value = TRUE)
    group <- str_extract(names(raw_data_list)[i], regex("grp[0-9]"))
    experiment <- str_extract(names(raw_data_list)[i], regex("exp[0-9]"))

    # Add file name and time points to corresponding columns
    tidy_data$Timepoint <- timepoint
    tidy_data$File <- file_name
    tidy_data$Group <- group_list[[group]]
    tidy_data$Experiment <- experiment_list[[experiment]]

    # Append tidy data to list
    tidy_list[[length(tidy_list) + 1]] <- tidy_data
  }

  # Synchronize invalid samples across plates
  invalid_samples <- unique(unlist(lapply(tidy_list, function(df) df$Position[df$Validity == "invalid"])))
  for (i in seq_along(tidy_list)) {
    tidy_list[[i]]$Validity <- ifelse(tidy_list[[i]]$Position %in% invalid_samples, "invalid", tidy_list[[i]]$Validity)
  }

  # Combine all data frames
  combined_data <- do.call(rbind, tidy_list)
  attr(combined_data, "info") <- attr(raw_data_list, "info")
  
  return(tibble::tibble(combined_data))
}
