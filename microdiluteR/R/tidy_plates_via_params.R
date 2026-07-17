#' Tidy multiple 96-well plates via parameters
#'
#' This function processes raw plates data from photometer measurements, adds metadata via user-specified parameter values, and combines processed data into a single data frame.
#' 
#' @param input_data Either a folder path containing raw data files or a list of data frames.
#' @param direction A character vector specifying the orientation of the plate layout.
#' It can be either "horizontal" or "vertical".
#' @param group_IDs A character vector providing group identifiers for each experiment.
#' @param experiment_names A character vector providing names for each experiment. The hierarchy is group > experiment, i.e.
#' within a single group, there might be several experiments taking place (e.g. multiple extracts from the same plant species
#' tested with plant species being the group and type of extract being the experiment).
#' @param validity_method A character vector specifying the method for determining cell validity. 
#'                        It can be either "threshold" (i.e. samples are validated based on a common absorption maximum) or "samples" (i.e. samples are manually specified as invalid).
#' @param threshold A numeric threshold value. Applied if \code{validity_method} is set to 'threshold'.
#' @param invalid_samples A character vector containing well positions (e.g. "A-3", "B-8",...) of invalid samples. Applied if \code{validity_method} is set to 'samples'.
#' @param treatment_labels A character vector containing treatment labels.
#' @param concentration_levels A numeric vector containing concentration levels.
#' @param ... Additional arguments to be passed to \code{\link{read_plates}}.
#' @return A tidy tibble containing combined data and metadata from all input plates.
#' @examples
#' # Load example data
#' data(bma)
#' # Add metadata from user parameters
#' bma_tidy <- tidy_plates_via_params(input_data = bma,
#'                                    direction = "horizontal",
#'                                    group_IDs = paste0("Group_", letters[1:2]),
#'                                    experiment_names = c("Experiment 1", "Experiment 2"),
#'                                    validity_method = "threshold",
#'                                    threshold = 1,
#'                                    treatment_labels = LETTERS[1:8],
#'                                    concentration_levels = seq(from=80, to=10, length.out=8))
#' bma_tidy # View tidy data
#' @details
#' This function processes photometer data from multiple experiments and adds metadata based on user-set parameters if
#' the experimental layout is repeated across plates and should by synchronized. It supports two methods for 
#' determining cell validity: "threshold" and "invalid". If "threshold" method is chosen, the validity of each cell
#' is determined based on a specified threshold value. If "sample" method is chosen, samples at specified well positions
#' on the plate are considered invalid. The function generates lists of treatments and concentration levels based on the
#' direction parameter, i.e. the direction of the treatments and concentration levels applied (either horizontally or
#' vertically on the plate). If the plate layout and, thus, the metadata changes across plates, then function
#' \code{tidy_plates_via_prompts} might be a better choice since it helps the user to add metadata for each plate separately
#' based on user prompts. If there is only one plate, where metadata should be added, then \code{tidy_single_plate} should be used.
#' 
#' For all three functions, \code{tidy_single_plate}, \code{tidy_plates_via_params}, and \code{tidy_plates_via_prompts}, to work
#' properly, file names should provide a file identifier (i.e. "bma" in case there are additional but not relevant files in
#' the folder), a group identifier (i.e. starting with "grp" followed by an incrementing number), an identifier for 
#' experiments (starting with "exp" followed by a number, e.g. "exp1") and an identifier for timepoints (starting with the
#' upper- or lower-case letter t followed by an incrementing number, e.g. "T0" or "t0").
#' @seealso
#' \code{\link{tidy_single_plate}}, \code{\link{tidy_plates_via_prompts}}
#' @importFrom purrr map_lgl
#' @importFrom stringr str_extract regex
#' @importFrom tibble tibble
#' @export
tidy_plates_via_params <- function(input_data,
                                  direction = c("horizontal","vertical"),
                                  group_IDs = NULL,
                                  experiment_names = NULL,
                                  validity_method = c("threshold","invalid"),
                                  threshold = NULL,
                                  invalid_samples = NULL,
                                  treatment_labels,
                                  concentration_levels,
                                  ...) {
  
  # Read data
  raw_data_list <- read_plates(input_data, ...)

  # Initialize empty list to store tidy data
  tidy_list <- list()

  # Generate list of groups
  group_list <- generate_group_list(group_IDs, names(raw_data_list))

  # Generate list of experiments
  experiment_list <- generate_experiment_list(experiment_names, names(raw_data_list))
  
  # Check user-preferred validity method
  if (tolower(validity_method) == "threshold") {
    threshold <- threshold
  } else if (tolower(validity_method) == "samples") {
    invalid_samples <- invalid_samples
  } else {
    stop("Invalid validity method. Please use either 'threshold' or 'samples'.")
  }
  
  # Generate direction-specific list for treatments
  treatment_list <- generate_treatment_list(treatment_labels, direction)
  
  # Generate direction-specific list for concentrations
  concentration_list <- generate_concentration_list(concentration_levels, direction)
  
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
