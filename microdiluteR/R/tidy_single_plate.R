#' @title Tidy single 96-well plate via parameters
#' @description This function processes a single raw 96-well plate data from a photometer measurement by adding metadata via user-specified parameter values.
#' @param input_data Either a file path or a data frames with 8 rows and 12 columns.
#' @param direction A character vector specifying the orientation of the plate layout.
#' It can be either "horizontal" or "vertical".
#' @param group_ID A character vector providing group identifiers for each experiment.
#' @param experiment_name A string providing the name of the experiment. The hierarchy is group > experiment, i.e.
#' within a single group, there might be several experiments taking place (e.g. multiple extracts from the same plant species
#' tested with plant species being the group and type of extract being the experiment).
#' @param validity_method A character vector specifying the method for determining cell validity. 
#'                        It can be either "threshold" (i.e. samples are validated based on a common absorption maximum) or "samples" (i.e. samples are manually specified as invalid).
#' @param threshold A numeric threshold value. Applied if \code{validity_method} is set to 'threshold'.
#' @param invalid_samples A character vector containing well positions (e.g. "A-3", "B-8",...) of invalid samples. Applied if \code{validity_method} is set to 'samples'.
#' @param treatment_labels A character vector containing treatment labels.
#' @param concentration_levels A numeric vector containing concentration levels.
#' @param ... Additional arguments to be passed to \code{\link{read_plates}}.
#' @return A tidy tibble containing data and metadata.
#' @examples
#' # Load example data
#' data(bma)
#' # Add metadata from user parameters
#' bma_tidy <- tidy_single_plate(input_data = bma[1],
#'                        direction = "horizontal",
#'                        group_ID = "Group A",
#'                        experiment_name = "Experiment 1",
#'                        validity_method = "threshold",
#'                        threshold = 1,
#'                        treatment_labels = LETTERS[1:8],
#'                        concentration_levels = seq(from=80, to=10, length.out=8))
#' bma_tidy # View tidy data
#' @details
#' This function processes photometer data from a single measurement and adds metadata based on user-set parameters.
#' It supports two methods for determining cell validity: "threshold" and "invalid". If "threshold" method is chosen,
#' the validity of each cell is determined based on a specified threshold value. If "sample" method is chosen, samples
#' at specified well positions on the plate are considered invalid. The function generates lists of treatments and
#' concentration levels based on the direction parameter, i.e. the direction of the treatments and concentration levels
#' applied (either horizontally or vertically on the plate). To add metadata to several plates at the same time, the
#' functions \code{tidy_plates_via_params} and \code{tidy_plates_via_prompts} are recommended.
#' 
#' For all three functions, \code{tidy_plate}, \code{tidy_plates_via_params}, and \code{tidy_plates_via_prompts}, to work
#' properly, file names should provide a file identifier (i.e. "bma" in
#' case there are additional but unused files in the folder), an identifier for experiments (starting with "exp" followed
#' by a number, e.g. "exp1") and an identifier for timepoints (starting with the upper- or lower-case letter t followed
#' by a number, e.g. "T0" or "t0").
#' @seealso
#' \code{\link{tidy_plates_via_params}}, \code{\link{tidy_plates_via_prompts}}
#' @export
tidy_single_plate <- function(input_data,
                       direction = c("horizontal","vertical"),
                       group_ID = NULL,
                       experiment_name = NULL,
                       validity_method = c("threshold","invalid"),
                       threshold = NULL,
                       invalid_samples = NULL,
                       treatment_labels,
                       concentration_levels,
                       ...) {
  # Read data
  raw_data <- read_plates(input_data, ...)
  
  # Check if input is empty
  if (!length(raw_data)) {
    stop("Content is empty. Check input.")
  }

  # Get row and column names separately
  row_names <- rownames(raw_data[[1]])
  col_names <- colnames(raw_data[[1]])
  
  # Generate list of groups
  group_list <- generate_group_list(group_ID, names(raw_data))
  
  # Generate list of experiments
  experiment_list <- generate_experiment_list(experiment_name, names(raw_data))
  
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
    
  # Check validity of each cell in the data matrix
  tidy_data <- validate_cells(raw_data[[1]], row_names, col_names, validity_method, threshold, invalid_samples)
  
  # Add treatment information
  tidy_data <- add_treatment(tidy_data, treatment_list, ask_treatment_list = FALSE)
  
  # Add concentration information
  tidy_data <- add_concentration(tidy_data, concentration_list, ask_concentration_list = FALSE)
  
  # Extract file name, time points and experiment identifier from file name
  timepoint <- str_extract(names(raw_data), regex("[tT][0-9]"))
  group <- str_extract(names(raw_data)[1], regex("grp[0-9]"))
  experiment <- str_extract(names(raw_data), regex("exp[0-9]"))
  
  # Add file name and time points to corresponding columns
  tidy_data$Timepoint <- timepoint
  tidy_data$File <- names(raw_data)
  tidy_data$Group <- group_list[[group]]
  tidy_data$Experiment <- experiment_list[[experiment]]
  
  return(tibble::tibble(tidy_data))
}
