#' @title Check validity of each cell in data frame.
#' @description \code{validate_cells} checks if samples are valid based on either a user-set threshold (i.e.
#' a maximum absorption value) or a list of invalid samples provided by the user.
#' @param raw_data The original data frame.
#' @param row_names Row names of the original data frame.
#' @param col_names Column names of the original data frame.
#' @param validity_method The user-preferred validity method. Either 'threshold' or 'samples'.
#' @param threshold The threshold for distinguishing valid from invalid samples. Only applied if
#' parameter 'validity method' is set to 'threshold'. Defaults to \code{NULL}.
#' @param invalid_samples Vector of invalid samples as defined by their well positions. Only applied if
#' parameter 'validity method' is set to 'samples'.
#' @return \code{validate_cells} returns a data frame with validity information
#' @seealso
#' \code{\link{generate_experiment_list}}, \code{\link{ask_experiment_list}},
#' \code{\link{generate_group_list}}, \code{\link{ask_group_list}},
#' \code{\link{add_treatment}}, \code{\link{add_concentration}}, \code{\link{validate_cells}}
#' @export
validate_cells <- function(raw_data,
                           row_names,
                           col_names,
                           validity_method = c("threshold", "samples"),
                           threshold = NULL,
                           invalid_samples = NULL) {
  validated_data <- data.frame()
  for (i in 1:nrow(raw_data)) {
    for (j in 1:ncol(raw_data)) {
      value <- raw_data[i, j]
      validity <- apply_validation_method(value, i, j, row_names, col_names, validity_method, threshold, invalid_samples)
      well_position <- paste(row_names[i], col_names[j], sep = "-")
      validated_data <- rbind(validated_data, data.frame(Position = well_position, Value = value, Validity = validity))
    }
  }
  return(validated_data)
}

#' @title Assess validity of samples
#' @description \code{apply_validation_method} evaluates whether a sample meets a user-set validity criteria based 
#' on a specified validity method.
#' @param value The value to be checked for validity.
#' @param i The row index of the value in the matrix or data frame.
#' @param j The column index of the value in the matrix or data frame.
#' @param row_names Names or identifiers of rows in the matrix or data frame.
#' @param col_names Names or identifiers of columns in the matrix or data frame.
#' @param validity_method The method used to determine validity. Either 'threshold' or 'samples'.
#' @param threshold A threshold value used for determining validity. Only applied if 'validity_method
#' is set to 'threshold'.
#' @param invalid_samples A container for storing invalid samples or their indices. Only applied if 'validity_method
#' is set to 'samples'.
#' @return \code{apply_validation_method} returns logical value indicating whether the value meets the validity criteria.
#' @rdname validate_cells
#' @export
apply_validation_method <- function(value,
                           i, j,
                           row_names, col_names,
                           validity_method = c("threshold", "samples"), 
                           threshold = NULL, invalid_samples = NULL) {
  
  # Get position of samples to check
  well_position <- paste0(row_names[i], "-", col_names[j])
  
  # Apply validity method to sample
  if (validity_method == "threshold") {
    if (value <= threshold) return("valid")
    return("invalid")
  } 
  if (validity_method == 'samples') {
    if (well_position %in% invalid_samples) return("invalid")
    return("valid")
  } 
  
  stop("Invalid option selected. Please choose either 'threshold' or 'samples'.")
  
}


#' @title Ask user for validity method preference
#' @description \code{ask_validity_method} applies a user prompt to check for the validation method to apply
#' on the samples. This can be either 'threshold' (then a maximum absorption value is asked via a call to function
#' \code{ask_threshold}) or 'samples'
#' @rdname validate_cells
#' @return The user's validity method preference
ask_validity_method <- function() {
  cat("Enter 'threshold' for specifying a threshold or 'samples' for directly providing invalid samples: ")
  option <- readLines(con = getOption("microdiluteR.connection"), n = 1)

  # Make option case-insensitive
  option <- tolower(option)
  
  # Check options
  if (option %in% c("threshold", "samples")) {
    return(option)
  } else {
    stop("Invalid option selected: ", option, ". Please choose either 'threshold' or 'samples'.")
  }
}


#' Ask user for a threshold
#' @description \code{ask_threshold} applies a user prompt to check for a valid absorption maximum used as a threshold.
#' @rdname validate_cells
#' @return \code{ask_threshold} returns the user-specified threshold
ask_threshold <- function() {
  cat("Enter threshold for distinguishing valid from invalid samples: ")
  threshold <- readLines(con = getOption("microdiluteR.connection"), n = 1)
  threshold <- suppressWarnings(as.numeric(threshold))
  
  # Check if character input was converted to NA (if not numeric)
  if (is.na(threshold)) {
    stop("Please enter a numeric value for 'threshold'.")
  }
  
  return(threshold)
}


#' @title Ask user for invalid samples
#' @description \code{ask_invalid_samples} applies a user prompt to check for invalid samples.
#' @rdname validate_cells
#' @return \code{ask_invalid_samples} returns a vector of invalid samples
ask_invalid_samples <- function() {
  cat("Enter well positions (e.g. A-2) of invalid samples separated by spaces: ")
  invalid_samples <- readLines(con = getOption("microdiluteR.connection"), n = 1)
  
  # Validate input format
  if (!grepl("^([A-H]-[1-9][0-2]?\\s?)+$", invalid_samples)) {
    # Invalid input format
    stop("Invalid input format. Please enter well positions separated by spaces in the format 'A-2'.")
  }
  
  # Split and return well positions
  return(unlist(strsplit(invalid_samples, " ")))
}

#' @title Update Validity based on specified position and group levels
#' @description \code{update_validity} updates the Validity column in a dataframe based on a specified position
#' and combinations of factors. It sets the Validity to "invalid" for rows where the Position
#' matches the specified position and where the combinations of factors A, B, and C match the
#' provided group levels.
#' @param input_data A dataframe containing the data to be updated.
#' @param wp_var A character string specifying the column providing the well positions. Defaults to
#' "Position".
#' @param well_positions The well positions to filter the data on.
#' @param group_levels A list specifying the combinations of factors A, B, and C to match.
#'        Each element of the list should be a vector of factor levels.
#' @return \code{update_validity} returns the updated dataframe with Validity modified accordingly.
#' @rdname validate_cells
#' @examples
#' df <- data.frame(Position = c("pos1", "pos2", "pos2", "pos4", "pos4"),
#'                  Value = c(1, 2, 3, 4, 5),
#'                  Validity = c("valid", "valid", "valid", "valid", "valid"),
#'                  A = c("a1", "a2", "a3", "a1", "a2"),
#'                  B = c("b1", "b2", "b3", "b1", "b2"),
#'                  C = c("c1", "c2", "c3", "c1", "c2"))
#' updated_df <- update_validity(df,
#'                               well_positions = "pos2",
#'                               group_levels = list(A = c("a2", "a3"), B = c("b2", "b3")))
#' updated_df
#' @export
update_validity <- function(input_data,
                            wp_var = "Position",
                            well_positions,
                            group_levels = NULL) {
  
  # Change to data.frame (in case it was a tibble)
  input_data <- data.frame(input_data)
  
  # Filter rows based on well_positions
  subset_data <- input_data[input_data[[wp_var]] %in% well_positions, ]
  
  # Identify rows that match the specified group levels and set as invalid
  if (nrow(subset_data) != 0) {
    match_rows <- which(rownames(input_data) %in% rownames(subset_data))
    input_data[rownames(input_data) %in% match_rows, "Validity"] <- "invalid"
  }
  
  return(input_data)
}

