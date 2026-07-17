#' @title Add concentration metadata
#' @description \code{add_concentration} adds concentration metadata to photometer data that is specified in long format.
#' For this function to work properly, the column containing the well positions should be named 'Position'
#' and the column containing the corresponding absorption values should be named 'Values'.
#' @param input_data A data frame with well positions and their corresponding values.
#' @param concentration_list A list of plate axes as keys and concentration levels as values (optional)
#' @param ask_concentration_list A boolean parameter indicating whether concentration levels should be retrieved via user prompt (default) or not.
#' @param ... Additional arguments to be passed to \code{\link{ask_concentration_list}}.
#' @return \code{add_concentration} returns a data frame with concentration metadata added.
#' @seealso
#' \code{\link{generate_experiment_list}}, \code{\link{ask_experiment_list}},
#' \code{\link{generate_group_list}}, \code{\link{ask_group_list}},
#' \code{\link{add_treatment}}, \code{\link{validate_cells}}
#' @export
add_concentration <- function(input_data,
                              concentration_list = NULL,
                              ask_concentration_list = TRUE,
                              ...) {
  if (ask_concentration_list) {
    concentration_list <- ask_concentration_list(...)
  } else {
    concentration_list = concentration_list
  }
  input_data$Concentration <- sapply(input_data$Position, match_concentration, concentration_list)
  input_data$Concentration <- as.numeric(input_data$Concentration)
  return(input_data)
}


#' @title Generate list of concentration levels from user parameters
#' @description \code{generate_concentration_list} generates a list of provided concentration levels mapped to the user-specified
#' plate layout. The plate layout is based on a 96-well plate and can be either horizontal (i.e. letters A-H) or vertical (i.e. numbers 1-12).
#' @param concentration_levels A numeric vector containing concentration levels.
#' @param direction A character vector specifying the orientation of the plate layout. 
#'                  It can be either "horizontal" or "vertical".
#' @return \code{generate_concentration_list} returns a list of concentration levels where each level is assigned to a corresponding 
#'         row or column based on the selected direction parameter.
#' @details
#' \code{generate_concentration_list} checks if the length of \code{concentration_levels} matches the specified 
#' number of rows or columns based on the direction parameter. If not, it throws an error.
#' If the lengths match, it generates a list of concentration levels where each level is 
#' assigned to a corresponding row or column based on the direction parameter.
#' @rdname add_concentration
#' @export
generate_concentration_list <- function(concentration_levels, direction){
  if (length(concentration_levels) != 12 && direction == "vertical" ||
      length(concentration_levels) != 8 && direction == "horizontal") {
    stop("Number of concentration levels must match the number of rows or columns as chosen by the direction parameter.")
  } else {
    concentration_list <- switch(direction,
                                 "horizontal" = as.list(setNames(concentration_levels, LETTERS[1:8])),
                                 "vertical" = as.list(setNames(concentration_levels, 1:12)))
    return(concentration_list)
  }
}


#' @title Retrieve concentration information via user prompt
#' @description \code{ask_concentration_list} works the same way as \code{generate_concentration_list}, but retrieves the
#' concentration levels based on a user prompt instead of user-set parameters. The plate axis can be either in horizontal direction
#' providing letters A-H or in vertical direction providing numbers 1-12 based on a 96-well plate layout.
#' @param direction A character vector specifying the orientation of the plate layout. 
#' It can be either "horizontal" or "vertical".
#' @return \code{ask_concentration_list} returns a list containing plate axes as keys and concentration information as values.
#' @rdname add_concentration
#' @export
ask_concentration_list <- function(direction = c("horizontal","vertical")) {
  direction <- match.arg(direction)
  
  # Check plate direction
  if (direction == "horizontal") {
    plate_lab = LETTERS[1:8]
    axis_label <- "row"
  } else {
    plate_lab = 1:12
    axis_label <- "column"
  }
  
  concentration_list <- setNames(rep(list(NULL), length(plate_lab)), plate_lab)
  
  for (axis in plate_lab) {
    cat(sprintf("Enter concentration of %s %s: ", axis_label, axis))
    concentration_label <- readLines(con = getOption("microdiluteR.connection"),
                                     n = 1)

    if (concentration_label == "") {
      message("Missing value. Please enter valid input.\n")
      return(NULL)
    }
    
    concentration_list[[axis]] <- concentration_label
  }

  attr(concentration_list, "axis") <- axis_label
  return(concentration_list)
}

#' @title Check if a sample position(s) match the concentration criteria
#' @description \code{match_concentration} maps concentration levels to corresponding well positions and returns 'NA' otherwise.
#' @param well_position The sample position(s) to check
#' @param concentration_list A list containing concentration information
#' @return \code{match_concentration} returns the corresponding concentration level if sample position matches concentration criteria, "NA" otherwise
#' @rdname add_concentration
#' @export
match_concentration <- function(well_position, concentration_list) {
  for (concentration_axis in names(concentration_list)) {
    if (grepl(paste0("\\b", concentration_axis, "\\b"), well_position)) {
      return(concentration_list[[concentration_axis]])
    }
  }
  return("NA")
}
