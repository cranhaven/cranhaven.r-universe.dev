#' @title Add treatment metadata
#' @description \code{add_treatment} adds treatment metadata to photometer data that is specified in long format.
#' For this function to work properly, the column containing the well positions should be named 'Position'
#' and the column containing the corresponding absorption values should be named 'Values'.
#' @param input_data A data frame with well positions and their corresponding values.
#' @param treatment_list A list of plate axes as keys and treatment labels as values (optional)
#' @param ask_treatment_list A boolean parameter indicating whether treatment labels should be retrieved via user prompt (default) or not.
#' @param ... Additional arguments to be passed to \code{\link{ask_treatment_list}}.
#' @return \code{add_treatment} returns a data frame with treatment information added.
#' @seealso
#' \code{\link{generate_experiment_list}}, \code{\link{ask_experiment_list}},
#' \code{\link{generate_group_list}}, \code{\link{ask_group_list}},
#' \code{\link{add_concentration}}, \code{\link{validate_cells}}
#' @export
add_treatment <- function(input_data,
                          treatment_list = NULL,
                          ask_treatment_list = TRUE,
                          ...) {
  if (ask_treatment_list == TRUE) {
    treatment_list <- ask_treatment_list(...)
  } else {
    treatment_list = treatment_list
  }
  input_data$Treatment <- sapply(input_data$Position, match_treatment, treatment_list)
  input_data$Treatment <- as.character(input_data$Treatment)
  return(input_data)
}


#' @title Generate list of treatment labels from user parameters
#' @description \code{generate_treatment_list} generates a list of provided treatment labels mapped to the user-specified
#' plate layout. The plate layout is based on a 96-well plate and can be either horizontal (i.e. letters A-H) or vertical (i.e. numbers 1-12).
#' @param treatment_labels A character vector containing treatment labels.
#' @param direction A character vector specifying the orientation of the plate layout. 
#'                  It can be either "horizontal" or "vertical".
#' @return \code{generate_treatment_list} returns a list of treatment labels where each level is assigned to a corresponding 
#'         row or column based on the selected direction parameter.
#' @details
#' \code{generate_treatment_list} checks if the length of treatment_labels matches the specified 
#' number of rows or columns based on the direction parameter. If not, it throws an error.
#' If the lengths match, it generates a list of treatment_labels where each label is 
#' assigned to a corresponding row or column based on the direction parameter.
#' @rdname add_treatment
#' @export
generate_treatment_list <- function(treatment_labels, direction){
  if (length(treatment_labels) != 12 && direction == "vertical" ||
      length(treatment_labels) != 8 && direction == "horizontal") {
    stop("Number of treatment labels must match the number of rows or columns as chosen by the direction parameter.")
  } else {
    treatment_list <- switch(direction,
                             "horizontal" = as.list(setNames(treatment_labels, LETTERS[1:8])),
                             "vertical" = as.list(setNames(treatment_labels, 1:12)))
    return(treatment_list)
  }
}


#' @title Retrieve treatment information via user prompt
#' @description \code{ask_treatment_list} works the same way as \code{generate_treatment_list}, but retrieves the
#' treatment labels based on a user prompt instead of user-set parameters. The plate axis can be either in horizontal direction
#' providing letters A-H or in vertical direction providing numbers 1-12 based on a 96-well plate layout.
#' @return \code{ask_treatment_list} returns a list containing plate axes as keys and treatment labels as values.
#' @rdname add_treatment
#' @export
ask_treatment_list <- function(direction=c("horizontal","vertical")) {
  direction <- match.arg(direction)
  
  # Check plate direction
  if (direction == "horizontal") {
    plate_lab = LETTERS[1:8]
    axis_label <- "row"
  } else {
    plate_lab = 1:12
    axis_label <- "column"
  }
  
  treatment_list <- setNames(rep(list(NULL), length(plate_lab)), plate_lab)
  
  for (axis in plate_lab) {
    cat(sprintf("Enter treatment of %s %s: ", axis_label, axis))
    treatment_label <- readLines(con = getOption("microdiluteR.connection"),
                                     n = 1)
    
    if (treatment_label == "") {
      message("Missing value. Please enter valid input.\n")
      return(NULL)
    }
    
    treatment_list[[axis]] <- treatment_label
  }
  
  attr(treatment_list, "axis") <- axis_label
  return(treatment_list)
}


#' @title Check if a sample position(s) match the treatment criteria
#' @description \code{match_treatment} maps treatment labels to corresponding well positions and returns 'NA' otherwise.
#' @param well_position The sample position(s) to check
#' @param treatment_list A list containing treatment information
#' @return \code{match_treatment} returns the corresponding treatment labels if sample position matches treatment criteria, "NA" otherwise
#' @rdname add_treatment
#' @export
match_treatment <- function(well_position, treatment_list) {
  for (treatment_axis in names(treatment_list)) {
    if (grepl(paste0("\\b", treatment_axis, "\\b"), well_position)) {
      return(treatment_list[[treatment_axis]])
    }
  }
  return("NA")
}
