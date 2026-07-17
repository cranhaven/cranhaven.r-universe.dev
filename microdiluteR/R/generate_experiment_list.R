#' @title Generate list of experiment names from user parameters
#' @description \code{generate_experiment_list} generates a list of provided experiment names extracted
#' from file names.
#' @param experiment_names A character vector containing names for each experiment.
#' @param file_list A character vector of file IDs. Used to extract experiment IDs from.
#' @return \code{generate_experiment_list} returns a list of experiment names where each level is assigned to a corresponding 
#'         row or column based on the selected direction parameter.
#' @details
#' \code{generate_experiment_list} extracts unique identifiers from file names and matches them with the 
#' provided experiment names. If the number of experiment names does not match the number 
#' of unique identifiers extracted from the file names, it throws an error. If the lengths match,
#' it generates a list of experiment names where each name is associated with a unique
#' identifier extracted from the file names.
#' @seealso
#' \code{\link{generate_group_list}}, \code{\link{ask_group_list}},
#' \code{\link{add_treatment}}, \code{\link{add_concentration}}
#' @export
generate_experiment_list <- function(experiment_names, file_list){
  experiments <- sort(unique(str_extract(file_list, pattern="exp[0-9]")))
  if (length(experiment_names) != length(experiments)) {
    stop("Number of experiment names must match the number of identifiers used in file names.")
  } else {
    experiment_list <- as.list(setNames(experiment_names, experiments))
    return(experiment_list)
  }
}


#' @title Retrieve experiment information via user prompt
#' @description \code{ask_experiment_list} works the same way as \code{generate_experiment_list}, but retrieves the
#' experiment names based on a user prompt instead of user-set parameters.
#' @param file_list A character vector of file IDs. Used to extract experiment IDs from.
#' @return \code{ask_experiment_list} returns a list containing experiment identifiers as keys and experiment names as values.
#' @rdname generate_experiment_list
#' @export
ask_experiment_list <- function(file_list) {
  experiments <- sort(unique(str_extract(file_list, pattern="exp[0-9]")))
  experiment_list <- setNames(rep(list(NULL), length(experiments)), experiments)
  
  for (experiment in names(experiment_list)) {
    cat(sprintf("Enter experiment identifier for %s: ", experiment))
    experiment_label <- readLines(con = getOption("microdiluteR.connection"), n = 1)

    if (experiment_label == "") {
      message("Missing value. Please enter valid input.\n")
      return(NULL)
    }
    experiment_list[[experiment]] <- experiment_label
  }
  return(experiment_list)
}
