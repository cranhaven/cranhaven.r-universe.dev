#' @title Generate list of group IDs from user parameters
#' @description \code{generate_group_list} generates a list of provided group IDs extracted
#' from file IDs.
#' @param group_names A character vector containing IDs for each group.
#' @param file_list A character vector of file IDs. Used to extract group IDs from.
#' @return \code{generate_group_list} returns a list of group IDs where each level is assigned to a corresponding 
#'         row or column based on the selected direction parameter.
#' @details
#' \code{generate_group_list} extracts unique identifiers from file IDs and matches them with the 
#' provided group IDs. If the number of group IDs does not match the number 
#' of unique identifiers extracted from the file IDs, it throws an error. If the lengths match,
#' it generates a list of group IDs where each ID is associated with a unique
#' identifier extracted from the file IDs.
#' @seealso
#' \code{\link{generate_experiment_list}}, \code{\link{ask_experiment_list}},
#' \code{\link{add_treatment}}, \code{\link{add_concentration}}
#' @export
generate_group_list <- function(group_names, file_list){
  groups <- sort(unique(str_extract(file_list, pattern="grp[0-9]")))
  if (length(group_names) != length(groups)) {
    stop("Number of group IDs must match the number of identifiers used in file names.")
  } else {
    group_list <- as.list(setNames(group_names, groups))
    return(group_list)
  }
}


#' @title Retrieve group information via user prompt
#' @description \code{ask_group_list} works the same way as \code{generate_group_list}, but retrieves the
#' group IDs based on a user prompt instead of user-set parameters.
#' @param file_list A character vector of file IDs. Used to extract group IDs from.
#' @return \code{ask_group_list} returns a list containing group identifiers as keys and group IDs as values.
#' @rdname generate_group_list
#' @export
ask_group_list <- function(file_list) {
  groups <- sort(unique(str_extract(file_list, pattern="grp[0-9]")))
  group_list <- setNames(rep(list(NULL), length(groups)), groups)

  for (group in names(group_list)) {
    cat(sprintf("Enter group name for %s: ", group))
    group_label <- readLines(con = getOption("microdiluteR.connection"), n = 1)

    if (group_label == "") {
      message("Missing value. Please enter valid input.\n")
      return(NULL)
    }
    group_list[[group]] <- group_label
  }
  return(group_list)
}
