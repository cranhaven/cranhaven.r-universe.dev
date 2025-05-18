#' @title Generating a param.sti type file
#' @description Generating a parameters forcing file from parameters names
#' and parameters values vectors
#' @param workspace STICS or JavaSTICS workspace path
#' @param par_names vector of parameters names
#' @param par_values vector of parameters values
#' @param file_name file name to generate (default value: param.sti)
#'
#' @return A logical status TRUE if successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' gen_paramsti(".", c("par1", "par2"), c(1, 2))
#' gen_paramsti("/path/to/stics/workspace", c("par1", "par2"), c(1, 2))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_paramsti <- function(workspace,
                         par_names,
                         par_values,
                         file_name = "param.sti") {

  # Checking if workspace exists
  if (!dir.exists(workspace)) {
    stop(paste(workspace, ": directory does not exist !"))
  }

  # Full file path & removing file if exists
  file_path <- file.path(workspace, file_name)
  if (file.exists(file_path)) {
    file.remove(file_path)
  }

  # Checking parameters vectors consistency
  # exiting if not
  nb_par <- unique(c(length(par_names), length(par_values)))
  if (length(nb_par) > 1) {
    return(FALSE)
  }

  # TODO: checking if par_names exist (related to STICS version) ?

  # Writing file content
  con <- file(file_path, method = "w+")
  w <- try(write(
    paste0(
      nb_par, "\n",
      paste0(sprintf("%s\n%f", par_names, par_values), collapse = "\n")
    ),
    con
  ))

  close(con)

  # Checking if any error writing the file
  if (methods::is(w, "try-error")) {
    return(invisible(FALSE))
  }

  # Returning file creation status
  return(invisible(TRUE))
}
