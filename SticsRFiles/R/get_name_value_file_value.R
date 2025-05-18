#' Getting parameters values from a STICS text file with name/value on
#' following lines
#'
#' @param file_path STICS text file path
#' @param param_names vector of parameters names
#' @param names_dict List of correspondence between parameter names
#' given and files intern names
#'
#' @return A vector of parameters values
#'
#'
# @examples
#' @keywords internal
#'
#' @noRd
#'
get_name_value_file_value <- function(file_path,
                                      param_names,
                                      names_dict = NULL) {

  # Case plt, station, tempopar.sti, tempoparv6.sti

  lines_list <- readLines(file_path, warn = FALSE)


  # removing leading/trailing blanks and multiple blanks
  lines_list <- strsplit(gsub("\\s+", " ", trimws(lines_list)), " ")


  couples_nb <- length(lines_list) / 2

  sel <- rep(c(TRUE, FALSE), couples_nb)

  par_list <- unlist(lines_list[sel])
  # Getting the sub list according to
  par_val <- lines_list[!sel]


  par_val <- par_val[par_list %in% param_names]


  par_idx <- !is.na(lapply(par_val, as.numeric))
  num_val <- lapply(par_val[par_idx], function(x) x <- as.numeric(x))
  par_val[par_idx] <- num_val


  # naming the returned list
  names(par_val) <- param_names

  return(par_val)
}
