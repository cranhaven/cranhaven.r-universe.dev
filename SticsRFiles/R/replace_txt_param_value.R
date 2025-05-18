#' @title Replacing value in STICS parameters files
#'
#' @description Replacing one or several values in a source file
#' corresponding to a parameter tag for generating one (in place or not)
#' or several files
#'
#' @param file_path The file path
#' @param param_tag The parameter name
#' @param param_value The parameter values
#' @param out_file_path The new file path
#'
#' @keywords internal
#'
#' @noRd
#'
replace_txt_param_value <- function(file_path,
                                    param_tag,
                                    param_value,
                                    out_file_path = NULL) {

  # default values for redirecting output to another file, or other files
  # if several values and several output files !
  redir <- paste0(" > ", out_file_path)
  opt <- ""
  old <- file_path
  in_place <- FALSE
  new <- out_file_path
  # if the target file is the input file
  # in place modification
  if (base::is.null(out_file_path)) {
    opt <- " -i "
    redir <- ""
    old <- file.path(dirname(file_path), "tmp.txt")
    file.copy(file_path, old)
    new <- file_path
    in_place <- TRUE
  }

  if (length(param_value) > 1 && length(param_value) != length(out_file_path)) {
    stop()
  }

  # filtering character ':'
  param_tag <- gsub(":", "", param_tag)

  # character vector of system commands
  cmd <- paste0("sed ",
                opt,
                "'/\\b",
                param_tag,
                "\\b/{n;s/.*/",
                as.character(param_value),
                "/}'",
                " ",
                file_path,
                redir)

  lapply(cmd, function(x) system(x, intern = TRUE))

  # testing if replacement is ok
  # 1- in place against a temporary file
  # 2- with one or several output files
  # diff for a file or several files
  diff_files <- lapply(new,
                       function(x) {
                         system(paste("diff",
                                      old,
                                      x,
                                      "| wc -l"),
                                intern = TRUE)
                       }
  )

  diff_files <- as.numeric(diff_files)
  not_replaced_files <- diff_files == 0
  if (any(not_replaced_files)) {
    message("No replacement for file(s):")
    message(out_file_path[not_replaced_files])
  }
  # removing the temporary file
  if (in_place) {
    file.remove(old)
  }
  # returning replacement success as logical vector
  # for one or several output files
  return(invisible(!not_replaced_files))
}
