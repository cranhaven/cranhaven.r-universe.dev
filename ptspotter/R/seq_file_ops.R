#' seq_file_ops
#'
#' Quickly create the required number of sequentially labelled files.
#'
#' @param n The number of files to create. Also accepts numerical vector.
#'
#' @param target_dir Directory to create files. Creates the directory if
#' file.exists(target_dir) evaluates to FALSE.
#'
#' @param filetype The suffix to append the filename. Defaults to ".R".
#'
#' @param force Defaults to FALSE. If set to TRUE, seq_file_ops will
#' overwrite any pre-existing files that match the write filenames asked for.
#'
#' @return Write a series of sequentially numbered files within a specified
#' directory. Creates the directory if required.
#'
#' @importFrom stringr str_count
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#'
#' seq_file_ops(n = 10, target_dir = "munge")
#'
#' seq_file_ops(n = c(1, 3:8, 10), target_dir = "complex_vector")
#'
#' # if force == FALSE, pre-existing numbered scripts will not be overwritten
#' # only 02-.R and 09-.R are written below
#' seq_file_ops(10, target_dir = "complex_vector")
#'
#' unlink("munge", recursive = TRUE)
#' unlink("complex_vector", recursive = TRUE)
#'
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
seq_file_ops <- function(n, target_dir = NULL, filetype = "R",
                         force = FALSE) {
  # handle input params -----------------------------------------------------

  if (!file.exists(target_dir)) {
    # if the directory doesn't exist, create it with a prompt.
    dir.create(target_dir)
    message(paste(
      "seq_file_ops() created",
      paste0(target_dir, "/"), "as it was not found."
    ))
  }

  # create file vector ------------------------------------------------------

  if (length(n) == 1) {
    # if user specifies n as single digit, create required vector
    req_nos <- c(1:n)
  } else if (length(n) > 1) {
    # if user provides a vector for n, just use vector
    req_nos <- n
  }

  # wherever the digits are single, add a 0 in front
  req_nos[str_count(req_nos) == 1] <- paste0(
    "0", req_nos[str_count(req_nos) == 1]
  )

  # create the filenames
  req_files <- paste0(
    paste(target_dir, req_nos, sep = "/"),
    "-.",
    filetype
  )

  # write to file -----------------------------------------------------------

  if (force == FALSE) {
    # find any preexisting files
    ex_files <- req_files[file.exists(req_files)]
    # only message warning if at least one existing file is found
    if (length(ex_files) > 0) {
      # message message
      message(paste(
        "Following found files will not be overwritten:",
        paste(ex_files, collapse = ", ")
      ))
    }

    # write only new files
    only_new <- req_files[!(req_files %in% ex_files)]
    invisible(file.create(only_new))
    # message conf msg
    message(paste("New files created:", paste(only_new, collapse = ", ")))
  } else {
    # message conf msg
    warning(paste(
      "force = TRUE. Files may be overwritten. Following files created",
      paste(req_files, collapse = ", ")
    ))
    # write all files asked for
    invisible(file.create(req_files))
  }
}
