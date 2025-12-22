is.empty <- function(x, mode = NULL, ...) {
  if (is.null(x)) {
    warning("x is NULL")
    return(FALSE)
  }
  if (is.null(mode)) {
    mode <- class(x)
  }
  identical(vector(mode, 1), c(x, vector(class(x), 1)))
}
#' get_block
#'
#' Get a block (e.g., $DATA) from a control file with multiple lines
#' return block as a single character string
#'
#' @param stem Block title, e.g., $DATA to look for
#' @param control control file text
#' @return a block with specified header
#' returns empty character if nothing is found
#'
#' @examples
#' \dontrun{
#' get_block("$PROB", "$PROB test \n$INPUT ...")
#' }
#' @noRd
get_block <- function(stem, control) {
  OneLineSemicolon <- "(?:;(?:\\\\\\n|[^\\n])*(?=$|\\n))"

  tryCatch(
    {
      CollapsedControl <- paste(control, collapse = "\n")

      StatementsLineswoComm <-
        gsub(OneLineSemicolon, "\n", CollapsedControl, perl = TRUE)

      Blocks <- paste0("$", unlist(strsplit(StatementsLineswoComm, split = "\\$")))
      Block <- Blocks[grepl(paste0("^\\", stem), Blocks)]
      Block <- gsub("\r?\n", " ", Block)
      Block
    },
    error = function(err) {
      character(0)
    }
  )
}

remove_old_files <- function(run_dir, samp_size) {
  msg <- ""
  # check files that need to be written to, try to delete them
  files_to_remove <-
    file.path(
      run_dir,
      c(
        "All_results.csv",
        "BICS.csv",
        "Parameters.csv",
        "Power.csv"
      )
    )
  files_to_remove <-
    c(
      files_to_remove,
      file.path(run_dir, paste0("data_samp", 1:samp_size, ".csv")),
      file.path(
        run_dir,
        paste0("MBBE", 1:samp_size),
        paste0("NCAresults", 1:samp_size, ".csv")
      ),
      file.path(
        run_dir,
        paste0("MBBE", 1:samp_size),
        paste0("MBBE", 1:samp_size, ".mod")
      ),
      file.path(run_dir, paste0("MBBEsim", 1:samp_size), "OUT.DAT")
    )

  files_to_remove <-
    files_to_remove[file.exists(files_to_remove)]

  count <- 0
  while (any(!file.remove(files_to_remove)) & count < 10) {
    files_to_remove <-
      files_to_remove[file.exists(files_to_remove)]
    count <- count + 1
    Sys.sleep(0.1)
  }

  if (length(files_to_remove[file.exists(files_to_remove)]) > 0) {
    msg <- paste(
      "Unable to delete required output file(s) ",
      paste(files_to_remove, collapse = ", ")
    )
  }


  return(msg)
}
