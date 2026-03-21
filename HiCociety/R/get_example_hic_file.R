#' Example download
#'
#' This function downloads an example Hi-C file from chromosome 19 of mice (mm10) Th1 cells.
#'
#' @param destfile Path to save the example Hi-C file
#' @return Hi-C file download at destfile.

get_example_hic_file <- function(destfile = "example.hic") {
  url <- "https://github.com/ysora/HiCociety/raw/main/example.hic"
  if (!file.exists(destfile)) {
    message("Downloading example.hic file...")
    utils::download.file(url, destfile, mode = "wb")
  } else {
    message("Example .hic file already exists.")
  }
  return(destfile)
}

