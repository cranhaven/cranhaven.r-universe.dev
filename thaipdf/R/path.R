


# Construct Path ----------------------------------------------------------


#' Construct Path to files in thatpdf package
#'
#' @param th_pre preamble filename
#'
#' @return a list of paths
#' @noRd
thaipdf_paths <- function(th_pre = "thai-preamble.tex") {

  ## Template file name
  temp_filename <- paste0("template-", th_pre)

  # Input Path
  ## Template path
  path_temp <- fs::path_package("thaipdf", "templates", temp_filename)
  ## before_body.tex for \sloppy macro
  path_before_body <- fs::path_package("thaipdf", "templates", "before_body.tex")
  # At rmarkdown/
  ### Thai PDF with Preamble (for Project)
  path_pre_proj <- fs::path_package("thaipdf", "rmarkdown", "templates",
                                    "thai-pdf-rmd-w-pre",
                                    "skeleton", "pre-tex", th_pre)
  list(
    path_temp = path_temp,
    path_before_body = path_before_body,
    path_pre_proj = path_pre_proj
  )
}


# Path to before body -----------------------------------------------------



#' Path to Global before body
#'
#' @noRd
before_body <- function(){
  thaipdf_paths()[["path_before_body"]]
}

