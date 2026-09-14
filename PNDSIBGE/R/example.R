#' Get the path of the PNDS toy example files
#' @description This function provides the path of the microdata from year 2023 of the PNDS toy example files, loaded with this package.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param path Name of file. If \code{NULL}, the PNDS toy example files names will be listed.
#' @return A vector with names of all the available PNDS toy example files or the path for specific requested PNDS toy example file.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-demografia-e-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNDSIBGE]{get_pnds} for downloading, labeling, deflating and creating survey design object for PNDS microdata.\cr \link[PNDSIBGE]{read_pnds} for reading PNDS microdata.\cr \link[PNDSIBGE]{pnds_labeller} for labeling categorical variables from PNDS microdata.\cr \link[PNDSIBGE]{pnds_deflator} for adding deflator variables to PNDS microdata.\cr \link[PNDSIBGE]{pnds_design} for creating PNDS survey design object.
#' @examples
#' pnds_example()
#' pnds_example(path="exampledata.txt")
#' pnds_example(path="input_example.txt")
#' pnds_example(path="dictionaryexample.xls")
#' pnds_example(path="deflatorexample.xls")
#' @export

pnds_example <- function(path = NULL) {
  message("The pnds_example function is under development and will be available soon in package PNDSIBGE.\n")
  return(NULL)
  if (is.null(path)) {
    dir(system.file("extdata", package="PNDSIBGE"))
  }
  else {
    system.file("extdata", path, package="PNDSIBGE", mustWork=TRUE)
  }
}
