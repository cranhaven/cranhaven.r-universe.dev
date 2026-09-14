#' Create PNDS survey object with its sample design
#' @description This function creates PNDS survey object with its sample design for analysis using \code{survey} package functions.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pnds A tibble of PNDS microdata read with \code{read_pnds} function.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from PNDS and its sample design.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-demografia-e-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNDSIBGE]{get_pnds} for downloading, labeling, deflating and creating survey design object for PNDS microdata.\cr \link[PNDSIBGE]{read_pnds} for reading PNDS microdata.\cr \link[PNDSIBGE]{pnds_labeller} for labeling categorical variables from PNDS microdata.\cr \link[PNDSIBGE]{pnds_deflator} for adding deflator variables to PNDS microdata.\cr \link[PNDSIBGE]{pnds_example} for getting the path of the PNDS toy example files.
#' @examples
#' # Using data read from disk
#' input_path <- pnds_example(path="input_example.txt")
#' data_path <- pnds_example(path="exampledata.txt")
#' dictionary.path <- pnds_example(path="dictionaryexample.xls")
#' deflator.path <- pnds_example(path="deflatorexample.xls")
#' pnds.df <- read_pnds(microdata=data_path, input_txt=input_path, vars=c("J007","J009"))
#' pnds.df <- pnds.df[(pnds.df$M001 == "1" & !is.na(pnds.df$M001)),]
#' pnds.df <- pnds.df[,!(names(pnds.df) %in% c("V0029", "V00291", "V00292", "V00293"))]
#' pnds.df <- pnds_labeller(data_pnds=pnds.df, dictionary.file=dictionary.path)
#' pnds.df <- pnds_deflator(data_pnds=pnds.df, deflator.file=deflator.path)
#' \donttest{
#' pnds.svy <- pnds_design(data_pnds=pnds.df)
#' # Calculating proportion of people diagnosed with chronic diseases
#' if (!is.null(pnds.svy)) survey::svymean(x=~J007, design=pnds.svy, na.rm=TRUE)}
#' \donttest{
#' # Downloading data
#' pnds.df2 <- get_pnds(year=2023, section="Female", vars=c("J007","J009"),
#'                        labels=TRUE, deflator=TRUE, design=FALSE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' pnds.svy2 <- pnds_design(data_pnds=pnds.df2)
#' # Calculating proportion of people diagnosed with chronic diseases
#' if (!is.null(pnds.svy2)) survey::svymean(x=~J007, design=pnds.svy2, na.rm=TRUE)}
#' @export

pnds_design <- function(data_pnds) {
  message("The pnds_design function is under development and will be available soon in package PNDSIBGE.\n")
  return(NULL)
  if (sum(class(data_pnds) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("UPA_PNDS", "ID_DOMICILIO", "V0024", "V0028", "V00281", "V00282", "V00283") %in% names(data_pnds))) |
        !(FALSE %in% (c("UPA_PNDS", "ID_DOMICILIO", "V0024", "V0029", "V00291", "V00292", "V00293") %in% names(data_pnds)))) {
      options(survey.lonely.psu="adjust")
      options(survey.adjust.domain.lonely=TRUE)
      if (!(FALSE %in% (c("UPA_PNDS", "ID_DOMICILIO", "V0024", "V0028", "V00281", "V00282", "V00283") %in% names(data_pnds)))) {
        data_prior <- survey::svydesign(ids=~UPA_PNDS, strata=~V0024, data=data_pnds, weights=~V0028, nest=TRUE)
        popc.types <- data.frame(V00283=as.character(unique(data_pnds$V00283)), Freq=as.numeric(unique(data_pnds$V00282)))
        popc.types <- popc.types[order(popc.types$V00283),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00283, population=popc.types)
      }
      else {
        data_prior <- survey::svydesign(ids=~UPA_PNDS, strata=~V0024, data=data_pnds, weights=~V0029, nest=TRUE)
        popc.types <- data.frame(V00293=as.character(unique(data_pnds$V00293)), Freq=as.numeric(unique(data_pnds$V00292)))
        popc.types <- popc.types[order(popc.types$V00293),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00293, population=popc.types)
      }
    }
    else {
      message("Weight variables required for sample design are missing.\n")
      data_posterior <- data_pnds
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so applying another design is not possible.\n")
    data_posterior <- data_pnds
  }
  return(data_posterior)
}
