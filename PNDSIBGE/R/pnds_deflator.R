#' Add deflator variables to PNDS microdata
#' @description This function adds deflator variables to PNDS microdata. For deflation of income variables, the documentation provided through the following address must be used: \samp{https://ftp.ibge.gov.br/PNDS/Documentacao_Geral/PNDSIBGE_Deflator.pdf}.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pnds A tibble of PNDS microdata read with \code{read_pnds} function.
#' @param deflator.file The deflator file for selected survey available on official website: (select the deflator zip file) - \samp{https://ftp.ibge.gov.br/PNDS/Documentacao_Geral/}.
#' @return A tibble with the data provided from PNDS survey and the deflator variables added for use.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-demografia-e-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNDSIBGE]{get_pnds} for downloading, labeling, deflating and creating survey design object for PNDS microdata.\cr \link[PNDSIBGE]{read_pnds} for reading PNDS microdata.\cr \link[PNDSIBGE]{pnds_labeller} for labeling categorical variables from PNDS microdata.\cr \link[PNDSIBGE]{pnds_design} for creating PNDS survey design object.\cr \link[PNDSIBGE]{pnds_example} for getting the path of the PNDS toy example files.
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
#' # Downloading data
#' pnds.df2 <- get_pnds(year=2023, section="Female", vars=c("J007","J009"),
#'                        labels=TRUE, deflator=FALSE, design=FALSE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' deflator.path2 <- pnds_example(path="deflatorexample.xls")
#' pnds.df2 <- pnds_deflator(data_pnds=pnds.df2, deflator.file=deflator.path2)}
#' @export

pnds_deflator <- function(data_pnds, deflator.file) {
  message("The pnds_deflator function is under development and will be available soon in package PNDSIBGE.\n")
  return(NULL)
  if (sum(class(data_pnds) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("V0020", "V0001") %in% names(data_pnds)))) {
      data_pnds <- data_pnds[, !names(data_pnds) %in% c("Deflator"), drop=FALSE]
      deflator <- suppressMessages(readxl::read_excel(deflator.file))
      colnames(deflator)[c(1:2)] <- c("V0020", "V0001")
      deflator$V0001 <- as.factor(deflator$V0001)
      if (identical(intersect(levels(deflator$V0001), levels(as.factor(data_pnds$V0001))), character(0)) & length(levels(deflator$V0001)) == length(levels(as.factor(data_pnds$V0001)))) {
        levels(deflator$V0001) <- levels(as.factor(data_pnds$V0001))
      }
      data_pnds <- merge(x=data_pnds, y=deflator, by.x=c("V0020", "V0001"), by.y=c("V0020", "V0001"), all.x=TRUE, all.y=FALSE)
      if (!(FALSE %in% (c("ID_DOMICILIO") %in% names(data_pnds)))) {
        data_pnds <- data_pnds[order(data_pnds$V0024, data_pnds$ID_DOMICILIO, data_pnds$C00301),]
      }
      else {
        data_pnds <- data_pnds[order(data_pnds$V0024, data_pnds$UPA_PNDS, data_pnds$V0006_PNDS, data_pnds$C00301),]
      }
      data_pnds <- tibble::as_tibble(data_pnds)
    }
    else {
      message("Merge variables required for adding deflator variables are missing.\n")
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so adding deflator variables is not possible.\n")
  }
  return(data_pnds)
}
