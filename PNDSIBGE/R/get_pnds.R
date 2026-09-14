#' Download, label, deflate and create survey design object for PNDS microdata
#' @description Core function of package. With this function only, the user can download a PNDS microdata from a year and get a sample design object ready to use with \code{survey} package functions.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param year The year of the data to be downloaded. Must be a number equal to 2023. Vector not accepted.
#' @param section Argument corresponding to which section of the questionnaire will be obtained, being able to receive only the values of "Female" or "Male", the writing of the value must be identical to the indicated value. Default is to use the "Female" section of the questionnaire.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variables will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design} or \code{svyrep.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param reload Logical value. If \code{TRUE}, will re-download the files even if they already exist in the save directory. If \code{FALSE}, will be checked if the files already exist in the save directory and the download will not be performed repeatedly, be careful with coinciding names of microdata files.
#' @param curlopts A named list object identifying the curl options for the handle when using functions from \code{RCurl} package.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from PNDS and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-demografia-e-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNDSIBGE]{read_pnds} for reading PNDS microdata.\cr \link[PNDSIBGE]{pnds_labeller} for labeling categorical variables from PNDS microdata.\cr \link[PNDSIBGE]{pnds_deflator} for adding deflator variables to PNDS microdata.\cr \link[PNDSIBGE]{pnds_design} for creating PNDS survey design object.\cr \link[PNDSIBGE]{pnds_example} for getting the path of the PNDS toy example files.
#' @examples
#' \donttest{
#' pnds.svy <- get_pnds(year=2023, section="Female", vars=c("J007","J009"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' # Calculating proportion of people diagnosed with chronic diseases
#' if (!is.null(pnds.svy)) survey::svymean(x=~J007, design=pnds.svy, na.rm=TRUE)
#' pnds.svy2 <- get_pnds(year=2023, section="Male", vars=c("N001","N00101"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' # Calculating proportion of people's self-rated health
#' if (!is.null(pnds.svy2)) survey::svymean(x=~N001, design=pnds.svy2, na.rm=TRUE)}
#' @export

get_pnds <- function(year, section = "Female", vars = NULL,
                     labels = TRUE, deflator = TRUE, design = TRUE, reload = TRUE, curlopts = list(), savedir = tempdir())
{
  message("The get_pnds function is under development and will be available soon in package PNDSIBGE.\n")
  return(NULL)
  if (year != 2023) {
    message("Year must be equal to 2023.\n")
    return(NULL)
  }
  if (section != "Female" & section != "Male") {
    message("Section argument must be given 'Female' or 'Male' value.\n")
    return(NULL)
  }
  if (!(labels %in% c(TRUE, FALSE))) {
    labels <- TRUE
    message("Invalid value provided for labels argument, so default value TRUE was set to this argument.\n")
  }
  if (!(deflator %in% c(TRUE, FALSE))) {
    deflator <- TRUE
    message("Invalid value provided for deflator argument, so default value TRUE was set to this argument.\n")
  }
  if (!(design %in% c(TRUE, FALSE))) {
    design <- TRUE
    message("Invalid value provided for design argument, so default value TRUE was set to this argument.\n")
  }
  if (!(reload %in% c(TRUE, FALSE))) {
    reload <- TRUE
    message("Invalid value provided for reload argument, so default value TRUE was set to this argument.\n")
  }
  if (!is.list(curlopts)) {
    curlopts <- list()
    message("Invalid value provided for curlopts argument, as the value of this argument needs to be a list, so the value provided will be ignored.\n")
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    message(paste0("The directory provided does not exist, so the directory was set to '", savedir), "'.\n")
  }
  if (savedir != tempdir()) {
    printpath <- TRUE
  }
  else {
    printpath <- FALSE
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  ftpdir <- paste0("https://ftp.ibge.gov.br/PNDS/", year, "/Microdados/")
  if (!projmgr::check_internet()) {
    message("The internet connection is unavailable.\n")
    return(NULL)
  }
  if (httr::http_error(httr::GET(ftpdir, httr::timeout(60)))) {
    message("The microdata server is unavailable.\n")
    return(NULL)
  }
  restime <- getOption("timeout")
  on.exit(options(timeout=restime))
  options(timeout=max(600, restime))
  ftpdata <- paste0(ftpdir, "Dados/")
  datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE, .opts=curlopts)), "\n")), "<a href=[[:punct:]]")), ".zip"))
  dataname <- datayear[which(startsWith(datayear, paste0("PNDS_", year)))]
  if (length(dataname) == 0) {
    message("Data unavailable for selected year.\n")
    return(NULL)
  }
  else if (length(dataname) > 1) {
    message("There is more than one file available for the requested microdata, please contact the package maintainer.\n")
    return(NULL)
  }
  else {
    dataname <- paste0(dataname, ".zip")
  }
  if (reload == FALSE & file.exists(paste0(savedir, "/", dataname))) {
    message("The reload argument was defined as FALSE and the file of microdata was already downloaded, so the download process will not execute again.\n")
  }
  else {
    utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
    if (suppressWarnings(class(try(utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir), silent=TRUE)) == "try-error")) {
      message("The directory defined to save the downloaded data is denied permission to overwrite the existing files, please clear or change this directory.\n")
      return(NULL)
    }
    if (reload == FALSE) {
      message("The definition of FALSE for the reload argument will be ignored, since the file of microdata was not downloaded yet.\n")
    }
  }
  utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
  docfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Documentacao/"), dirlistonly=TRUE, .opts=curlopts)), "\n")), "<a href=[[:punct:]]")), ".zip"))
  inputzip <- paste0(docfiles[which(startsWith(docfiles, "Dicionario_e_input"))], ".zip")
  if (reload == FALSE & file.exists(paste0(savedir, "/Dicionario_e_input.zip"))) {
    message("The reload argument was defined as FALSE and the file of dictionary and input was already downloaded, so the download process will not execute again.\n")
  }
  else {
    utils::download.file(url=paste0(ftpdir, "Documentacao/", inputzip), destfile=paste0(savedir, "/Dicionario_e_input.zip"), mode="wb")
    if (reload == FALSE) {
      message("The definition of FALSE for the reload argument will be ignored, since the file of dictionary and input was not downloaded yet.\n")
    }
  }
  utils::unzip(zipfile=paste0(savedir, "/Dicionario_e_input.zip"), exdir=savedir)
  microdataname <- dir(savedir, pattern=paste0("^PNDS_", year, ".*\\.txt$"), ignore.case=FALSE)
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$mtime),])[length(microdatafile)]
  inputname <- dir(savedir, pattern=paste0("^input_PNDS_", year, ".*\\.txt$"), ignore.case=FALSE)
  inputfile <- paste0(savedir, "/", inputname)
  inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$mtime),])[length(inputfile)]
  data_pnds <- PNDSIBGE::read_pnds(microdata=microdatafile, input_txt=inputfile, vars=vars)
  if (section == "Male" & c("W001") %in% names(data_pnds)) {
    data_pnds <- data_pnds[(data_pnds$W001 == "1" & !is.na(data_pnds$W001)),]
    data_pnds <- data_pnds[,!(names(data_pnds) %in% c("V0028", "V00281", "V00282", "V00283"))]
  }
  else if (section == "Female" & c("M001") %in% names(data_pnds)) {
    data_pnds <- data_pnds[(data_pnds$M001 == "1" & !is.na(data_pnds$M001)),]
    data_pnds <- data_pnds[,!(names(data_pnds) %in% c("V0029", "V00291", "V00292", "V00293"))]
  }
  else {
    message("An error occurred in the process of obtaining these specific microdata, check the arguments values provided.\n")
    return(NULL)
  }
  if (labels == TRUE) {
    if (exists("pnds_labeller", where="package:PNDSIBGE", mode="function")) {
      dicname <- dir(savedir, pattern=paste0("^dicionario_PNDS_microdados_", year, ".*\\.xls$"), ignore.case=FALSE)
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$mtime),])[length(dicfile)]
      data_pnds <- PNDSIBGE::pnds_labeller(data_pnds=data_pnds, dictionary.file=dicfile)
    }
    else {
      message("Labeller function is unavailable in package PNDSIBGE.\n")
    }
  }
  if (deflator == TRUE) {
    if (exists("pnds_deflator", where="package:PNDSIBGE", mode="function")) {
      ftpdef <- ("https://ftp.ibge.gov.br/PNDS/Documentacao_Geral/")
      deffiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdef, dirlistonly=TRUE, .opts=curlopts)), "\n")), "<a href=[[:punct:]]")), ".zip"))
      defzip <- paste0(deffiles[which(startsWith(deffiles, "Deflatores"))], ".zip")
      if (reload == FALSE & file.exists(paste0(savedir, "/Deflatores.zip"))) {
        message("The reload argument was defined as FALSE and the file of deflator was already downloaded, so the download process will not execute again.\n")
      }
      else {
        utils::download.file(url=paste0(ftpdef, defzip), destfile=paste0(savedir, "/Deflatores.zip"), mode="wb")
        if (reload == FALSE) {
          message("The definition of FALSE for the reload argument will be ignored, since the file of deflator was not downloaded yet.\n")
        }
      }
      utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
      defname <- dir(savedir, pattern=paste0("^deflator_PNDS.*\\.xls$"), ignore.case=FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$mtime),])[length(deffile)]
      data_pnds <- PNDSIBGE::pnds_deflator(data_pnds=data_pnds, deflator.file=deffile)
    }
    else {
      message("Deflator function is unavailable in package PNDSIBGE.\n")
    }
  }
  if (design == TRUE) {
    if (exists("pnds_design", where="package:PNDSIBGE", mode="function")) {
      data_pnds <- PNDSIBGE::pnds_design(data_pnds=data_pnds)
    }
    else {
      message("Sample design function is unavailable in package PNDSIBGE.\n")
    }
  }
  if (printpath == TRUE) {
    message("Paths of files downloaded in this function at the save directory provided are:")
    message(paste0(list.files(path=savedir, pattern="PNDS", full.names=TRUE), collapse="\n"), "\n")
  }
  return(data_pnds)
}
