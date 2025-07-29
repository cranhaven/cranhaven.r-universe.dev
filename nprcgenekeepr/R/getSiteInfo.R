#' Get site information
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @return{A list of site specific information used by the application.}
#'
#' Currently this returns the following character strings in a named list.
#' \enumerate{
#'   \item\{center\}\{One of "SNPRC" or "ONPRC"\}
#'   \item\{baseUrl\}\{If \code{center} is "SNPRC", baseUrl is one of
#'   "https://boomer.txbiomed.local:8080/labkey" or
#'   "https://vger.txbiomed.local:8080/labkey".
#'   To allow testing, if \code{center} is "ONPRC" baseUrl is
#'   "https://boomer.txbiomed.local:8080/labkey".\}
#'   \item\{schemaName\}\{If \code{center} is "SNPRC", schemaName is "study".
#'   If \code{center} is "ONPRC", schemaName is "study"\}
#'   \item\{folderPath\} \{If \code{center} is "SNPRC", folderPath is "/SNPRC".
#'   If \code{center} is "ONPRC", folderPath is "/ONPRC"\}
#'   \item\{queryName\} \{is "demographics"\}
#' }
#'
#' @param expectConfigFile logical parameter when set to \code{FALSE}, no
#' configuration is looked for. Default value is \code{TRUE}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ## default sends warning if configuration file is missing
#' suppressWarnings(getSiteInfo())
#' getSiteInfo(expectConfigFile = FALSE)
getSiteInfo <- function(expectConfigFile = TRUE) {
  sysInfo <- Sys.info()
  config <- getConfigFileName(sysInfo)

  if (file.exists(config[["configFile"]])) {
    lines <- readLines(config[["configFile"]], skipNul = TRUE)
    tokenList <- getTokenList(lines)
    list(
      center = getParamDef(tokenList, "center"),
      baseUrl = getParamDef(tokenList, "baseUrl"),
      schemaName = getParamDef(tokenList, "schemaName"),
      folderPath = getParamDef(tokenList, "folderPath"),
      queryName = getParamDef(tokenList, "queryName"),
      lkPedColumns = getParamDef(tokenList, "lkPedColumns"),
      mapPedColumns = getParamDef(tokenList, "mapPedColumns"),
      sysname = sysInfo[["sysname"]],
      release = sysInfo[["release"]],
      version = sysInfo[["version"]],
      nodename = sysInfo[["nodename"]],
      machine = sysInfo[["machine"]],
      login = sysInfo[["login"]],
      user = sysInfo[["user"]],
      effective_user = sysInfo[["effective_user"]],
      homeDir = config[["homeDir"]],
      configFile = config[["configFile"]]
    )
  } else {
    if (expectConfigFile) {
      warning(
        "The nprcgenekeepr configuration file is missing.\n",
        "It is required when the LabKey API is to be used.\n",
        "The file should be named: ",
        config[["configFile"]], ".\n"
      )
    }
    list(
      center = "ONPRC",
      baseUrl = "https://primeuat.ohsu.edu",
      schemaName = "study",
      folderPath = "/ONPRC/EHR", # nolint: nonportable_path_linter
      queryName = "demographics",
      lkPedColumns = c(
        "Id", "gender", "birth", "death", "lastDayAtCenter",
        "Id/parents/dam", "Id/parents/sire" # nolint: nonportable_path_linter
      ),
      mapPedColumns = c("id", "sex", "birth", "death", "exit", "dam", "sire"),
      sysname = sysInfo[["sysname"]],
      release = sysInfo[["release"]],
      version = sysInfo[["version"]],
      nodename = sysInfo[["nodename"]],
      machine = sysInfo[["machine"]],
      login = sysInfo[["login"]],
      user = sysInfo[["user"]],
      effective_user = sysInfo[["effective_user"]],
      homeDir = config[["homeDir"]],
      configFile = config[["configFile"]]
    )
  }
}
