#' getErrorTab skeleton of list of errors
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return HTML formatted error list
#'
#' @param errorLst list of errors and changes made by \code{qcStudbook}
#' @param pedigreeFileName name of file provided by user on Input tab
#' @export
getErrorTab <- function(errorLst, pedigreeFileName) {
  tabPanel(
    "Error List",
    # tags$style(
    #   type = "text/css",
    #   "table {border: 1px solid black; width: 100%; padding: 15px;}",
    #   "tr, td, th {border: 1px solid black; padding: 5px;}",
    #   "th {font-weight: bold; background-color: #7CFC00;}",
    #   "hr {border-width:2px;border-color:#A9A9A9;}"
    # ),
    # titlePanel(div(
    #   style = "height:125px;width:100%",
    #   div(style = "float:right;text-align:right;width:45%",
    #       "Errors")
    # )),
    div(HTML(insertErrorTab(errorLst, pedigreeFileName)))
  )
}
