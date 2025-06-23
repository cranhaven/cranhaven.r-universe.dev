#' @docType package
#'
#' @name opendataformat-package
#'
#' @aliases opendataformat
#'
#' @title Open Data Format
#'
#' @description
#' The package is designed to support the use of the open data format.
#' For this purpose, three main functions have been developed:
#'
#' ## read_odf()
#' Import data from the Open Data Format to an R data frame.
#'
#' ## write_odf()
#' Export data from an R data frame to the open data format.
#'
#' ## docu_odf()
#' Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#' 
#' ## setlanguage_odf()
#' Set the default language for displaying the metadata for docu_odf 
#' and getmetadata_odf
#'
#' ## getmetadata_odf()
#' Retrieve specific metadata like variable labels, or value labels.
#'
#' ## as_odf_tbl()
#' Convert data frame (data.frame object or any subclass) to an ODF 
#' tibble (odf_tbl class object).
#' 
#' @seealso
#' More information about the Open Data Format specification and
#' data examples are available here:
#' [https://opendataformat.github.io/](https://opendataformat.github.io/)
#'
#' @author
#'
#' Tom Hartl (\email{thartl@diw.de}),
#' Claudia Saalbach (\email{csaalbach@diw.de})
#'
#' Other Contributors: KonsortSWD/NFDI, DIW Berlin
#'
"_PACKAGE"
