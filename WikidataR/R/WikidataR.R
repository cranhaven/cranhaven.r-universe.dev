#' @title API client library for Wikidata
#' @description This package serves as an API client for reading and writing
#' to and from \href{https://www.wikidata.org/wiki/Wikidata:Main_Page}{Wikidata}, (including 
#' via the \href{https://quickstatements.toolforge.org/}{QuickStatements} format),
#' as well as for reading from \href{https://www.wikipedia.org}{Wikipedia}.
#' @name WikidataR
#' @docType package
#' @seealso \code{\link{get_random}} for selecting a random item or property,
#' \code{\link{get_item}} for a /specific/ item or property, or \code{\link{find_item}}
#' for using search functionality to pull out item or property IDs where the descriptions
#' or aliases match a particular search term.
#' @import WikidataQueryServiceR
#' @import tibble
#' @import dplyr
#' @import stringr
#' @import pbapply
#' @import progress
#' @import crayon
#' @import utils
#' @importFrom readr format_tsv
#' @importFrom readr format_delim
#' @importFrom dplyr bind_cols
#' @importFrom stats var
#' @importFrom stats cor
#' @importFrom WikipediR page_content random_page query
#' @importFrom httr user_agent
#' @importFrom jsonlite fromJSON
#' @aliases WikidataR WikidataR-package
NULL