#' @title choose_database
#'
#' @description It allows the user to choose the databases on which to perform the enrichment analysis. Either it returns all the possible databases or a subset of them.
#' @param db_search (Default = NULL), is the string pattern to be matched against the list of enrichR databases. Any matching DBs will be returned.
#' @return List of database names as a character vector.
#' @examples
#' \dontrun{
#' choose_database(db_search = "KEGG")
#' }
#' @export


choose_database <- function(db_search = NULL) {
  dbs_table <- listEnrichrDbs()
  dbs <- dbs_table$libraryName

  if (!is.null(db_search)) {
    index <- grepl(db_search, dbs)
    correlated_dbs_list <- dbs[index]
    return(correlated_dbs_list)
  } else {
    return(dbs)
  }
}
