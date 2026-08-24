#' Get all items in a public Zotero group
#'
#' @param group The group ID
#' @param format The format to export
#'
#' @return A character vector
#' @export
#'
#' @examples zotero_get_all_items(2425237);
zotero_get_all_items <- function(group,
                                 format = "bibtex") {
  tryCatch({
    nr_of_items <- zotero_nr_of_items(group);
    nr_of_calls <- ceiling(nr_of_items / 100);
    call_startValues <- 0 + ((1:nr_of_calls - 1) * 100);
    bibliography <- c();
    for (startValue in call_startValues) {
      bibCon <- url(zotero_construct_export_call(group));
      bibliography <- c(bibliography,
                        readLines(bibCon, warn=FALSE));
      close(bibCon);
    }
    return(bibliography);
  }, error=function(e) {
    cat("Error downloading the references from Zotero's API:\n\n", e$message);
  });
}
