#' Download and save all items in a public Zotero group
#'
#' @param group The group ID
#' @param file The filename to write to
#' @param format The format to export
#' @param showKeys Whether to show the keys
#'
#' @return The bibliography as a character vector
#' @export
#'
#' @examples \dontrun{
#' tmpFile <- tempfile(fileext=".bib");
#' zotero_download_and_export_items(
#'   2425237,
#'   tmpFile
#' );
#' writtenBibliography <- readLines(tmpFile);
#' writtenBibliography[1:7];
#' }
zotero_download_and_export_items <- function(group,
                                             file,
                                             format = "bibtex",
                                             showKeys = TRUE) {
  bibliography <-
    zotero_get_all_items(group = group,
                         format = format);

  if (dir.exists(dirname(file))) {
    writeLines(bibliography, file);
  } else {
    warning(
      paste0(
        "You specified filename `",
        file,
        "` to write to, but the directory in that path does not exist."
      )
    )
  }

  if (showKeys) {
    print(gsub("^@[a-zA-Z0-9]+\\{(.*),",
               "\\1",
               bibliography)[grep("^@[a-zA-Z0-9]+\\{(.*),", bibliography)]);
  }

  return(invisible(bibliography));

}
