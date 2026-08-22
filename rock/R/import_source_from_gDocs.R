#' Import a source from Google Documents
#'
#' @param x The URL to the source: has to be viewable publicly!
#' @param localFile A local file (where to store a local backup).
#'
#' @return The source contents.
#' @export
#'
#' @examples
#' ### Note that this will require an active
#' ### internet connection! This if statement
#' ### checks for that.
#' \donttest{
#' if (tryCatch({readLines("https://google.com",n=1); TRUE}, error=function(x) FALSE)) {
#'
#'   gDocs_url <-
#'     paste0(
#'       "https://docs.google.com/document/d/",
#'       "1iACYjV7DdCjOmfgX6KEMtCcCjuuXD3iuikTSGWtsK84",
#'       "/edit?usp=sharing"
#'     );
#'
#'   ### Import the source
#'   exampleSource <-
#'     import_source_from_gDocs(
#'       gDocs_url
#'     );
#'
#'   ### Show the downloaded file:
#'   exampleSource;
#'
#'   ### Parse the source:
#'   parsedExampleSource <-
#'     rock::parse_source(
#'       exampleSource
#'     );
#'
#'   ### Imported; the comments are gone:
#'   parsedExampleSource$qdt$utterances_raw;
#' }
#' }
import_source_from_gDocs <- function(x,
                                     localFile = NULL) {

  gDocsId_extractionRegex <- rock::opts$get("gDocsId_extractionRegex");
  gDocsId_to_exportLink <- rock::opts$get("gDocsId_to_exportLink");


  if (grepl(gDocsId_extractionRegex, x)) {

    gDocs_id <-
      gsub(
        rock::opts$get("gDocsId_extractionRegex"),
        "\\1",
        x
      );

  } else {

    gDocs_id <- x;

  }

  gDocId_downloadLink <- sprintf(gDocsId_to_exportLink, gDocs_id);

  if (is.null(localFile)) {
    res <-
      readLines(
        gDocId_downloadLink,
        warn = FALSE
      );
  } else {
    downloadResult <-
      utils::download.file(url = gDocId_downloadLink,
                           destfile = localFile,
                           quiet = TRUE, mode = "wb");
    if (downloadResult != 0) {
      warning(
        "Could not download the file you specified; ",
        "are you connected to the internet?\n",
        "Attempting to read local file, if specified."
      );
    }
   if (file.exists(localFile)) {
      res <- readLines(localFile, warn=FALSE);
    }
  }

  return(res);

}
