#' Import a code book specification from a spreadsheet
#'
#' @inheritParams read_spreadsheet
#'
#' @return The code book specification as a `rock` code book object
#' @export
#' @examples ### Note that this will require an active
#' ### internet connection! This if statement
#' ### checks for that.
#' \donttest{
#' if (tryCatch({readLines("https://google.com",n=1); TRUE}, error=function(x) FALSE)) {
#'
#'   ### Read the example ROCK codebook
#'   gs_url <- paste0(
#'     "https://docs.google.com/spreadsheets/d/",
#'     "1gVx5uhYzqcTH6Jq7AYmsLvHSBaYaT-23c7ZhZF4jmps"
#'   );
#'   ROCK_codebook <- rock::codebook_fromSpreadsheet(gs_url);
#'
#'   ### Show a bit
#'   ROCK_codebook$metadata[1:3, ];
#' }
#' }
codebook_fromSpreadsheet <- function(x,
                                     localBackup = NULL,
                                     exportGoogleSheet = TRUE,
                                     xlsxPkg = c("rw_xl", "openxlsx", "XLConnect"),
                                     silent = rock::opts$get("silent")) {

  res <-
    rock::read_spreadsheet(
      x = x,
      localBackup = localBackup,
      exportGoogleSheet = exportGoogleSheet,
      xlsxPkg = xlsxPkg,
      silent = silent
    );

  res$codes$code_id <- sanitize_identifiers(res$codes$code_id);

  res$metadata$content[res$metadata$field == "date"] <-
    format(
      number_as_xl_date(
        res$metadata$content[res$metadata$field == "date"]
      ),
      "%Y-%m-%d"
    );

  class(res) <- c("rock", "rock_codebook_spec", "list");

  return(res);

}
