#' Import a (pre)registration form specification from a spreadsheet
#'
#' With this function, you can import a (pre) registration from a
#' spreadsheet. See the "Creating a form from a spreadsheet" vignette
#' for more information. That is available at
#' <https://preregr.opens.science/articles/creating_form_from_spreadsheet.html>
#' or can be shown by running \code{vignette("creating_form_from_spreadsheet",
#' package = "preregr")}
#'
#' An empty simple
#' example spreadsheet is available at
#' <https://docs.google.com/spreadsheets/d/14Qbak7JbBhTqmJaMgJ4tU9ZROaBbUfq37_UzkoHnM60>
#' and can be initialized as the `almostEmptyForm` form
#' with [preregr::prereg_initialize()]
#'
#' @inheritParams read_spreadsheet
#'
#' @return The preregr form specification
#'
#' @export
form_fromSpreadsheet <- function(x,
                                 localBackup = NULL,
                                 exportGoogleSheet = TRUE,
                                 xlsxPkg = c("rw_xl", "openxlsx", "XLConnect"),
                                 silent = preregr::opts$get("silent")) {

  res <-
    preregr::read_spreadsheet(
      x = x,
      localBackup = localBackup,
      exportGoogleSheet = exportGoogleSheet,
      xlsxPkg = xlsxPkg,
      silent = silent
    );

  res$items$item_id <- sanitize_identifiers(res$items$item_id);
  res$items$section_id <- sanitize_identifiers(res$items$section_id);
  res$sections$section_id <- sanitize_identifiers(res$sections$section_id);
  res$items$item_valueTemplate <- sanitize_identifiers(res$items$item_valueTemplate);
  res$valueTemplates$identifier <- sanitize_identifiers(res$valueTemplates$identifier);

  verify_identifier_uniqueness(res$items$item_id,
                               dfName="items", colName="item_id");
  verify_identifier_uniqueness(res$sections$section_id,
                               dfName="sections", colName="section_id");
  verify_identifier_uniqueness(res$valueTemplates$identifier,
                               dfName="valueTemplates", colName="identifier");
  verify_identifier_uniqueness(res$metadata$field,
                               dfName="metadata", colName="field");
  verify_identifier_uniqueness(res$instructions$heading,
                               dfName="instructions", colName="heading");

  if (any(is.na(res$items$item_valueTemplate))) {
    warning("Not all items have a specified value template!");
    res$items$item_valueTemplate[is.na(res$items$item_valueTemplate)] <-
      "string";
  }

  res$metadata$content[res$metadata$field == "date"] <-
    format(
      number_as_xl_date(
        res$metadata$content[res$metadata$field == "date"]
      ),
      "%Y-%m-%d"
    );

  class(res) <- c("preregr", "preregr_form", "list");

  return(res);

}
