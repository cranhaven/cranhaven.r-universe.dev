#' Import a (pre)registration specification from JSON embedded in HTML
#'
#' @param x The HTML as URL, path to a file, or HTML that has already
#' been imported to a character vector.
#' @param select If multiple `preregr` specifications are present in `x`,
#' the `select` argument can be used to select which one to import. `select`
#' is a number corresponding to the order of the encountered `preregr`
#' specifications (e.g., pass `2` to import the second specification, etc).
#'
#' @return The (pre)registration specification.
#' @export
#'
#' @examples \donttest{
#' ### Note that this example writes to a local file!
#'
#' ### Temporary file to save to
#' tmpFile <- tempfile(fileext = ".html");
#'
#' ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Save it to an HTML file
#' preregr::prereg_spec_to_html(
#'   examplePrereg_1,
#'   file = tmpFile
#' );
#'
#' ### Import the example again
#' importedPreregr <-
#'   preregr::import_from_html(
#'     tmpFile
#'   );
#'
#' ### Show the result
#' preregr::prereg_show_item_completion(
#'   importedPreregr,
#'   section="metadata"
#' );
#' }
import_from_html <- function(x,
                             select = 1) {

  if (!requireNamespace('rvest', quietly=TRUE)) {
    stop("You need to have 'rvest' installed to work with HTML!");
  }

  if (!requireNamespace('jsonlite', quietly=TRUE)) {
    stop("You need to have 'jsonlite' installed to convert from JSON!");
  }

  html <- rvest::read_html(x = x);

  preregrElement <- rvest::html_elements(
    html,
    css = ".preregr-json"
  );

  if (length(preregrElement) == 0) {
    stop("No embedded JSON code with a preregr (pre)registration form or ",
         "specification present in `x` (", substitute(deparse(x)), ").");
  } else if (length(preregrElement) == 1) {
    preregrElement <- preregrElement[[1]];
  } else {
    preregrElement <- preregrElement[[select]];
  }

  json <- rvest::html_attr(
    preregrElement,
    "data-preregr"
  );

  res <- jsonlite::fromJSON(json);

  res <-
    serialized_data_to_dfs(
      res
    );

  res$form$metadata$content[res$form$metadata$field == "date"] <-
    format(
      number_as_xl_date(
        res$form$metadata$content[res$form$metadata$field == "date"]
      ),
      "%Y-%m-%d"
    );

  if ("specs" %in% names(res)) {

    ### Set the correct justifier classes
    res$jstf <-
      set_justifier_classes_to_structured_object(
        res$jstf
      );

    class(res) <- c("preregr", "preregr_spec", "list");

    return(res);

  } else {

    return(res$form);

  }

}

reorderList <- function(x, fifthElement) {
  elementNames <- names(x);
  correctOrder <- c("id", "label", "description", "type", fifthElement);
  correctOrder <- c(correctOrder, setdiff(correctOrder, elementNames));
  correctOrder <- union(correctOrder, elementNames);
  print(correctOrder);
  return(x[correctOrder]);
}

