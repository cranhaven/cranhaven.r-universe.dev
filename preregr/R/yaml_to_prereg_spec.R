#' Convert a (pre)registration specification from YAML or JSON
#'
#' @param x The YAML or JSON as character vector, or a path to a file
#' containing the YAML or JSON.
#'
#' @return The imported object.
#' @export
#' @rdname import_yaml_or_json
#'
#' @examples ### Get path to example file
#' examplePreregFile <-
#'   system.file(
#'     "extdata",
#'     "preregr-spec-example1.yml",
#'     package = "preregr"
#'   );
#'
#' ### Load it and show which items are completed
#' preregr::yaml_to_prereg_spec(
#'   examplePreregFile
#' ) |>
#'   preregr::prereg_show_item_completion();
yaml_to_prereg_spec <- function(x) {

  if (!is.character(x)) {
    stop("As `x`, pass a character vector that either contains the ",
         "path to a file, or the imported YAML.");
  }

  ## Load YAML file or otherwise parse YAML character string

  if ((length(x) == 1) && (file.exists(x))) {
    res <-
      yaml::read_yaml(x);
  } else {
    res <-
      yaml::yaml.load(
        x
      );
  }

  res <- serialized_data_to_dfs(res);

  class(res) <- c("preregr", "preregr_spec", "list");

  return(res);

}
