#' Convert a (pre)registration specification to YAML or JSON
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]), or, for the print method, the produced
#' YAML or JSON.
#' @param includeFormSpec Whether to include the (pre)registration form
#' specification. Note that this includes metadata about the form fields
#' such as their labels and descriptions - without the form specification,
#' only the item identifiers are stored.
#' @param file Optionally, a file to save the YAML or JSON to.
#' @param ... Any additional arguments are ignored.
#'
#' @return If a file is specified to write, to, `x` will be returned invisibly
#' to allow building a pipe chain; if `file=NULL`, the resulting YAML/JSON
#' will be returned as a character vector.
#' @export
#' @rdname export_to_yaml_or_json
#'
#' @examples
#' ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Export to YAML
#' preregr::prereg_spec_to_yaml(
#'   examplePrereg_1
#' );
prereg_spec_to_yaml <- function(x,
                                includeFormSpec = TRUE,
                                file = NULL) {

  if (!(inherits(x, "preregr") && inherits(x, "preregr_spec"))) {
    stop("As `x`, you have to pass an initialized {preregr} object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

  xToWrite <- x;

  if (!includeFormSpec) {
    xToWrite <- xToWrite['specs'];
  }

  xToWrite <-
    structure_for_serialization(xToWrite);

  if (is.null(file)) {
    res <-
      yaml::as.yaml(
        xToWrite
      );
    class(res) <- c("preregr_yaml", class(res));
    return(
      res
    );
  } else {
    yaml::write_yaml(
      xToWrite,
      file
    );
    return(invisible(xToWrite));
  }
}

#' @rdname export_to_yaml_or_json
#' @export
#' @method print preregr_yaml
print.preregr_yaml <- function(x, ...) {
  cat(x, sep="\n");
  return(invisible(x));
}
