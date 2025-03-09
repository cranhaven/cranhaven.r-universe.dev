#' Write a source to a file
#'
#' These functions write one or more source(s) from memory (as
#' loaded by [rock::load_source()] or [rock::load_sources()] to a file.
#'
#' @param x The source(s).
#' @param output The filename (for `rock::write_source()`) or path (for
#' `rock::write_sources()`) to write to.
#' @param encoding The encoding to use.
#' @param preventOverwriting Whether to prevent against overwriting of the
#' file(s) to write. Set to `FALSE` to overwrite.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the input (`x`), to enable chaining in pipes.
#' @rdname writing_sources
#' @aliases write_source write_sources writing_sources
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Get a temporary file to write to
#' tempFile <- tempfile(fileext = ".rock")
#'
#' ### Pipe chain to load the example source; add a code;
#' ### and write the result to disk
#' loadedSource <-
#'
#'   rock::load_source(exampleFile) |>
#'
#'   rock::code_source(c("Lorem Ipsum" = "lorumIpsum")) |>
#'
#'   rock::write_source(tempFile);
#'
#' @export
write_source <- function(x,
                         output,
                         encoding = rock::opts$get('encoding'),
                         preventOverwriting = rock::opts$get("preventOverwriting"),
                         silent = rock::opts$get('silent')) {

  if (!("rock_source" %in% class(x))) {
    stop("As `x`, you must pass a source as imported by {rock}. It should ",
         "have class `rock_source`, but the object you passed as `x` has ",
         "class(es) ", vecTxtQ(), ".");
  }

  writingResult <-
    writeTxtFile(
      x = x,
      output = output,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    );

  if (writingResult) {
    msg("I just wrote a source to file '",
        output,
        "'.",
        silent = silent);
  } else {
    warning("Could not write source to `",
            output, "`.");
  }

  return(invisible(x));

}
