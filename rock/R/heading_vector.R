#' Make a vector of strings into headings
#'
#' This is just a convenience function to convert a vector of strings into
#' markdown or HTML headings at a given 'depth'.
#'
#' @param x The vector.
#' @param headingLevel The level of the heading; the default can be set
#' with e.g. `rock::opts$set(defaultHeadingLevel=1)`.
#' @param output Whether to output to HTML ("`html`") or markdown (anything
#' else).
#' @param cat Whether to cat (print) the heading or just invisibly return it.
#'
#' @return The heading, invisibly.
#' @export
#'
#' @examples rock::heading_vector(c("Hello ", "World"), headingLevel=5);
#' ### This produces: "\\n\\n##### Hello\\n\\n" and
#' ### "\\n\\n##### World\\n\\n"
heading_vector <- function(x,
                           headingLevel = rock::opts$get("defaultHeadingLevel"),
                           output = "markdown",
                           cat = FALSE) {
  if (output == "html") {
    res <- paste0(
      "\n\n<h", headingLevel, "> ",
      x, "</h", headingLevel, ">\n\n"
    );
  } else {
    res <- paste0(
      "\n\n",
      repStr("#", headingLevel), " ",
      x, "\n\n"
    );
  }
  if (cat) {
    cat(res);
  }
  return(invisible(res));
}
