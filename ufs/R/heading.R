#' Print a heading
#'
#' This is just a convenience function to print a markdown or HTML heading at
#' a given 'depth'.
#'
#' @param ... The heading text: pasted together with no separator.
#' @param headingLevel The level of the heading; the default can be set
#' with e.g. `ufs::opts$set(defaultHeadingLevel=1)`.
#' @param output Whether to output to HTML ("`html`") or markdown (anything
#' else).
#' @param cat Whether to cat (print) the heading or just invisibly return it.
#'
#' @return The heading, invisibly.
#' @export
#'
#' @examples heading("Hello ", "World", headingLevel=5);
#' ### This produces: "\\n\\n##### Hello World\\n\\n"
heading <- function(...,
                    headingLevel = ufs::opts$get("defaultHeadingLevel"),
                    output = "markdown",
                    cat = TRUE) {
  text <- paste0(..., collapse="");
  if (output == "html") {
    res <- paste0(
      "\n\n<h", headingLevel, "> ",
      text, "</h", headingLevel, ">\n\n"
    );
  } else {
    res <- paste0(
      "\n\n",
      repStr("#", headingLevel), " ",
      text, "\n\n"
    );
  }
  if (cat) {
    cat(res);
  }
  return(invisible(res));
}
