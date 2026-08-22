#' Create HTML fragment with CSS styling
#'
#' @param template The template to load; either the name of one
#' of the ROCK templates (currently, only 'default' is available), or
#' the path and filename of a CSS file.
#' @param includeBootstrap Whether to include the default bootstrap CSS.
#'
#' @return A character vector with the HTML fragment.
#'
#' @export
css <- function(template = "default",
                includeBootstrap = rock::opts$get("includeBootstrap")) {

  ### Load stylesheets
  bootstrapCSS <-
    paste0(readLines(system.file("css", "bootstrap.min.css", package="rock")),
           collapse="\n");
  basicCSS <-
    paste0(readLines(system.file("css", "basic.css", package="rock")),
           collapse="\n");

  if (file.exists(template)) {
    templateCSS <-
      paste0(readLines(template),
             collapse="\n");
  } else if (file.exists(system.file("css", paste0(template, ".css"), package="rock"))) {
    templateCSS <-
      paste0(readLines(system.file("css", "default.css", package="rock")),
             collapse="\n");
  } else {
    templateCSS <-
      paste0(readLines(system.file("css", "default.css", package="rock")),
             collapse="\n");
  }

  ### Merge stylesheets
  fullCSS <-
    paste0("\n<style>\n",
           ifelse(isTRUE(includeBootstrap),
                  bootstrapCSS,
                  ""),
           "\n\n",
           basicCSS,
           "\n\n",
           templateCSS,
           "\n</style>\n");

  return(fullCSS);

}
