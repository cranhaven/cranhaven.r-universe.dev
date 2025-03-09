#' Show all coded fragments
#'
#' @param root The root code
#'
#' @inheritParams collect_coded_fragments
#'
#' @return Invisibly, the coded fragments in a character vector.
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(
#'     examplePath, "example-1.rock"
#'   );
#'
#' ### Parse single example source
#' parsedExample <-
#'   rock::parse_source(
#'     exampleFile
#'   );
#'
#' ### Show organised coded fragments in Markdown
#' cat(
#'   rock::resultsOverview_allCodedFragments(
#'     parsedExample
#'   )
#' );
#'
resultsOverview_allCodedFragments <- function(x,
                                              root = "codes",
                                              context = 0,
                                              heading = NULL,
                                              headingLevel = 2,
                                              add_html_tags = TRUE,
                                              cleanUtterances = FALSE,
                                              output = NULL,
                                              outputViewer = "viewer",
                                              template = "default",
                                              includeCSS = TRUE,
                                              includeBootstrap = rock::opts$get("includeBootstrap"),
                                              preventOverwriting = rock::opts$get(preventOverwriting),
                                              silent=rock::opts$get(silent)) {

  if (!is.null(heading)) {
    res <-
      rock::heading(
        heading,
        headingLevel = headingLevel,
        cat = FALSE
      );
    headingLevel <- headingLevel + 1;
  } else {
    res <- character();
  }

  res <- c(res,
           collect_coded_fragments_recursively(
             x = x,
             root = root,
             context = context,
             omitHeading = TRUE,
             headingLevel = headingLevel,
             add_html_tags = add_html_tags,
             cleanUtterances = cleanUtterances,
             output = NULL,
             outputViewer = FALSE,
             template = template,
             rawResult = FALSE,
             includeCSS = FALSE,
             includeBootstrap = FALSE,
             silent = silent
           ));


  ### These bits are taken from collect_coded_fragments


  if (interactive() && ("viewer" %in% outputViewer)) {
    if ((!requireNamespace("rstudioapi", quietly = TRUE)) &&
        (rstudioapi::isAvailable())) {
      viewer <- rstudioapi::viewer
    }
    else {
      viewer <- getOption("viewer", utils::browseURL)
    }
    outputToViewer <- TRUE
  } else {
    outputToViewer <- FALSE
  }



  if (is.null(output)) {
    if (isTRUE(getOption('knitr.in.progress'))) {

      ###-----------------------------------------------------------------------
      ### Adding the CSS is missing, isn't that wrong?
      ###-----------------------------------------------------------------------

      if (add_html_tags && includeCSS) {
        res <-
          c(rock::css(template=template,
                      includeBootstrap = ifelse(is.character(includeBootstrap),
                                                TRUE,
                                                includeBootstrap)),
            res);
      }

      res <-
        knitr::asis_output(c("\n\n",
                             res,
                             "\n\n"));

      return(res);

    } else {

      if (outputToViewer) {
        viewerHTML <- markdown::markdownToHTML(text=res);

        if (add_html_tags && includeCSS) {
          viewerHTML <- htmltools::HTML(
            rock::css(template=template,
                      includeBootstrap = ifelse(is.character(includeBootstrap),
                                                TRUE,
                                                includeBootstrap)),
            viewerHTML
          );
        } else {
          viewerHTML <- htmltools::HTML(viewerHTML);
        }
        htmltools::html_print(htmltools::HTML(viewerHTML),
                              background = "white",
                              viewer = viewer)
      }
      if ("console" %in% outputViewer) {
        cat(res)
      }
      return(invisible(res));
    }
  } else {

    if (outputToViewer) {
      viewerHTML <- markdown::markdownToHTML(text=res);
      if (add_html_tags && includeCSS) {
        viewerHTML <- htmltools::HTML(
          rock::css(template=template,
                    includeBootstrap = ifelse(is.character(includeBootstrap),
                                              TRUE,
                                              includeBootstrap)),
          viewerHTML
        );
      } else {
        viewerHTML <- htmltools::HTML(viewerHTML);
      }
      htmltools::html_print(htmltools::HTML(viewerHTML),
                            background = "white",
                            viewer = viewer)
    }
    if ("console" %in% outputViewer) {
      cat(res)
    }

    if (dir.exists(dirname(output))) {
      if (file.exists(output) | preventOverwriting) {
        writeLines(res,
                   con = con <- file(output,
                                     "w",
                                     encoding="UTF-8"));
        close(con);
        if (!silent) {
          cat0("Wrote output file '", output,
               "' to disk.");
        }
      } else {
        if (!silent) {
          cat0("Specified output file '", output,
               "' exists, and `preventOverwriting` is set to `TRUE`; ",
               "did not write the file!");
        }
      }
      return(invisible(res));
    } else {
      stop("You passed '", output,
           "' as output filename, but directory '", dirname(output),
           "' does not exist!");
    }
  }

}
