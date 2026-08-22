#' Prettify a source in HTML
#'
#' This function adds HTML tags to a source to allow pretty printing/viewing.
#' For an example, visit <https://rock.opens.science/articles/rock.html>.
#'
#' @param x The source, as imported with [rock::load_source()] or as a path
#' to a file.
#' @param preserveSpaces Whether to preservce spaces (by replacing every second
#' space with `&nbsp;`) or not.
#'
#' @inheritParams collect_coded_fragments
#'
#' @returns A character vector with the prettified source
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Prettify source; if using RStudio, by default the
#' ### prettified source is shown in the viewer. You can
#' ### view the output of this example in the "rock" vignette
#' ### at https://rock.opens.science/articles/rock.html
#' rock::prettify_source(
#'   exampleFile
#' );
prettify_source <- function(x,
                            heading = NULL,
                            headingLevel = 2,
                            add_html_tags = TRUE,
                            output = NULL,
                            outputViewer = "viewer",
                            template = "default",
                            includeCSS = TRUE,
                            preserveSpaces = TRUE,
                            includeBootstrap = rock::opts$get("includeBootstrap"),
                            preventOverwriting = rock::opts$get(preventOverwriting),
                            silent=rock::opts$get(silent)) {

  if (is.null(x) || all(is.na(x))) {
    stop("As `x`, pass either the path to a file, or a source as ",
         "read with rock::load_source(). You passed either NULL or NA.");
  }

  if (!inherits(x, "rock_source")) {
    if ((length(x) == 1) && (file.exists(x))) {
      x <- rock::load_source(x);
    } else {
      stop("As `x`, pass either the path to a file, or a source as ",
           "read with rock::load_source(). You passed an object with class ",
           vecTxtQ(class(x)), ".");
    }
  }

  if (!is.null(heading)) {
    headingBit <-
      rock::heading(
        heading,
        headingLevel = headingLevel,
        cat = FALSE,
        output = "html"
      );
    headingLevel <- headingLevel + 1;
  } else {
    headingBit <- "";
  }

  if (add_html_tags && includeCSS) {
    cssBit <-
      rock::css(template=template,
                includeBootstrap = ifelse(is.character(includeBootstrap),
                                          TRUE,
                                          includeBootstrap));
  } else {
    cssBit <- "";
  }

  sourceBit <-
    add_html_tags(x);

  if (preserveSpaces) {
    sourceBit <-
      gsub("  ", "&nbsp;&nbsp;", sourceBit);
  }

  res <-
    paste0(
      "<html>\n",
      "<head>\n",
      '<link rel="preconnect" href="https://fonts.googleapis.com">',
      '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>',
      '<link href="https://fonts.googleapis.com/css2?family=Ubuntu+Mono:wght@400;700&display=swap" rel="stylesheet">\n',
      paste0(cssBit, collapse="\n"),
      "</head>\n",
      "<body>\n",
      paste0(headingBit, collapse="\n"),
      "\n",
      paste0(sourceBit, collapse="\n"),
      "</body>\n",
      "</html>\n"
    );

  ### Save to file

  if (!is.null(output)) {

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

    } else {
      stop("You passed '", output,
           "' as output filename, but directory '", dirname(output),
           "' does not exist!");
    }
  }

  ### Show in viewer, or return when knitting

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

  if (isTRUE(getOption('knitr.in.progress'))) {

    res <-
      knitr::asis_output(c("\n\n",
                           res,
                           "\n\n"));

    return(res);

  } else if (requireNamespace("pkgdown", quietly = TRUE) && pkgdown::in_pkgdown()) {

    # res <- htmltools::HTML(res);
    # class(res) <- c("prettified_ROCK_source", class(res));
    # pkgdown::pkgdown_print(res);

    return(
      c(
        "This example prints the HTML result in the viewer.",
        "However, for some reason PkgDown refuses to pretty",
        "print the HTML, despite it being a `htmltools::HTML()`",
        "result passed to a custom printing function. Therefore,",
        "you can check the example in the vignette at",
        "https://rock.opens.science/articles/rock.html",
        "If you happen to know a solution, please let me know!"
      )
    );

  } else {

    if (outputToViewer) {
      htmltools::html_print(htmltools::HTML(res),
                            background = "white",
                            viewer = viewer)
    }
    if ("console" %in% outputViewer) {
      cat(res)
    }
    return(invisible(res));

  }

}


# #' @export
# pkgdown_print.prettified_ROCK_source <- function(x, visible = TRUE) {
#
#   if (!visible) {
#     return(invisible());
#   } else {
#     cat(
#       x, sep="\n"
#     )
#   }
#
# }
#
# #' @export
# print.prettified_ROCK_source <- function(x, visible = TRUE) {
#
#   if (!visible) {
#     return(invisible());
#   } else {
#     cat(
#       x, sep="\n"
#     )
#   }
#
# }
