#' Exporting tables to HTML
#'
#' This function exports data frames or matrices to
#' HTML, sending output to one or more of the console,
#' viewer, and one or more files.
#'
#' @param input Either a `data.frame`, `table`, or `matrix`,
#' or a list with three elements: `pre`, `input`, and `post`.
#' The `pre` and `post` are simply prepended and postpended to
#' the HTML generated based on the `input$input` element.
#' @param output The output: a character vector with one or more
#' of "`console`" (the raw concatenated input, without conversion
#' to HTML), "`viewer`", which uses the RStudio viewer if available,
#' and one or more filenames in existing directories.
#' @param tableOutputCSS The CSS to use for the HTML table.
#'
#' @return Invisibly, the (potentially concatenated) `input` as character
#' vector.
#' @export
#'
#' @examples exportToHTML(mtcars[1:5, 1:5]);
exportToHTML <- function(input,
                         output = ufs::opts$get('tableOutput'),
                         tableOutputCSS = ufs::opts$get('tableOutputCSS')) {

  ### Check whether to save to one or more files
  files <- setdiff(output,
                   c("viewer", "console"));
  if (length(files) > 0) {
    outputToFiles <- TRUE;
  } else {
    outputToFiles <- FALSE;
  }

  ### Set viewer based on whether we're in rstudio and interactive
  if (interactive() && ("viewer" %in% output)) {
    ### Set viewer depending on whether we're in RStudio
    if ((requireNamespace("rstudioapi", quietly = TRUE)) &&
        (rstudioapi::isAvailable())) {
      viewer <- rstudioapi::viewer;
    } else {
      viewer <- getOption("viewer",
                          utils::browseURL);
    }
    outputToViewer <- TRUE;
  } else {
    outputToViewer <- FALSE;
  }

  if (is.list(input) && (length(input) == 3) &&
      all(c("pre", "input", "post") %in% names(input))) {
    ### At the minimum, we want the converted table
    inputToHTML <-
      paste0(tableOutputCSS,
             "\n\n",
             knitr::kable(input$input, format="html"));
    inputToConsole <-
      unlist(paste0(utils::capture.output(print(input$input)),
                    collapse="\n"));
    ### If applicable, add the bit to prepend
    if (!is.null(input$pre)) {
      inputToHTML <-
        paste0("<p>\n",
               input$pre,
               "</p>\n",
               inputToHTML,
               collapse="\n");
      inputToConsole <-
        paste0(input$pre,
               "\n\n",
               inputToConsole,
               collapse="\n");
    }
    ### If applicable, add the bit to append
    if (!is.null(input$post)) {
      inputToHTML <-
        paste0(inputToHTML,
               "<p>\n",
               input$post,
               "</p>\n",
               collapse="\n");
      inputToConsole <-
        paste0(inputToConsole,
               "\n\n",
               input$post,
               collapse="\n");
    }
  } else if (any(c("data.frame", "matrix", "table") %in%
                 class(input))) {
    ### At the minimum, we want the converted table
    inputToHTML <-
      paste0(tableOutputCSS,
             "\n\n",
             knitr::kable(input, format="html"));
    inputToConsole <-
      unlist(paste0(utils::capture.output(print(input)),
                    collapse="\n"));
  } else {
    stop("As `input` argument, pass either a data.frame, ",
         "matrix, or table, or a list containing a data.frame, ",
         "matrix, or table in `$input`, and either `NULL` or ",
         "text/HTML as `$pre` and `$post`.");
  }

  ### Save to one or more files if needed
  if (length(files) > 0) {
    for (currentFile in outputToFiles) {
      if (dir.exists(dirname(output))) {
        htmltools::save_html(inputToHTML,
                             file = currentFile,
                             background = opts$get('exportHTMLbackground'),
                             libdir = "lib");
      } else {
        stop("You specified you wanted to export to '",
             currentFile, "', but directory '",
             dirname(currentFile), "' does not exist!");
      }
    }
  }

  ### If knitting, output HTML table
  if (isTRUE(getOption('knitr.in.progress'))) {
    cat(inputToHTML);
  } else {

    ### Show in viewer if needed
    if (outputToViewer) {
      htmltools::html_print(htmltools::HTML(inputToHTML),
                            background = opts$get('exportHTMLbackground'),
                            viewer=viewer);
    }

    ### Print output to console if needed
    if ("console" %in% output) {
      cat(inputToConsole);
    }

  }

  return(invisible(input$input));
}
