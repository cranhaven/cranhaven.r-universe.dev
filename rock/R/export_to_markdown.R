#' @rdname exporting_sources
#' @export
export_to_markdown <- function(input,
                               heading = "Sources",
                               headingLevel = 2,
                               template = "default",
                               silent=rock::opts$get(silent)) {

  if (!("rock_parsedSources" %in% class(input))) {
    stop("As argument 'input', only provide an object with parsed sources, ",
         "such as results from a call to `rock::parse_sources()`. You ",
         "provided an object of class(es) ", rock::vecTxtQ(class(input)), ".");
  }

  htmlSources <-
    export_to_html(input,
                   template=template,
                   fragment = TRUE);

  res <- paste0("\n\n",
                rock::css(template = template),
                "\n\n",
                repStr("#", headingLevel),
                " ", heading, " {.tabset}\n\n",
                repStr("#", headingLevel+1),
                " Overview\n\n",
                "This is an overview of all ", length(htmlSources),
                " sources in this project.\n\n",
                "In HTML output, these are presented in a tabbed interface; ",
                "by selecting one of the tabs above, you can see the sources'",
                "contents. In PDF output, all sources are printed below.\n\n",
                "The included sources are ", vecTxtQ(names(htmlSources)),
                ".\n\n");

  for (i in names(htmlSources)) {
    res <- paste0(res,
                  "\n\n",
                  repStr("#", headingLevel+1),
                  " ", i, "\n\n",
                  htmlSources[[i]]);
  }

  cat(res);

  return(invisible(res));

}
