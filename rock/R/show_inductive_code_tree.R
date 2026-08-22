#' Show the inductive code tree(s)
#'
#' This function shows one or more inductive code trees.
#'
#' @param x A `rock_parsedSources` object (the result of a call to
#' `rock::parse_sources`).
#' @param codes A regular expression: only code trees from codes coded
#' with a coding pattern with this name will be shown.
#' @param output Whether to show the code tree in the console (`text`),
#' as a plot (`plot`), or both (`both`).
#' @param headingLevel The level of the heading to insert when showing the
#' code tree as text.
#' @param nodeStyle,edgeStyle,graphStyle Arguments to pass on to,
#' respectively, [data.tree::SetNodeStyle()], [data.tree::SetEdgeStyle()],
#' and [data.tree::SetGraphStyle()].
#'
#' @return `x`, invisibly, unless being knitted into R Markdown,
#' in which case a [knitr::asis_output()]-wrapped character vector is returned.
#' @export
show_inductive_code_tree <- function(x,
                                     codes = ".*",
                                     output = "both",
                                     headingLevel = 3,
                                     nodeStyle = list(shape = "box",
                                                      fontname = "Arial"),
                                     edgeStyle = list(arrowhead = "none"),
                                     graphStyle = list(rankdir = "LR")) {

  if (!(("rock_parsedSources" %in% class(x)) |
        ("rock_parsedSource"  %in% class(x)))) {
    stop("As `x`, you must pass either an `rock_parsedSource` or ",
         "an `rock_parsedSources` object (i.e. either the result ",
         "from a call to `rock::parseSource()` or the result from ",
         "a call to `rock::parseSources()`). However, you ",
         "provided an object of class ", vecTxtQ(x), ".");
  }

  trees <- names(x$inductiveDiagrammeRs);

  if (is.null(x$inductiveDiagrammeRs)) {
    return(invisible(NULL));
  }

  trees <-
    trees[!unlist(lapply(x$inductiveDiagrammeRs, is.null))];

  res <- c();

  for (i in trees) {
    if (grepl("both|text", output)) {
      if (isTRUE(getOption('knitr.in.progress'))) {
        res1 <-
          c("\n\n",
            repStr("#", headingLevel),
            " Inductive code tree for ",
            i,
            "\n\n");
        res3 <-
          c("<pre>",
            paste0(
              utils::capture.output(
                print(
                  x$inductiveCodeTrees[[i]])
                ),
              collapse="\n"
            ),
            "</pre>");
      } else {
        res1 <- "";
        res3 <- "";
        print(x$inductiveCodeTrees[[i]]);
      }
    }
    if (grepl("both|plot", output)) {
      if (isTRUE(getOption('knitr.in.progress'))) {
        dot_code <- DiagrammeR::generate_dot(x$inductiveDiagrammeRs[[i]]);
        graphSvg <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
        graphSvg <- sub(".*\n<svg ", "<svg ", graphSvg);
        graphSvg <- gsub("<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=",
                         "<svg viewBox=", graphSvg);
        res2 <- graphSvg;
      } else {
        res2 <- "";
        print(
          DiagrammeR::render_graph(
            x$inductiveDiagrammeRs[[i]]
          )
        );
      }
      # do.call(data.tree::SetNodeStyle,
      #         c(list(node = x$inductiveCodeTrees[[i]]),
      #           nodeStyle));
      # do.call(data.tree::SetEdgeStyle,
      #         c(list(node = x$inductiveCodeTrees[[i]]),
      #           edgeStyle));
      # do.call(data.tree::SetGraphStyle,
      #         c(list(root = x$inductiveCodeTrees[[i]]),
      #           graphStyle));
      # print(plot(x$inductiveCodeTrees[[i]]));
    }

    res <-
      c(res,
        res1,
        res2,
        res3);

  }

  if (isTRUE(getOption('knitr.in.progress'))) {
    return(knitr::asis_output(c("\n\n",
                                res,
                                "\n\n")));
  }

  return(invisible(x));

}
