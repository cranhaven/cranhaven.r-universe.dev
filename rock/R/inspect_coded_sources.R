#' Read sources from a directory, parse them, and show coded fragments and code tree
#'
#' This function combines successive calls to [parse_sources()],
#' [collect_coded_fragments()] and [show_inductive_code_tree()].
#'
#' @param path The path containing the sources to parse and inspect.
#' @param parse_args The arguments to pass to [parse_sources()].
#' @param fragments_args The arguments to pass to [collect_coded_fragments()].
#' @param inductive_tree_args The arguments to pass
#' to [show_inductive_code_tree()].
#' @param deductive_tree_args Not yet implemented.
#'
#' @return The parsedSources object.
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Inspect a selection of example sources - this takes too long
#' ### to test, so hence the 'donttest' directive.
#' \donttest{
#' rock::inspect_coded_sources(
#'   examplePath,
#'   parse_args = list(regex = "test(.txt|.rock)")
#' );
#' }
inspect_coded_sources <- function(path,
                                  parse_args = list(extension = "rock|dct",
                                                    regex=NULL,
                                                    recursive=TRUE,
                                                    ignoreOddDelimiters = FALSE,
                                                    encoding=rock::opts$get("encoding"),
                                                    silent=rock::opts$get("silent")),
                                  fragments_args = list(codes = ".*",
                                                        context = 0),
                                  inductive_tree_args = list(codes = ".*",
                                                             output = "both",
                                                             headingLevel = 3),
                                  deductive_tree_args = list()) {

  parsedSources <-
    do.call(
      parse_sources,
      c(list(path = path),
        parse_args)
    );

  fragments <-
    do.call(
      collect_coded_fragments,
      c(list(x = parsedSources),
        fragments_args)
    );

  if ((length(parsedSources$inductiveCodeTrees) > 0) &&
      ("Node" %in% class(parsedSources$inductiveCodeTrees[[1]]))) {
    inductiveTrees <-
      do.call(
        show_inductive_code_tree,
        c(list(x = parsedSources),
          inductive_tree_args)
      );
    # if (isTRUE(getOption('knitr.in.progress'))) {
    #   print(inductiveTrees);
    # }
  } else {
    inductiveTrees <- "";
  }

  if ("Node" %in% class(parsedSources$deductiveCodeTrees)) {
    deductiveTrees <-
      do.call(
        show_inductive_code_tree,
        c(list(x = parsedSources),
          deductive_tree_args)
      );
  } else {
    deductiveTrees <- "";
  }

  if (isTRUE(getOption('knitr.in.progress'))) {
    res <-
      c("\n\n",
        inductiveTrees,
        "\n\n",
        deductiveTrees,
        "\n\n",
        fragments,
        "\n\n");
    return(
      knitr::asis_output(res)
    );
  } else {
    return(parsedSources);
  }

}
