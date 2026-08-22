#' @param root The root code(s) for which to collect the coded fragments.
#' @param inductiveCodingHierarchyMarker The character(s) used to mark nesting of codes.
#'
#' @noRd
# #' @export
# #' @rdname collect_coded_fragments
collect_coded_fragments_recursively <- function(x,
                                                root,
                                                context = 0,
                                                attributes = NULL,
                                                omitHeading = FALSE,
                                                headingLevel = 3,
                                                add_html_tags = TRUE,
                                                cleanUtterances = FALSE,
                                                output = NULL,
                                                outputViewer = "viewer",
                                                template = "default",
                                                rawResult = FALSE,
                                                includeCSS = TRUE,
                                                includeBootstrap = rock::opts$get("includeBootstrap"),
                                                inductiveCodingHierarchyMarker = rock::opts$get("inductiveCodingHierarchyMarker"),
                                                preventOverwriting = rock::opts$get(preventOverwriting),
                                                silent=rock::opts$get(silent)) {

  if (!is.null(attributes)) {
    stop("Also selecting based on attributes is not yet implemented, sorry!");
  }

  if (length(root) == 0) {

    return(invisible(NULL));

  } else if (length(root) > 1) {

    res <- lapply(
      root,
      collect_coded_fragments_recursively,
      x = x,
      context = context,
      attributes = attributes,
      omitHeading = omitHeading,
      headingLevel = headingLevel,
      add_html_tags = add_html_tags,
      cleanUtterances = cleanUtterances,
      output = FALSE,
      outputViewer = FALSE,
      template = template,
      rawResult = rawResult,
      includeCSS = includeCSS,
      includeBootstrap = includeBootstrap,
      inductiveCodingHierarchyMarker = inductiveCodingHierarchyMarker,
      preventOverwriting = preventOverwriting,
      silent=silent
    );

    res <- do.call(
      paste,
      c(list(collapse = "\n\n\n"), res)
    );

    return(res);

  } else {

    if (omitHeading) {
      res <- character();
    } else {
      res <-
        rock::heading(
          "Code: `", root, "`",
          headingLevel = headingLevel,
          cat = FALSE
        );
    }

    ### Get all children in the designated 'root'
    allParentCodes <-
      rock::get_childCodeIds(
        x,
        root
      );

    if (is.null(allParentCodes) || all(is.na(allParentCodes)) || (length(allParentCodes) == 0)) {
      msg("Code `", root, "` has no child codes.\n",
          silent = silent);
    } else {
      msg("Code `", root, "` has ", length(allParentCodes), " child codes.\n",
          silent = silent);
    }

    if ((root %in% names(x$mergedSourceDf)) &&
      sum(x$mergedSourceDf[, root] == 1) > 0) {

      res <- c(res,
               collect_coded_fragments(
                 x,
                 codes = paste0("^", root, inductiveCodingHierarchyMarker,
                                "?$"),
                 context = context,
                 attributes = attributes,
                 headingLevel = headingLevel,
                 add_html_tags = add_html_tags,
                 cleanUtterances = cleanUtterances,
                 template = template,
                 rawResult = rawResult,
                 outputViewer = FALSE,
                 includeCSS = FALSE
               ));

    }

    if (!(is.null(allParentCodes) || all(is.na(allParentCodes)) || (length(allParentCodes) == 0))) {

      for (currentParentCode in allParentCodes) {

        res <- c(res,
                 collect_coded_fragments_recursively(
                   x,
                   root = currentParentCode,
                   context = context,
                   attributes = attributes,
                   headingLevel = headingLevel + 1,
                   add_html_tags = add_html_tags,
                   cleanUtterances = cleanUtterances,
                   template = template,
                   rawResult = rawResult,
                   outputViewer = FALSE,
                   includeCSS = FALSE
                 ));

      }

    }

    return(res);

  }

}
