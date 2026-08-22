#' @rdname get_childCodeIds
#' @export
get_descendentCodeIds <- function(x,
                                  parentCodeId,
                                  includeParentCode = FALSE) {

  if ((!inherits(x, "rock_parsedSources")) &&
      (!inherits(x, "rock_parsedSource"))) { ### Added this, might be wrong
    stop("As `x`, you have to pass a parsed sources object. You passed ",
            deparse(substitute(x)), "', which instead has class(es) ",
            vecTxtQ(class(x)), ".");
  }

  if ((!is.character(parentCodeId)) | (length(parentCodeId) != 1)) {
    stop("As `parentCodeId`, you have to pass a single character value. ",
         "However, what you passed is either not a character value, or ",
         "has a length other than 1 (i.e. 0, 2, or larger).");
  }

  if (inherits(x, "rock_parsedSource")) {
    node <- NULL;
    for (i in seq_along(x$inductiveCodeTrees)) {
      node <- NULL;
      if ((!is.null(x$inductiveCodeTrees[[i]])) && is.null(node)) {
        node <-
          data.tree::FindNode(x$inductiveCodeTrees[[i]], parentCodeId);
      }
    }
  } else if (inherits(x, "rock_parsedSources")) {
    node <-
      data.tree::FindNode(x$fullyMergedCodeTrees, parentCodeId);
  } else {
    stop("As `x`, you passed '",
         substitute(deparse(x)), "', but this does not have class ",
         "`rock_parsedSource` or `rock_parsedSources`.");
  }

  if (is.null(node)) {
    stop("In the parsed sources object that you passed (",
         substitute(deparse(x)), "), no code identifier '",
         parentCodeId, "' was found.");
  }

  nodeNames <- node$Get('name');

  if (!includeParentCode) {
    nodeNames <- setdiff(nodeNames, parentCodeId);
  }

  return(nodeNames);

}
