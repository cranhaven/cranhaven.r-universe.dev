#' Get the code identifiers from QNA codings
#'
#' @param x A parsed source or multiple parsed sources
#' @param within Which type of network coding to look in
#' @param return What to return ('all', 'nodes', 'edges', 'from', 'to')
#' @param includeUIDs Whether to return the UIDs as well
#'
#' @returns A character vector or data frame
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Read a souce coded with the Qualitative Network Approach
#' qnaExample <-
#'   rock::parse_source(
#'     file.path(
#'       examplePath,
#'       "network-example-1.rock"
#'     )
#'   );
#'
#' rock::get_codeIds_from_qna_codings(
#'   qnaExample
#' );
get_codeIds_from_qna_codings <- function(x,
                                         within = "network",
                                         return = "all",
                                         includeUIDs = TRUE) {

  if (inherits(x, "rock_parsedSources")) {

    return(lapply(x$parsedSources, get_codeIds_from_qna_codings));

  } else if (!inherits(x, "rock_parsedSource")) {

    stop("As `x`, you have to pass either a single parsed source (as ",
         "produced with a call to `rock::parse_source()`), or an ",
         "object with multiple parsed sources (as produced with a ",
         "call to `rock::parsed_sources()`). However, you passed an ",
         "object with class ", vecTxtQ(x), ".");

  }

  from <-
    x$networkCodes[[within]]$coded_list$from[
      !is.na(x$networkCodes[[within]]$coded_list$from$from),
    ];

  names(from)[names(from) == "from"] <- "codeId";
  from$type <- "from";

  to <-
    x$networkCodes[[within]]$coded_list$to[
      !is.na(x$networkCodes[[within]]$coded_list$to$to),
    ];

  names(to)[names(to) == "to"] <- "codeId";
  to$type <- "to";

  type <-
    x$networkCodes[[within]]$coded_list$type[
      !is.na(x$networkCodes[[within]]$coded_list$type$type),
    ];

  names(type)[names(type) == "type"] <- "codeId";
  type$type <- "type";

  if (includeUIDs) {
    return(
      list(
        from = from,
        to = to,
        type = type
      )
    );
  }

  if (return == "all") {
    return(unique(c(from, to, type)));
  } else if (return == "nodes") {
    return(unique(c(from, to)));
  } else if (return %in% c("edges", "types")) {
    return(unique(c(type)));
  } else if (return == "from") {
    return(from);
  } else if (return == "to") {
    return(to);
  } else {
    return(NULL);
  }

}
