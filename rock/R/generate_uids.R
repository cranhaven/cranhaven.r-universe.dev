#' Generate utterance identifiers (UIDs)
#'
#' This function generates utterance identifiers. Utterance identifiers are
#' Short Quasi-Unique Identifiers (SQUIDs) generated using
#' the [squids::squids()] function.
#'
#' @param x The number of identifiers to generate.
#' @param origin The origin to use when generating the actual
#' identifiers; see the [squids::squids()] documentation.
#' @param follow A vector of one or more UIDs (or a list; lists are
#' recursively `unlist()`ed); the highest UID will be taken, converted
#' to a timestamp, and used as `origin` (well, 0.01 second later), so that the
#' new UIDs will follow that sequence.
#' @param followBy When following a vector of UIDs, this can be used to
#' specify the distance between the two vectors in centiseconds.
#'
#' @return A vector of UIDs.
#' @export
#'
#' @examples ### Produce and store five UIDs
#' fiveUIDs <-
#'   rock::generate_uids(5);
#'
#' ### Look at them
#' fiveUIDs;
#'
#' ### Use a specific origin to be able to reproduce
#' ### a set of UIDs later (e.g. in a script)
#' uidOrigin <-
#'   as.POSIXct("2025-05-21 21:53:25 CEST");
#'
#' rock::generate_uids(
#'   5,
#'   origin = uidOrigin
#' );
#'
#' ### Produce five more UIDs to show
#' ### their 'progression'
#' rock::generate_uids(5);
#'
#' ### Produce a set of five UIDs that follow
#' ### the first set of five UIDs
#' rock::generate_uids(
#'   5,
#'   follow = fiveUIDs
#' );
#'
#' ### Follow with a 'distance' of 5 utterances
#' rock::generate_uids(
#'   5,
#'   follow = fiveUIDs,
#'   followBy = 5
#' );
generate_uids <- function(x,
                          origin=Sys.time(),
                          follow = NULL,
                          followBy = NULL) {

  uidPrefix <- rock::opts$get("uidPrefix");
  codeDelimiters <- rock::opts$get("codeDelimiters");

  if (!is.null(follow)) {
    follow_as_SQUID <-
      gsub(
        paste0(
          codeDelimiters[1],
          uidPrefix
        ),
        "",
        follow,
        fixed = TRUE
      );
    follow_as_SQUID <-
      gsub(
        codeDelimiters[2],
        "",
        follow_as_SQUID,
        fixed = TRUE
      );
  } else {
    follow_as_SQUID <- NULL;
  }

  res <-
    squids::squids(
      x = x,
      origin = origin,
      follow = follow_as_SQUID,
      followBy = followBy
    );

  return(
    paste0(
      codeDelimiters[1],
      uidPrefix,
      res,
      codeDelimiters[2]
    )
  );

}
