#' @rdname prepending_uids
#' @param outputPrefix,outputSuffix The prefix and suffix to add to the
#' filenames when writing the processed files to disk.
#' @param uidSpacing The number of UID spaces to leave between sources (in case
#' more data may follow in with source).
#' @export
prepend_ids_to_sources <- function(input,
                                   output = NULL,
                                   outputPrefix = "",
                                   outputSuffix = "_withUIDs",
                                   origin=Sys.time(),
                                   follow = NULL,
                                   followBy = NULL,
                                   uidSpacing = NULL,
                                   preventOverwriting=rock::opts$get(preventOverwriting),
                                   encoding=rock::opts$get(encoding),
                                   silent=rock::opts$get(silent)) {

  uidPrefix <- rock::opts$get(uidPrefix);
  utteranceMarker <- rock::opts$get(utteranceMarker);

  if (is.list(input) && (all(unlist(lapply(input, is.character))))) {

    res <- list();

  } else if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string (with the path to a directory ",
         "with sources) or a list of character vectors (where each character ",
         "vector is a source) as 'input'! 'input' now has class(es) ",
         vecTxtQ(class(input)), ".");
  } else {

    res <- character();

  }

  if (((!is.null(output)) && !is.character(output)) || (!is.null(output) && !length(output)==1)) {
    stop("Only specify a single string as 'output'!");
  }

  if ((!is.null(output)) && tolower(output) == "same") {
    if ((is.null(outputPrefix) || (nchar(outputPrefix) == 0)) &&
        (is.null(outputSuffix) || (nchar(outputSuffix) == 0))) {
      stop("If writing the output to the same directory, you must specify ",
           "an outputPrefix and/or an outputSuffix!");
    }
  } else {
    if (!is.null(output) && !dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  ### Used to override following distance in sources other than the first
  furtherFollowBy <- NULL;

  if (is.character(res)) {

    rawSourceFiles <-
      list.files(input,
                 full.names=TRUE);

    ### Loop through files
    for (filename in rawSourceFiles) {
      newFilename <-
        paste0(outputPrefix,
               sub("^(.*)\\.[a-zA-Z0-9]+$",
                   "\\1",
                   basename(filename)),
               outputSuffix,
               ".rock");
      if (tolower(output) == "same") {
        newFileDir <-
          dirname(filename);
      } else {
        newFileDir <-
          output;
      }

      if (!is.null(furtherFollowBy)) {
        followBy <- furtherFollowBy;
      }

      tmp <-
        prepend_ids_to_source(input = filename,
                              output = file.path(newFileDir,
                                                 newFilename),
                              follow = follow,
                              followBy = followBy,
                              preventOverwriting = preventOverwriting,
                              origin=origin,
                              silent=silent);

      ### Getting UIDs
      regexToMatch <-
        paste0("\\[\\[", uidPrefix, "([0123456789bcdfghjklmnpqrstwxyz]{8})\\]\\].*");
      follow <-
        gsub(regexToMatch, "\\1", res[[i]], perl = TRUE);

      if (!is.null(uidSpacing)) {
        ### Used for all but the first source as UID spacing
        furtherFollowBy <- uidSpacing;
      }

    }

    if (!silent) {
      message("I just added utterenance identifiers to ", length(rawSourceFiles),
              " sources and wrote the new files to path '",
              output,
              "' ",
              ifelse(preventOverwriting,
                     "(unless the files already existed)",
                     "(overwriting any files that may already have existed)"),
              ". Note that these files may all be overwritten if this ",
              "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
              "Therefore, make sure to copy them to ",
              "another directory before starting to code those sources!");
    }

  } else if (is.list(res)) {

    ### When being provided with a list of character vectors

    for (i in seq_along(input)) {

      if (!is.null(furtherFollowBy)) {
        followBy <- furtherFollowBy;
      }

      res[[i]] <-
        prepend_ids_to_source(input = input[[i]],
                              output = NULL,
                              follow = follow,
                              followBy = followBy,
                              origin=origin,
                              silent=silent);

      ### Getting UIDs
      regexToMatch <-
        paste0("\\[\\[", uidPrefix, "([0123456789bcdfghjklmnpqrstwxyz]{8})\\]\\].*");
      follow <-
        gsub(regexToMatch, "\\1", res[[i]], perl = TRUE);

      if (!is.null(uidSpacing)) {
        ### Used for all but the first source as UID spacing
        furtherFollowBy <- uidSpacing;
      }

    }

    if (!is.null(names(input))) {
      names(res) <- names(input);
    }

    if (!silent) {
      message("I just added utterenance identifiers to ", length(input),
              " sources.");
    }

  }

  invisible(res);
}
