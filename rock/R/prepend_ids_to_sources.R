#' @rdname prepending_uids
#' @param outputPrefix,outputSuffix The prefix and suffix to add to the
#' filenames when writing the processed files to disk.
#' @export
prepend_ids_to_sources <- function(input,
                                   output = NULL,
                                   outputPrefix = "",
                                   outputSuffix = "_withUIDs",
                                   origin=Sys.time(),
                                   preventOverwriting=rock::opts$get(preventOverwriting),
                                   encoding=rock::opts$get(encoding),
                                   silent=rock::opts$get(silent)) {

  uidPrefix <- rock::opts$get(uidPrefix);
  utteranceMarker <- rock::opts$get(utteranceMarker);

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!is.character(output) || !length(output)==1) {
    stop("Only specify a single string as 'output'!");
  }

  if (tolower(output) == "same") {
    if ((is.null(outputPrefix) || (nchar(outputPrefix) == 0)) &&
        (is.null(outputSuffix) || (nchar(outputSuffix) == 0))) {
      stop("If writing the output to the same directory, you must specify ",
           "an outputPrefix and/or an outputSuffix!");
    }
  } else {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE);

  res <- character();
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

    tmp <-
      prepend_ids_to_source(input = filename,
                            output = file.path(newFileDir,
                                               newFilename),
                            preventOverwriting = preventOverwriting,
                            origin=origin,
                            silent=silent);
    ### Setting origin to a few seconds in the future to make sure all
    ### uids are unique
    regexToMatch <-
      paste0("^\\[\\[", uidPrefix, "([^]]*)\\]\\].*$");
    last_uid <-
      gsub(regexToMatch, "\\1", utils::tail(tmp, 1));
    origin <-
      as.POSIXct((1+base30toNumeric(last_uid)) / 100, origin="1970-01-01");
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
  invisible(res);
}
