#' @rdname writing_sources
#' @param filenamePrefix,filenameSuffix Optional prefixes or suffixes to pre-
#' or append to the filenames when writing the files.
#' @param recursive Whether to recursively create directories if the `output`
#' directory does not yet exist.
#' @export
write_sources <- function(x,
                          output,
                          filenamePrefix = "",
                          filenameSuffix = "_written",
                          recursive = TRUE,
                          encoding = rock::opts$get('encoding'),
                          preventOverwriting = rock::opts$get("preventOverwriting"),
                          silent = rock::opts$get('silent')) {

  utteranceMarker <- rock::opts$get(utteranceMarker);

 if (!inherits(x, "rock_loaded_sources_list")) {
    ### `x` argument isn't valid
    stop("As `x`, pass an object of ",
         "class `rock_loaded_sources_list`, as produced by a call ",
         "to `load_sources`. The object ",
         "you provided has class(es) ",
         vecTxtQ(class(x)), ".");
  }

  if (tolower(output) == "same") {
    if ((is.null(filenamePrefix) || (nchar(filenamePrefix) == 0)) &&
        (is.null(filenameSuffix) || (nchar(filenameSuffix) == 0))) {
      stop("If writing the output to the same directory, you must specify ",
           "an filenamePrefix and/or an filenameSuffix!");
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

  ### Produce new filenames and write sources to disk

  res <- character();
  for (sourcename in names(x)) {

    newFilename <-
      paste0(filenamePrefix,
             sub("^(.*)\\.[a-zA-Z0-9]+$",
                 "\\1",
                 basename(sourcename)),
             filenameSuffix,
             ".rock");
    if (tolower(output) == "same") {
      newFileDir <-
        dirname(sourcename);
      if (!dir.exists(newFileDir)) {
        stop("You specified a list of loaded sources as `input`, but you ",
             "specified 'same' as `output`; however, at least one of the ",
             "source names does not contain a valid path (the source ",
             "with name '", sourcename, "').");
      }
    } else {
      newFileDir <-
        output;
    }

    write_source(
      x[[sourcename]],
      output = file.path(newFileDir, newFilename),
      encoding = encoding,
      preventOverwriting = preventOverwriting,
      silent = silent
    );

    res <-
      c(res,
        newFilename);

  }

  if (!silent) {
    message("I just wrote ", length(names(x)), " sources to path '",
            output,
            "' ",
            ifelse(preventOverwriting,
                   "(unless the files already existed)",
                   "(overwriting any files that may already have existed)"),
            ". Note that these files may all be overwritten if this ",
            "script is ran again (unless `preventOverwriting` is set to `TRUE`).");
  }

  invisible(res);

}
