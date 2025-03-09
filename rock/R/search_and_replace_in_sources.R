#' @rdname cleaning_sources
#' @export
search_and_replace_in_sources <- function(input,
                                          output,
                                          replacements = NULL,
                                          outputPrefix = "",
                                          outputSuffix = "_postReplacing",
                                          preventOverwriting = rock::opts$get("preventOverwriting"),
                                          recursive=TRUE,
                                          filenameRegex=".*",
                                          encoding = rock::opts$get("encoding"),
                                          silent=rock::opts$get("silent")) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!is.character(output) || !length(output)==1) {
    stop("Only specify a single string as 'output'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (!(tolower(output) == "same")) {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  } else {
    if ((nchar(outputPrefix) == 0) && (nchar(outputSuffix) == 0)) {
      stop("You have to provide at least one of `outputPrefix` and `outputSuffix` ",
           "to allow saving the files to new names!");
    }
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

  skippedFiles <- character();
  res <- character();
  for (filename in rawSourceFiles) {
    if (((nchar(outputPrefix) > 0) &&
          grepl(outputPrefix,
                basename(filename))) ||
        ((nchar(outputSuffix) > 0) &&
          grepl(outputSuffix,
                basename(filename)))) {
      skippedFiles <-
        c(skippedFiles,
          filename);
      msg("File '", basename(filename), "' already contains ",
          "the prefix ('", outputPrefix, "') or suffix ('",
          outputSuffix, "') string; skipping this file!",
          silent = silent);
    } else {
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
      single_search_replace_result <-
        search_and_replace_in_source(input = filename,
                                     output = file.path(newFileDir,
                                                        newFilename),
                                     replacements=replacements,
                                     preventOverwriting=preventOverwriting,
                                     encoding=encoding,
                                     silent=TRUE);
      if (attr(single_search_replace_result,
               "output") == "existed") {
        skippedFiles <-
          c(skippedFiles,
            filename);
      }
      res <-
        c(res,
          newFilename);
    }
  }
  msg(
    "Out of ", length(rawSourceFiles), " provided sources, I just wrote ",
    length(rawSourceFiles) - length(skippedFiles),
    " 'post-search-replace-sources' to path '",
    output,
    "' ",
    ifelse(preventOverwriting,
           paste0("(skipping ", length(skippedFiles),
                  " files that already existed)"),
           "(overwriting any files that may already have existed)"),
    ". Note that these files may all be overwritten if this ",
    "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
    "Therefore, make sure to copy them to ",
    "another directory before starting to code those sources!\n\n",
    "A recommended convention is to place all data in a directory ",
    "called 'data', and use three subdirectories: 'raw-sources' for ",
    "the raw sources; 'clean-sources' for the cleaned sources, ",
    "and 'coded-sources' for the coded sources. If you have ",
    "multiple coders, use e.g. 'coded-sources-coder-A' and ",
    "'coded-sources-coder-B' to organise these versions, or use ",
    "different filenames (and use the coderId).",
    silent=silent
  );
return(invisible(res));
}
