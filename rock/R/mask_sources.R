#' @rdname masking_sources
#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @param filenameReplacement A character vector with two elements that represent,
#' respectively, the `pattern` and `replacement` arguments of the [gsub()] function.
#' In other words, the first argument specifies a regular expression to search for
#' in every processed filename, and the second argument specifies a regular
#' expression that replaces any matches with the first argument. Set to `NULL` to
#' not perform any replacement on the output file name.
#' @param outputPrefix,outputSuffix The prefix and suffix to add to the
#' filenames when writing the processed files to disk.
#' @export
mask_sources <- function(input,
                         output,
                         proportionToMask = 1,
                         outputPrefix = "",
                         outputSuffix = "_masked",
                         maskRegex = "[[:alnum:]]",
                         maskChar = "X",
                         perl = TRUE,
                         recursive=TRUE,
                         filenameRegex=".*",
                         filenameReplacement=c("_PRIVATE_", "_public_"),
                         preventOverwriting = rock::opts$get(preventOverwriting),
                         encoding = rock::opts$get(encoding),
                         silent=rock::opts$get(silent)) {

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

  if (tolower(output) == "same") {
    if ((is.null(outputPrefix) || (nchar(outputPrefix) == 0)) &&
        (is.null(outputSuffix) || (nchar(outputSuffix) == 0)) &&
        (is.null(filenameReplacement))) {
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
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

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

    if (!is.null(filenameReplacement)) {
      newFilename <-
        gsub(pattern = filenameReplacement[1],
             replacement = filenameReplacement[2],
             x = newFilename);
    }

    if (tolower(basename(filename)) == tolower(newFilename)) {
      warning("After prepending the `outputPrefix`, appending ",
              "the `outputSuffix`, and applying the replacements ",
              "in `filenameReplacement`, I still ended up with ",
              "an output filename ('", newFilename,
              "') that is the same as the input filename('",
              basename(filename), "'). I am skipping this file.");
    } else {
      mask_source(input = filename,
                  output = file.path(newFileDir,
                                     newFilename),
                  proportionToMask = proportionToMask,
                  maskRegex = maskRegex,
                  maskChar = maskChar,
                  perl = perl,
                  preventOverwriting = preventOverwriting,
                  encoding = encoding,
                  silent = silent);
      res <-
        c(res,
          newFilename);
    }
  }
  if (!silent) {
    message("I just wrote ", length(rawSourceFiles), " masked sources to path '",
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
