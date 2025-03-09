#' @rdname coding_sources
#' @param outputPrefix,outputSuffix A prefix and/or suffix to prepend and/or
#' append to the filenames to distinguish them from the input filenames.
#' @param recursive Whether to also read files from all subdirectories
#' of the `input` directory
#' @param filenameRegex Only input files matching this patterns will be read.
#' @export
code_sources <- function(input,
                         codes,
                         output = NULL,
                         indices = NULL,
                         outputPrefix = "",
                         outputSuffix = "_coded",
                         decisionLabel = NULL,
                         justification = NULL,
                         justificationFile = NULL,
                         recursive=TRUE,
                         filenameRegex=".*",
                         preventOverwriting = rock::opts$get('preventOverwriting'),
                         encoding = rock::opts$get('encoding'),
                         silent = rock::opts$get('silent')) {

  utteranceMarker <- rock::opts$get(utteranceMarker);

  if (is.character(input) && (length(input)==1)) {
    if (dir.exists(input)) {
      ### Get list of files to code,; this list is the equivalent of the
      ### names of the sources if a 'loaded sources' object is provided.
      sourceNames <-
        list.files(input,
                   full.names=TRUE,
                   pattern = filenameRegex,
                   recursive=recursive);

      ### Delete directories, if any were present
      sourceNames <-
        setdiff(sourceNames,
                list.dirs(input,
                          full.names=TRUE));

      ### Set class for object to return
      sourceClass <- "rock_loaded_sources_list";

    } else {
      ### Directory doesn't exist, stop
      stop("Directory provided to read from ('",
           input,
           "') does not exist!");
    }
  } else if ("rock_loaded_sources_list" %in% class(input)) {
    ### Store source names and class
    sourceNames <-
      names(input);
    sourceClass <-
      class(input);

  } else {
    ### `input` argument isn't valid
    stop("As `input`, specify either the path to a directory containing ",
         "one or more sources to code, or an object of ",
         "class `rock_loaded_sources_list`, as produced by a call ",
         "to `load_sources`. The object ",
         "you provided has class ",
         vecTxtQ(class(input)), ".");
  }

  if (!is.null(output)) {
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
  }

  if (is.null(output)) {

    ### Process sources and return the coded sources as result

    res <- lapply(input,
                  code_source,
                  output=NULL,
                  codes=codes,
                  silent=silent);

    names(res) <- sourceNames;
    class(res) <- sourceClass;

    if (!silent) {
      message("I just coded ", length(res), " sources.");
    }

  } else {

    ### Read sources, code them, and write them to disk again

    res <- character();
    for (sourcename in sourceNames) {
      newFilename <-
        paste0(outputPrefix,
               sub("^(.*)\\.[a-zA-Z0-9]+$",
                   "\\1",
                   basename(sourcename)),
               outputSuffix,
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

      code_source(input = sourcename,
                  output = file.path(newFileDir,
                                     newFilename),
                  codes=codes,
                  indices=NULL,
                  decisionLabel = decisionLabel,
                  justification = justification,
                  justificationFile = justificationFile,
                  preventOverwriting = preventOverwriting,
                  encoding = encoding,
                  silent = silent);
      res <-
        c(res,
          newFilename);
    }
    if (!silent) {
      message("I just wrote ", length(sourceNames), " autocoded sources to path '",
              output,
              "' ",
              ifelse(preventOverwriting,
                     "(unless the files already existed)",
                     "(overwriting any files that may already have existed)"),
              ". Note that these files may all be overwritten if this ",
              "script is ran again (unless `preventOverwriting` is set to `TRUE`).");
    }

  }

  invisible(res);

}
