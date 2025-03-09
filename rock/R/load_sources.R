#' @rdname loading_sources
#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @param ignoreRegex Regular expression indicating which files to ignore. This
#' is a perl-style regular expression (see [base::regex]).
#' @param full.names Whether to store source names as filenames only or whether
#' to include paths.
#'
#' @export
load_sources <- function(input,
                         filenameRegex = ".*",
                         ignoreRegex = NULL,
                         recursive = TRUE,
                         full.names = FALSE,
                         encoding = rock::opts$get('encoding'),
                         silent = rock::opts$get('silent')) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single path as `input`!");
  }

  if (file.exists(input) && (!dir.exists(input))) {

    ### Read this one file only
    rawSourceFiles <- input;

  } else if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  } else {

    rawSourceFiles <-
      list.files(input,
                 pattern=filenameRegex,
                 recursive=recursive,
                 full.names=TRUE);

    if (!is.null(ignoreRegex)) {
      rawSourceFiles <-
        rawSourceFiles[!grepl(ignoreRegex,
                              rawSourceFiles,
                              perl=TRUE)];
    }
  }

  res <- list();
  for (filename in rawSourceFiles) {
    fileNameToUse <-
      ifelse(full.names,
             filename,
             basename(filename));

    if (file.exists(filename) && !dir.exists(filename)) {
      res[[fileNameToUse]] <-
        load_source(filename,
                    encoding=encoding,
                    silent=silent);
    }

  }

  if (!silent) {
    cat0("I just loaded ", length(rawSourceFiles), " sources.\n");
  }

  class(res) <-
    "rock_loaded_sources_list";

  invisible(res);
}
