#' Export a merged source data frame
#'
#' @param x The object with parsed sources.
#' @param file The file to export to.
#' @param exportArgs Optionally, arguments to pass to the function to use to
#' export.
#' @param preventOverwriting Whether to prevent overwriting if the file already
#' exists.
#' @param silent Whether to be silent or chatty.
#'
#' @return Silently, the object with parsed sources.
#' @rdname export_mergedSourceDf_to_file
#' @export
export_mergedSourceDf_to_csv <- function(x,
                                         file,
                                         exportArgs = list(fileEncoding = rock::opts$get('encoding')),
                                         preventOverwriting = rock::opts$get('preventOverwriting'),
                                         silent = rock::opts$get('silent')) {
  return(
    export_mergedSourceDf_to_file(
      x = x,
      file = file,
      exportArgs = exportArgs,
      preventOverwriting = preventOverwriting,
      silent = silent,
      exportFunc = utils::write.csv
    )
  );
}

#' @rdname export_mergedSourceDf_to_file
#' @export
export_mergedSourceDf_to_csv2 <- function(x,
                                          file,
                                          exportArgs = list(fileEncoding = rock::opts$get('encoding')),
                                          preventOverwriting = rock::opts$get('preventOverwriting'),
                                          silent = rock::opts$get('silent')) {
  return(
    export_mergedSourceDf_to_file(
      x = x,
      file = file,
      exportArgs = exportArgs,
      preventOverwriting = preventOverwriting,
      silent = silent,
      exportFunc = utils::write.csv2
    )
  );
}

#' @rdname export_mergedSourceDf_to_file
#' @export
export_mergedSourceDf_to_xlsx <- function(x,
                                          file,
                                          exportArgs = NULL,
                                          preventOverwriting = rock::opts$get('preventOverwriting'),
                                          silent = rock::opts$get('silent')) {

  if (requireNamespace("writexl", quietly = TRUE)) {
    return(
      export_mergedSourceDf_to_file(
        x = x,
        file = file,
        exportArgs = exportArgs,
        preventOverwriting = preventOverwriting,
        silent = silent,
        exportFunc = writexl::write_xlsx
      )
    );
  } else {
    stop("To export to .xlsx files, you need to have the {writexl} package ",
         "installed. You can install it with:\n\n  ",
         "install.packages('writexl');\n");
  }
}

#' @rdname export_mergedSourceDf_to_file
#' @export
export_mergedSourceDf_to_sav <- function(x,
                                         file,
                                         exportArgs = NULL,
                                         preventOverwriting = rock::opts$get('preventOverwriting'),
                                         silent = rock::opts$get('silent')) {

  if (requireNamespace("haven", quietly = TRUE)) {
    return(
      export_mergedSourceDf_to_file(
        x = x,
        file = file,
        exportArgs = exportArgs,
        preventOverwriting = preventOverwriting,
        silent = silent,
        exportFunc = haven::write_sav
      )
    );
  } else {
    stop("To export to .sav files, you need to have the {haven} package ",
         "installed. You can install it with:\n\n  ",
         "install.packages('haven');\n");
  }
}

export_mergedSourceDf_to_file <- function(x,
                                          file,
                                          exportFunc,
                                          exportArgs = NULL,
                                          preventOverwriting = rock::opts$get('preventOverwriting'),
                                          silent = rock::opts$get('silent')) {

  if (!(inherits(x, "rock_parsedSource") ||
        inherits(x, "rock_parsedSources"))) {
    stop("As `x`, you have to pass an object with one or more parsed sources; ",
         "you passed on object of class(es) ", vecTxtQ(class(x)), ".");
  }

  if ((file.exists(file)) && preventOverwriting) {

    warning("The file you specified to export to (", file, ") already ",
            "exists, and `preventOverwriting` is set to `TRUE`, so I'm ",
            "not writing to disk. To override this, pass ",
            "`preventOverwriting=FALSE`.");

  } else {

    do.call(
      exportFunc,
      c(list(x$mergedSourceDf,
             file),
        exportArgs)
    );

  }

  return(invisible(x));

}
