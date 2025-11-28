#' @rdname basicSPSStranslation
#' @export
useAll <- function(dat, replaceFilteredDataframe = TRUE) {
  ### Store name of filtered dataframe
  filteredDataframeName <- as.character(substitute(dat));
  ### Store number of rows in filtered dataframe
  nrOfRows <- nrow(dat);

  ### Get information required to find original dataframe
  originalDataframeName <- attr(dat, "originalDataframeName");
  lastUnfilteredDataframeName <- attr(dat, "lastUnfilteredDataframeName");
  lastUnfilteredDataframeEnvir <- attr(dat, "lastUnfilteredDataframeEnvir");
  lastFiltering <- attr(dat, "lastFiltering");

  ### Check whether original exists
  if (exists(lastUnfilteredDataframeName, envir=lastUnfilteredDataframeEnvir)) {
    dat <- get(lastUnfilteredDataframeName, envir=lastUnfilteredDataframeEnvir);
    rm(list=lastUnfilteredDataframeName, envir=lastUnfilteredDataframeEnvir);
  }
  else {
    stop("Could not find the original, prefiltered version of the dataframe (which was stored as '",
         lastUnfilteredDataframeName, " in environment '", lastUnfilteredDataframeEnvir,"').");
  }

  cat("Removed last applied filter to dataframe '", filteredDataframeName, "', which was ",
      "applied at ", format(lastFiltering), " and removed (filtered) ",
      nrow(dat) - nrOfRows, " rows (records, cases, participants, or datapoints) ",
      "from the dataframe that was originally called '", originalDataframeName,
      "'. Restored dataframe has ", nrow(dat), " rows.\n", sep="");

  if (replaceFilteredDataframe) {
    assign(filteredDataframeName, value=dat, envir=sys.frame(-1));
    cat("Replaced filtered dataframe '", filteredDataframeName, "'.\n", sep="");
    invisible(dat);
  }
  else {
    return(dat);
  }

}
