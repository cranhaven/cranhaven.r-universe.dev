#' @rdname basicSPSStranslation
#' @export
filterBy <- function(dat, expression,
                     replaceOriginalDataframe=TRUE,
                     envir = parent.frame()) {
  ### Store original dataframe and current time
  originalDataframeName <- as.character(substitute(dat));
  currentTime <- Sys.time();
  timeStamp <- round(as.numeric(currentTime) * 100);
  newDataframeName <- paste0('.', originalDataframeName, "_at_", timeStamp);

  ### Store original dataframe with new name in parent environment
  assign(newDataframeName, value=dat, envir=envir);

  ### Store number of rows for reporting to user
  nrOfRows <- nrow(dat);

  if (!is.logical(expression)) {
    if (is.character(expression)) {
      ### Replace single 'equals' characters with the 'equals' operator
      expression <- gsub("([^=])=([^=])", "\\1==\\2", expression);
      ### Generate logical vector
      expression <- with(dat, eval(parse(text=expression)));
    }
    else {
      stop("The argument 'expression' must be either a logical vector or a character string with a logical expression!");
    }
  }

  ### Create filtered dataframe
  dat <- dat[expression, ];

  attr(dat, "originalDataframeName") <- originalDataframeName;
  attr(dat, "lastUnfilteredDataframeName") <- newDataframeName;
  attr(dat, "lastUnfilteredDataframeEnvir") <- envir;
  attr(dat, "lastFiltering") <- currentTime;

  cat("Filtered ", nrOfRows - nrow(dat) ,
      " rows (records, cases, participants, or datapoints) from dataframe '",
      originalDataframeName, "'; result has ", nrow(dat), " rows.\n", sep="");

  if (replaceOriginalDataframe) {
    assign(originalDataframeName, value=dat, envir=sys.frame(-1));
    invisible(dat);
  }
  else {
    return(dat);
  }

}
