splitString <- function(x,
                        splittingValuesRegex = rock::opts$get("splittingValuesRegex")) {

  res <-
    strsplit(
      x,
      splittingValuesRegex
    );

  ### Retain empty elements (empty lines)
  res <- lapply(res, function(x) {
    if (length(x) == 0) {
      return("");
    } else {
      return(x);
    }
  });

  return(
    unlist(
      res
    )
  );

}
