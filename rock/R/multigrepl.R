multigrepl <- function(patterns,
                       x,
                       returnMatchesForPatterns=TRUE,
                       ...) {
  if (returnMatchesForPatterns) {
    bindFun <- rbind;
  } else {
    bindFun <- cbind;
  }
  res <- do.call(bindFun,
                 lapply(patterns,
                        grepl,
                        x=x,
                        ...));
  res <- apply(res,
               1,
               any);
  return(res);
}
