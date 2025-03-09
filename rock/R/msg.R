msg <- function(...,
                silent = rock::opts$get("silent")) {
  if (!silent) {
    cat0(...);
  }
  return(
    invisible(
      paste0(
        ...
      )
    )
  );
}
