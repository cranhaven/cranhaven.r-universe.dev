msg <- function(...,
                silent = preregr::opts$get("silent")) {
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
