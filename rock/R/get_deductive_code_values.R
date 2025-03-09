get_deductive_code_values <- function(x,
                                      name="id") {
  if (all(is.null(x)) || all(is.na(x))) {
    return(x);
  }
  if (name %in% names(x)) {
    res <- unlist(x[name]);
  } else {
    res <- NULL;
  }
  if (any(unlist(lapply(x, 'is.list')))) {
    ### Children; call ourselves and return result
    res <- c(res,
             unlist(lapply(x,
                           get_deductive_code_values,
                           name=name)));
  } else {
    ### No children; we extracted the only value, so return it.
    return(res);
  }
  res <- unname(sort(unique(res)));
  return(res);
}
