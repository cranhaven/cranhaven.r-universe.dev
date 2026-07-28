sanitize_identifiers <- function(x,
                                 regex = "[^a-zA-Z0-9_]",
                                 warningMsg = "Found (and removed) illegal characters in code identifiers %s. They were changed to %s.") {

  if (is.null(x)) {
    return(x);
  }

  if (all(is.na(x))) {
    return(x);
  }

  x <- trimws(x);

  sanitizedIds <-
    gsub(regex, "", x);

  illegalIds <- (x != sanitizedIds);

  if (any(illegalIds, na.rm=TRUE)) {
    warning(
      sprintf(
        message,
        vecTxtQ(x[illegalIds]),
        vecTxtQ(sanitizedIds[illegalIds])
      )
    );
  }

  return(sanitizedIds);

}
