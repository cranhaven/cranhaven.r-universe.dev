get_leaf_code <- function(x,
                          inductiveCodingHierarchyMarker) {

  if ((length(x)==0) || is.na(x)) {
    return(NA_character_);
  }

  if (length(x) > 1) {
    stop("Only pass exactly one code / string!");
  }

  ### Split by hierarchy marker; select first object in list,
  ### because strsplit always returns a list of the results
  ### since it's vectorized over x
  res <-
    strsplit(x,
             inductiveCodingHierarchyMarker)[[1]];

  ### Extract last substring and return it.
  return(utils::tail(res, 1));
}
