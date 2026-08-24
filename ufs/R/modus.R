modus <- function(vector) {
  if (is.data.frame(vector) | is.matrix(vector)) {
    stop("The first argument is not a vector! If you need to specify ",
         "a variable from a dataframe, separate the name of the ",
         "dataframe and the variable name with a dollar sign, for ",
         "example using 'dat$gender' to extract variable 'gender' from ",
         "dataframe 'dat'.");
  }
  ### Store original class
  originalClass <- class(vector);
  ### Convert to factor
  vector <- as.factor(vector);
  ### Store frequencies
  freqs <- summary(vector);
  ### Determine highest frequency
  highestFreq <- max(freqs);
  ### Store the names of the most common category (or categories)
  categoryVector <- names(freqs[freqs==highestFreq]);
  ### Now, we need to supply this back in the same class as the original.
  if ("factor" %in% originalClass) {
    categoryVector <- as.factor(categoryVector);
  }
  else {
    suppressWarnings(class(categoryVector) <- originalClass);
  }
  return(categoryVector);
}
