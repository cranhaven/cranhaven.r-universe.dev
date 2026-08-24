
mediaan <- function(vector) {
  if (is.data.frame(vector) | is.matrix(vector)) {
    stop("The first argument is not a vector! If you need to specify ",
         "a variable from a dataframe, separate the name of the ",
         "dataframe and the variable name with a dollar sign, for ",
         "example using 'dat$gender' to extract variable 'gender' from ",
         "dataframe 'dat'.");
  }
  if (is.character(vector)) {
    stop('The first argument is a character vector; please convert it ',
         'to a factor or a numeric vector first.');
  }
  ### Store original class
  originalClass <- class(vector);
  ### Store original vector
  originalVector <- vector;
  ### Convert to numeric vector
  vector <- as.numeric(vector);
  ### If need be, convert to relevant category
  if ("factor" %in% originalClass) {
    levelIndex <- stats::median(vector, na.rm=TRUE);
    if (round(levelIndex) == levelIndex) {
      res <- levels(originalVector)[stats::median(vector, na.rm=TRUE)];
    }
    else {
      res <- c(levels(originalVector)[round(stats::median(vector, na.rm=TRUE)-.5)],
               levels(originalVector)[round(stats::median(vector, na.rm=TRUE)+.5)]);
    }
  }
  else {
    res <- stats::median(vector, na.rm=TRUE);
  }
  return(res);
}
