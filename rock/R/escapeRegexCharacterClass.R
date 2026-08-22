escapeRegexCharacterClass <- function(x) {
  x <- gsub("[", "\\[", x, fixed=TRUE);
  x <- gsub("]", "\\]", x, fixed=TRUE);
  return(x);
}
