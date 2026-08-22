cleanCode <- function(x) {

  codeDelimiters <- rock::opts$get("codeDelimiters");
  inductiveCodingHierarchyMarker <-
    rock::opts$get("inductiveCodingHierarchyMarker");

  x <-
    gsub(
      escapeRegexCharacterClass(codeDelimiters[1]),
      "",
      x
    );

  x <-
    gsub(
      escapeRegexCharacterClass(codeDelimiters[2]),
      "",
      x
    );

  x <- trimws(x);

  x <-
    gsub(
      paste0("^", inductiveCodingHierarchyMarker),
      "",
      x
    );

  x <-
    gsub(
      paste0(inductiveCodingHierarchyMarker, "$"),
      "",
      x
    );

  return(x);

}
