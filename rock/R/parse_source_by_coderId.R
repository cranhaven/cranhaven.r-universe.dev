#' Parsing sources separately for each coder
#'
#' @param input For `parse_source_by_coderId`, either a character vector
#' containing the text of the relevant source *or* a path to a file that
#' contains the source text; for `parse_sources_by_coderId`, a path to a
#' directory that contains the sources to parse.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @rdname parsing_sources_by_coderId
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source_by_coderId(exampleFile);
#'
#' @export
parse_source_by_coderId <- function(input,
                                    ignoreOddDelimiters=FALSE,
                                    postponeDeductiveTreeBuilding = TRUE,
                                    rlWarn = rock::opts$get(rlWarn),
                                    encoding="UTF-8",
                                    silent=TRUE) {

  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);
  autoGenerateIds <- rock::opts$get(autoGenerateIds);
  ### Obsolete now all class instance identifiers are persistent
  # persistentIds <- rock::opts$get(persistentIds);
  noCodes <- rock::opts$get(noCodes);
  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);
  attributeContainers <- rock::opts$get(attributeContainers);
  codesContainers <- rock::opts$get(codesContainers);
  delimiterRegEx <- rock::opts$get(delimiterRegEx);
  ignoreRegex <- rock::opts$get(ignoreRegex);
  coderId <- rock::opts$get(coderId);
  idForOmittedCoderIds <- rock::opts$get(idForOmittedCoderIds);

  ### Read input, if it's a file
  if (file.exists(input)) {
    x <- readLines(input,
                   encoding=encoding,
                   warn = rlWarn);
  } else {
    x <- input;
  }

  ### Get all coders that coded this source
  codersIdAtLines <- grep(coderId,
                          x);
  matchedCoderIds <- regmatches(x,
                                regexpr(coderId, x));

  ### Set 'idForOmittedCoderIds' for lines without coder
  if ((length(codersIdAtLines) == 0) || (min(codersIdAtLines) > 1)) {
    codersIdAtLines <- c(1,
                         codersIdAtLines);
    matchedCoderIds <- c(idForOmittedCoderIds,
                         matchedCoderIds);
  }

  ### Process matchedCoderIds according to regex
  for (i in seq_along(matchedCoderIds)) {
    matchedCoderIds <-
      gsub(coderId,
           "\\1",
           matchedCoderIds);
  }

  ### Set 'subsource' for each coder
  if (length(codersIdAtLines) == 1) {
    subsources <-
      list(x);
    names(subsources) <- matchedCoderIds[1];
  } else {
    subsources <- list();
    for (i in seq_along(codersIdAtLines)) {
      if (i == length(codersIdAtLines)) {
        subsources[[matchedCoderIds[i]]] <-
          x[codersIdAtLines[i]:length(x)];
      } else {
        subsources[[matchedCoderIds[i]]] <-
          x[codersIdAtLines[i]:(codersIdAtLines[i+1]-1)];
      }
    }
  }

  ### Process each subsource
  parsedSubsources <- list();
  for (i in seq_along(codersIdAtLines)) {
    parsedSubsources[[matchedCoderIds[i]]] <-
      rock::parse_source(text = subsources[[i]],
                         ignoreOddDelimiters=ignoreOddDelimiters,
                         postponeDeductiveTreeBuilding=TRUE,
                         silent=silent);
  }

  res <- list(subsources = subsources,
              parsedSubsources=parsedSubsources);

  return(res);

}
