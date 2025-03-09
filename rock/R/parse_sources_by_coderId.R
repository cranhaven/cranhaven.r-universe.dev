#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @param ignoreOddDelimiters If an odd number of YAML delimiters is encountered, whether this
#' should result in an error (`FALSE`) or just be silently ignored (`TRUE`).
#' @param postponeDeductiveTreeBuilding Whether to imediately try to build the deductive
#' tree(s) based on the information in this file (`FALSE`) or whether to skip that. Skipping
#' this is useful if the full tree information is distributed over multiple files (in which case
#' you should probably call `parse_sources` instead of `parse_source`).
#' @param encoding The encoding of the file to read (in `file`).
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#'
#' @return For `rock::parse_source_by_coderId()`, an object of
#' class `rock_parsedSource`; for `rock::parse_sources_by_coderId()`, an
#' object of class `rock_parsedSources`. These objects contain the original
#' source(s) as well as the final data frame with utterances and codes, as
#' well as the code structures.
#'
#' @rdname parsing_sources_by_coderId
#' @export
parse_sources_by_coderId <- function(input,
                                     recursive = TRUE,
                                     filenameRegex = ".*",
                                     ignoreOddDelimiters=FALSE,
                                     postponeDeductiveTreeBuilding = TRUE,
                                     encoding=rock::opts$get(encoding),
                                     silent=rock::opts$get(silent)) {

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

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

  res <- list();
    res <-
      lapply(rawSourceFiles,
             parse_source_by_coderId,
             ignoreOddDelimiters=ignoreOddDelimiters,
             postponeDeductiveTreeBuilding=FALSE,
             encoding=encoding,
             silent=silent);

  names(res) <-
    unlist(lapply(rawSourceFiles,
                  basename));

  return(res);

}
