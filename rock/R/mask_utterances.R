#' @rdname masking_sources
#'
#' @examples ### Mask text but not the codes
#' rock::mask_utterances(
#'   paste0(
#'     "Lorem ipsum dolor sit amet, consectetur adipiscing ",
#'     "elit. [[expAttitude_expectation_73dnt5z1>earplugsFeelUnpleasant]]"
#'   )
#' )
#'
#' @export
mask_utterances <- function(input,
                            proportionToMask = 1,
                            maskRegex = "[[:alnum:]]",
                            maskChar = "X",
                            perl = TRUE) {

  if (!(is.character(input))) {
    stop("As argument `input`, pass a character vector.");
  }

  if (!is.numeric(proportionToMask) | (proportionToMask <= 0) |
      (proportionToMask > 1)) {
    stop("As argument `proportionToMask`, pass a number between 0 ",
         "and 1.");
  }

  ### Select the indices from the input vector that we will mask
  indicesToMask <- sample(seq_along(input),
                          size = ceiling(proportionToMask * length(input)),
                          replace = FALSE);
  res <- input[indicesToMask];

  ### Get regular expressions used for codes, identifiers, section breaks and UIDs
  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  ciiRegexes <- rock::opts$defaults$classInstanceRegex;
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);

  ### Get objects with starting points and lengths of matches
  codeMatches <- gregexpr(paste(codeRegexes, collapse="|"), res, perl=perl);
  #idMatches <- gregexpr(paste(idRegexes, collapse="|"), res, perl=perl);
  ciiMatches <- gregexpr(paste(ciiRegexes, collapse="|"), res, perl=perl);
  sectionMatches <- gregexpr(paste(sectionRegexes, collapse="|"), res, perl=perl);
  uidMatches <- gregexpr(paste(uidRegex, collapse="|"), res, perl=perl);

  ### Store the matching substrings
  codeSubStrings <- regmatches(res, codeMatches);
  #idSubStrings <- regmatches(res, idMatches);
  ciiSubStrings <- regmatches(res, ciiMatches);
  sectionSubStrings <- regmatches(res, sectionMatches);
  uidSubStrings <- regmatches(res, uidMatches);

  ### Replace all characters to mask with the masking character
  res <- gsub(maskRegex, maskChar, res);

  ### Replace the matches substrings again
  regmatches(res, codeMatches) <- codeSubStrings;
  #regmatches(res, idMatches) <- idSubStrings;
  regmatches(res, ciiMatches) <- ciiSubStrings;
  regmatches(res, sectionMatches) <- sectionSubStrings;
  regmatches(res, uidMatches) <- uidSubStrings;

  ### Replace these in the original input vector
  input[indicesToMask] <- res;

  ### Return result
  return(input);

}
