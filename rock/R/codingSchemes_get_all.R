#' Convenience function to get a list of all available coding schemes
#'
#' @return A list of all available coding schemes
#' @export
#'
#' @examples rock::codingSchemes_get_all();
codingSchemes_get_all <- function() {

  return(
    list(
      codingScheme_peterson = codingScheme_peterson,
      codingScheme_levine = codingScheme_levine,
      codingScheme_willis =codingScheme_willis
    )
  );

}
