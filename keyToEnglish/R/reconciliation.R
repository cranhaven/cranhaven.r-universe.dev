#' Reconcile Misspellings
#'
#' Fixes incorrect spellings from previous versions of this package
#' @export
#'
#' @param x `character` vector of phrases from `keyToEnglish()` output or related function
#' @param from `character` version number of how the hash was originally generated
#' @param to `character` version that hash should be converted to
#'
#' @return `character` vector of fixed phrase hash compatible with output for version `to`
reconcile_misspellings <- function(
  x,
  from='0.2.0',
  to='0.2.1'
){
  if (from == '0.2.0'){
    x = gsub('([Ll])eoprine','\\1eporine', x)
    x = gsub('LEOPRINE','LEPORINE',x)
 }

  return(x)
}