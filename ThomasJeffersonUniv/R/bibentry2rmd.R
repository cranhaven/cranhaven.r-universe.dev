

#' @title R Markdown Format of \link[utils]{citation} and/or \link[utils]{bibentry}
#' 
#' @description 
#' R markdown format of a \link[utils]{citation} and/or \link[utils]{bibentry} object.
#' 
#' @param x \link[base]{character} scalar, 
#' `'R'` (default) or name of an R package
#' 
#' @details
#' Function [bibentry2rmd] beautifies the output from 
#' function `utils:::format.bibentry` (with option `style = 'text'`)
#' in the following ways.
#' \itemize{
#' \item{Line break `'\n'` is replaced by a white space;}
#' \item{Fancy quotes \eqn{``}, \eqn{''}, \eqn{`} and \eqn{'} are removed;}
#' \item{doi entries are shown as URLs with labels (in R markdown grammar).}
#' }
#' 
#' @returns 
#' Function [bibentry2rmd] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples 
#' bibentry2rmd('survival')
#' if (FALSE) { # disabled for ?devtools::check
#' ap = rownames(installed.packages())
#' lapply(ap, FUN = bibentry2rmd)
#' }
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @importFrom utils citation
#' @export
bibentry2rmd <- function(x = 'R') {
  
  if (length(x) != 1L || !is.character(x)) stop('`x` must be len-1 character')
  if (is.na(x) || !nzchar(x)) stop('`x` must not be missing')
    
  x0 <- switch(x, R = citation(), suppressWarnings(citation(package = x))) # c('citation', 'bibentry')
  
  y <- format(x0, style = 'text') # ?utils:::format.citation -> ?utils:::format.bibentry
  # return may have `length(y) > 1L`
  if (!length(y) || anyNA(y) || any(!nzchar(y))) stop('Package ', sQuote(x), ' updated?')
  
  y <- gsub(pattern = '\n', replacement = ' ', x = y)
  
  # '\u201c|\u201d' # quotation marks created by ?base::dQuote
  # '\u2018|\u2019' # quotation marks created by ?base::sQuote
  y <- gsub(pattern = '\"|\u201c|\u201d|\u2018|\u2019', replacement = '', x = y)
  
  if (TRUE) {
    ptn <- '( doi:)(.*?)( <https://doi.org/)(.*?)(>)(.|,)'
    doi <- stri_extract_all_regex(str = y, pattern = ptn)
    doi_pattern <- c(' doi:', ' <https://doi.org/', '>')
    doi_replacement <- c(' [doi:', '](https://doi.org/', ')')
    y <- unlist(.mapply(FUN = function(y_, doi_) {
      doi_new <- stri_replace_all_fixed(str = doi_, pattern = doi_pattern, replacement = doi_replacement, vectorize_all = FALSE) # to get [topic](url)
      stri_replace_all_regex(str = y_, pattern = ptn, replacement = doi_new)  
    }, dots = list(y_ = y, doi_ = doi), MoreArgs = NULL), use.names = FALSE)
  } # [topic](url) for doi, in R markdown grammar
  
  return(y)
  
}