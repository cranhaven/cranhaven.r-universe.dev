

# ?cli::cli_text()
# The text to show .. will be concatenated into a single string. 
# Newlines are `not` preserved.

#' @title \link[base]{.Defunct} Messages using Package \CRANpkg{cli}
#' 
#' @description
#' Internal helper functions, 
#' to display beautiful \link[base]{.Defunct} messages using package \CRANpkg{cli}.
#' 
#' @param author \link[base]{character} scalar
#' 
#' @param pub \link[base]{character} scalar
#' 
#' @param chapter \link[base]{character} scalar
#' 
#' @param doi \link[base]{character} scalar
#' 
#' @examples
#' cli_RPubs_(pub = 'groupedHyperframe')
#' cli_QuartoPub_(pub = 'groupedhyperframe')
#' cli_QuartoPub_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
#' cli_Netlify_(pub = 'groupedhyperframe')
#' cli_Netlify_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
#' cli_doi_('10.1002/bimj.4710230408')
#' 
#' @keywords internal
#' @name cli_
#' @export
cli_doi_ <- function(doi) {
  # `x`: 'character' scalar of doi
  sprintf(fmt = '{.href [doi:%s](https://doi.org/%s)}', doi, doi) |>
    cli_text()
}


#' @rdname cli_
#' @export
cli_book_ <- function(
    fmt,
    author = 'tingtingzhan', 
    pub, 
    chapter, print_chapter = TRUE
) {    
  if (missing(chapter) || !print_chapter) {
    paste0('{.url ', fmt, '}') |>
      sprintf(fmt = _, author, pub) |>
      cli_text()
  } else {
    paste0('{.url ', fmt, '/%s.html}') |>
      sprintf(fmt = _, author, pub, chapter) |>
      cli_text()
  }
}


#' @rdname cli_
#' @export
cli_QuartoPub_ <- function(...) cli_book_(fmt = 'https://%s.quarto.pub/%s', ...)

#' @rdname cli_
#' @export
cli_Netlify_ <- function(...) cli_book_(fmt = 'https://%s-%s.netlify.app', ...)

#' @rdname cli_
#' @export
cli_RPubs_ <- function(...) cli_book_(fmt = '{.url https://rpubs.com/%s/%s}', ..., print_chapter = FALSE) 



#' @title Defunct Functions
#' 
#' @description
#' The functions mentioned in hard-copy journals, but later \link[base]{.Defunct}.
#' 
#' @param ... Defunct parameters
#' 
#' @keywords internal
#' @name defunct
#' @export
aggregate_quantile <- function(...) {
  
  new. <- '<groupedHyperframe> |> quantile() |> aggregate()'
  
  match.call()[[1L]] |> deparse1() |> 
    sprintf(fmt = '%s()') |>
    col_cyan() |> style_bold() |>
    sprintf(fmt = 'Function %s described in') |> message()
  cli_doi_('10.1093/bioinformatics/btaf430')
  'has been replaced by pipeline' |> message()
  
  new. |>
    col_red() |> style_bold() |>
    message()
  
  'Read vignette (mirrors) for details' |> message()
  cli_QuartoPub_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')
  cli_Netlify_(pub = 'groupedhyperframe', chapter = 'bioinformatics_btaf430')

  .Defunct(new = new.)
  
}






