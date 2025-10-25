#' iconv x
#' 
#' @inheritParams moranajp_all
#' @param x        A string vector or a tibble.
#' @param reverse  A logical.
#' @return         A string vector.
#' 
#' @export
iconv_x <- function(x, iconv = "", reverse = FALSE){
    if(iconv == ""){
        return(x)
    }else{
        encodings <- stringr::str_split(iconv, "_", simplify = TRUE)
        if(reverse){
            x <- iconv(x, from = encodings[2], to = encodings[1])
        }else{
            x <- iconv(x, from = encodings[1], to = encodings[2])
        }
        return(x)
    }
}

#' Generate code like "stringi::stri_unescape_unicode(...)"
#' 
#' @param x        A string or vector of Japanese
#' @return         A string or vector 
#' @examples
#' stringi::stri_unescape_unicode("\\u8868\\u5c64\\u5f62") |>
#'   print() |>
#'   escape_japanese()
#' 
#' @export
escape_japanese <- function(x){
  escaped <- stringi::stri_escape_unicode(x)
  codes <- paste0('stringi::stri_unescape_unicode("', escaped, '")')
  for(code in codes){
    message(code, "\n")
  }
}

#' Wrapper functions for escape and unescape unicode
#' 
#' @param x    A dataframe or character vector
#' @return     A dataframe or character vector
#' @examples
#' data(review_mecab)
#' review_mecab |>
#'   print() |>
#'   unescape_utf() |>
#'   print() |>
#'   escape_utf()
#' 
#' @export
unescape_utf <- function(x){
  if(is.character(x)){
    x <- stringi::stri_unescape_unicode(x)
    return(x)
  }
  if(is.data.frame(x)){
    colnames(x) <- stringi::stri_unescape_unicode(colnames(x))
    x <- dplyr::mutate_if(x, is.character, stringi::stri_unescape_unicode)
    return(x)
  }
}

#' @rdname unescape_utf
#' @export
escape_utf <- function(x){
  if(is.character(x)){
    x <- stringi::stri_escape_unicode(x)
    return(x)
  }
  if(is.data.frame(x)){
    colnames(x) <- stringi::stri_escape_unicode(colnames(x))
    x <- dplyr::mutate_if(x, is.character, stringi::stri_escape_unicode)
    return(x)
  }
}
