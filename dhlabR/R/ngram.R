NGRAM_API <- 'https://api.nb.no/dhlab/nb_ngram/ngram/query'

#' Hidden internal function to get NGRAM
#'
#' @param word The word to get NGRAM for.
#' @param corpus The corpus to use. Options are 'avis' and 'bok'.
#' @param language The language to use. Default is NULL.
#'
#' @return A list that contains the NGRAM.
#'
#' @import httr
#'
#' @keywords internal
get_ngram <- function(word, corpus, language = NULL) {

  if (corpus == "avis" & !is.null(language)) {
    stop("language can not be used with newspapers")
  }

  params <- list("terms" = word, "corpus" = corpus, "lang" = language)
  # result <- GET(NGRAM_API, query=params)
  result <- api_call_wrapper(NGRAM_API, query=params, method = "GET")

  # return(as.data.frame(do.call(rbind, content(result))))
  return (content(result))

}

#' Hidden internal function to convert NGRAM
#'
#' @param ngrams The NGRAMs to convert.
#' @param smooth Smoothing factor. Default is 1.
#' @param years A vector that contains the start and end years. Default is c(1810,2013).
#' @param mode The mode to use. Default is 'relative'.
#'
#' @return A data frame that contains the converted NGRAM.
#'
#' @import dplyr
#' @import tibble
#' @import zoo
#' @importFrom purrr map_df
#'
#'
#' @keywords internal
ngram_conv <- function(ngrams, smooth=1, years=c(1810,2013), mode='relative') {

  ngc <- list()

  if(startsWith(mode, 'rel') || mode=='y') {
    arg <- 'y'
  } else {
    arg <- 'f'
  }

  for(x in ngrams) {
    if(length(x) > 0) {
      temp <- map_df(x$values, function(z) {
        if(z$x <= years[2] && z$x >= years[1]) {
          return(tibble("year" = z$x, "value" = z[[arg]]))
        } else {
          return(NULL)
        }
      })
      if(!is.null(temp)) {
        colnames(temp) <- c('year', x$key)
        ngc[[x$key]] <- temp
      }
    }
  }

  df <- Reduce(function(...) merge(..., by = 'year', all = TRUE), ngc)

  df <- df %>% mutate(across(-"year", ~ zoo::rollapply(., width = smooth, FUN = mean, align = "right", fill = NA)))

  return(df)
}


#' Function to get and convert NGRAM
#'
#' @param word The word to get NGRAM for. Default is "havet".
#' @param corpus The corpus to use. Options are 'avis' and 'bok'. Default is "bok".
#' @param language The language to use. Default is NULL.
#' @param smooth Smoothing factor. Default is 1.
#' @param years A vector that contains the start and end years. Default is c(1810,2013).
#' @param mode The mode to use. Default is 'relative'.
#'
#' @return A data frame that contains the NGRAM.
#'
#' @export
ngram <- function(word="havet", corpus="bok", language=NULL, smooth=1, years=c(1810,2013), mode='relative') {

  result = get_ngram(word=word, corpus=corpus, language = language)
  resultdf = ngram_conv(result, smooth = smooth, years=years, mode=mode)

  return(resultdf)
}

