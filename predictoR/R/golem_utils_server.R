#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' dropNulls(list(1, NULL, 2))
dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv   <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

########################################################

load("inst/app/lang/translation.bin")
translation <- append(translation, loadeR::translation.loadeR())

tr <- function(text, idioma = "es") {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[idioma]]), s,
                   translation[[s]][[idioma]])
    Encoding(elem) <- "utf8"
    
    elem
  }, USE.NAMES = F)
}

# Obtiene los nombres de columnas o regresa un string vacio
colnames.empty <- function(res) {
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}
# 
# # Función para generar diccionario.
# crear.traslation <- function() {
#    library(plyr)
#    archivo <- read.table("inst/app/lang/diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#    translation <- dlply(archivo , .(key), function(s) key = as.list(s))
# 
#    save(translation, file = "inst/app/lang/translation.bin")
#  }
