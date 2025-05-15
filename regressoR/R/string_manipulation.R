# Updates the labelInputs that the language changes
#' exe
#' 
#' @description concat and execute a text in R.
#'
#' @param ... one or more texts to be concatenated and executed.
#' @param envir the environment in which expr is to be evaluated.
#' 
#' @return the result of the execute.
#' @export
#'
#' @examples
#' exe("5+5")
#' exe("5","+","5")
#' exe("plot(iris$Species)")
#' 
exe <- function(..., envir = parent.frame()){
  environ <- envir
  envir <- if(is.null(environ) || !is.environment(environ)) parent.frame() else environ
  eval(parse(text = paste0(...)), envir = environ)
}

#' extract_code
#' 
#' @description gets the code of a function in text form.
#'
#' @param funcion the name of the function to be extracted.
#' @param envir the environment in which expr is to be evaluated.
#'
#' @export
#'
#' @examples
#' extract_code("cat")
#' extract_code("plot")
#' 
#' parse(text = extract_code("plot"))
#' 
extract_code <- function(funcion, envir = parent.frame()) {
  code <- paste(deparse(exe(funcion, envir = envir)), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

#' as_string_c
#' 
#' @description creates a string representative of a vector
#'
#' @param vect a vector with values
#' @param quote a logical value. If TRUE, the values on the vector will be surrounded by quotes.
#' 
#' @export
#' 
#' @examples
#' as_string_c(c("A", "B", "C"))
#' as_string_c(c(5, 6, 7))
#' as_string_c(c(5, 6, 7), quote = FALSE)
#' as_string_c(iris$Species)
#'
as_string_c <- function(vect, quote = TRUE){
  if(quote){
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
  else{
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
}