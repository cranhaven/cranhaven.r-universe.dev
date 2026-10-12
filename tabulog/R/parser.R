#' Parser Objects
#'
#' Create or test for parser objects. These objects will be used by templates to
#' identify a field within a log file.
#'
#' Parser objects contain 3 things:
#' \enumerate{
#'     \item A regex expression that matches the given field
#'     \item A 'formatter'; a function that will in some way modify the captured text
#'     \itemize{
#'         \item By default, this the identity function
#'     }
#'     \item (Optional) A name for the parser
#' }
#'
#' @param x A regex string, a parser, or a list of either; Or object to be tested
#' @param f A function to format the captured output, or a named list of such
#'   functions if \code{x} is a list
#' @param name An optional name for the parser
#' 
#' @return \code{parser} and its S3 methods coerce \code{x} to a \code{parser} object,
#'         returning said parser object. \code{is.parser} returns TRUE or FALSE
#' 
#' @examples
#' # Captures integers
#' parser('[0-9]+')
#'
#' # Captures integers, cast to integers
#' parser('[0-9]+', as.integer)
#'
#' # List of parsers, all named (inferred from list names), some with parsers
#' parser(
#'   list(
#'     ip = '[0-9]{1,3}(\\.[0-9]{1,3}){3}',
#'     int = '[0-9]+',
#'     date = '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}'
#'   ),
#'   list(int = as.integer, date = as.Date)
#' )
#'
#' is.parser(parser('[0-9]+')) #TRUE
#' is.parser(100)              #FALSE
#'
#' @export
parser <- function(x, f, name=NULL){
  UseMethod('parser', x)
}

#' @export
parser.default <- function(x, f = NULL, name=NULL){
  class(x) <- c('parser', 'character')
  if(is.null(f))
    f <- `(`
  formatter(x) <- f
  name(x) <- name
  x
}

#' @export
parser.list <- function(x, f = list(), ...){
  parsers <- lapply(names(x), function(n){
    parser(x[[n]], f[[n]], n)
  })
  names(parsers) <- names(x)
  parsers
}

#' @export
parser.parser <- function(x, f = NULL, name = NULL){
  if(!is.null(f))
    formatter(x) <- f
  name(x) <- name
  x
}

#' @rdname parser
#' @export
is.parser <- function(x){
  inherits(x, 'parser')
}

#' Encode for printing
#'
#' Format a \code{parser} object for printing
#'
#' @param x parser to be formatted
#' @param ... other arguments to be passed to \code{format.character}
#'
#' @examples
#' # No name, default formatter
#' format(parser('[0-9]+'))
#' # Custom name and formatter
#' format(parser('[0-9]+]', as.integer, name='int'))
#'
#' @export
format.parser <- function(x, ...){
  head <- ifelse(is.null(name(x)),
                 "Parser",
                 sprintf("Parser: %s", name(x)))
  sep <- paste0(rep('-', nchar(head)), collapse='')
  body <- sprintf("Matches:\n  %s", x)
  foot <- sprintf("Formatter:\n%s", paste0('  ', format(formatter(x)), collapse='\n'))
  format(sprintf("%s\n%s\n%s\n%s\n", head, sep, body, foot), ...)
}

#' Print
#'
#' Print a \code{parser} object. Underlying method uses \code{cat}.
#'
#' @param x parser to be printed
#' @param ... Other arguments; ignored
#' 
#' @return \code{x}, invisibly
#'
#' @examples
#' # No name, default formatter
#' print(parser('[0-9]+'))
#'
#' #Custom name and formatter
#' print(parser('[0-9]+]', as.integer, name='int'))
#'
#' @export
print.parser <- function(x, ...){
  cat(format(x))
  invisible(x)
}

#' Formatters
#'
#' Get or set the formatter for a parser
#'
#' @param x parser
#' @param value formatter function to be set
#'
#' @examples
#' p <- parser('[0-9]+]')
#'
#' # Default formatter
#' formatter(p)
#'
#' # Set formatter
#' formatter(p) <- as.integer
#'
#' # Custom formatter
#' formatter(p)
#' 
#' @return The formatter attribute (should be a function) for the passed object 
#'         (usually a \code{parser} object)
#'
#' @export
formatter <- function(x){
  UseMethod('formatter', x)
}

#' @export
formatter <- function(x){
  attr(x, 'formatter')
}

#' @rdname formatter
#' @export
`formatter<-` <- function(x, value){
  UseMethod('formatter<-', x)
}

#' @export
`formatter<-.parser` <- function(x, value){
  if(is.null(value))
    value <- `(`
  attr(x, 'formatter') <- value
  x
}

#' Parser Names
#'
#' Get or set the name for a parser
#'
#' @param x parser
#' @param value Name to be set
#' 
#' @return The name attribute (should be a character) for the passed object 
#'         (usually a \code{parser} object)
#'
#' @examples
#' p <- parser('[0-9]+]')
#'
#' # Default name (NULL)
#' name(p)
#'
#' # Set name
#' name(p) <- 'int'
#'
#' # Custom name
#' name(p)
#'
#' @export
name <- function(x){
  UseMethod('name', x)
}

#' @export
name.parser <- function(x){
  attr(x, 'name')
}

#' @rdname name
#' @export
`name<-` <- function(x, value){
  UseMethod('name<-', x)
}

#' @export
`name<-.parser` <- function(x, value){
  attr(x, 'name') <- value
  x
}
