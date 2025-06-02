#' @title 
#' Convert object to function
#' 
#' @description 
#' \code{as.fun} is a generic function that does the same as \code{as.function} 
#' from package \pkg{base}, with the additional feature that 
#' \code{as.fun.character} converts a string into the function it names. 
#' 
#' @param x
#' The object to convert.
#' 
#' @param envir
#' Environment in which the function should be defined. 
#' 
#' @param ...
#' Additional arguments (currently not used). 
#' 
#' @return 
#' The desired function. 
#' 
#' @author 
#' \code{as.fun.character} is adapted from MrFlick, 
#' see \url{https://stackoverflow.com/a/38984214} on StackOverflow. 
#' 
#' @export
#'
#' @examples 
#' as.fun(mean)
#' as.fun("mean")
#' as.fun("edit")
#' as.fun("stats::predict")
#' 
#' ## the constant function '1'
#' f <- as.fun(1)
#' f(2)   # 1
#' f("a") # 1
#' 
#' ## the constant function 'FALSE'
#' f <- as.fun(FALSE)
#' f(2)   # FALSE
#' f("a") # FALSE
#' 
#' f <- as.fun(data.frame(x = 1:2, y = 2:3))
#' f("x") # 'x' column
#' f("y") # 'y' column
#' 
as.fun <- 
function(x, 
         ...)
{
  UseMethod("as.fun")  
}


#' @export
#' @rdname as.fun
#' 
as.fun.default <- 
function(x, 
         envir = parent.frame(),
         ...)
{
  if (is.null(envir)) envir <- baseenv()
  as.function.default(x, envir = envir, ...)
}


#' @export
#' @rdname as.fun
#' 
as.fun.character <-
function(x,
         ...)
{
  if (grepl(":::", x)) {
    ns <- strsplit(x, ":::")[[1L]]
    x <- ns[2L]
    w <- 1L
  } else if (grepl("::", x)) {
    ns <- strsplit(x, "::")[[1L]]
    x <- ns[2L]
    w <- 1L
  } else {
    ns <- .packages(all.available = FALSE) #unlist(sessionPackages(), use.names = FALSE)
    w <- which(vapply(ns, 
                      FUN = function(n) { x %in% getNamespaceExports(n) }, 
                      FUN.VALUE = logical(1L)))
    if (length(w) > 1L) 
      stop(paste0("several packages export '", x, "', please use ::"))
    if (length(w) == 0L) 
      stop(paste0("'", x, "' is not exported by the packages currently loaded"))
  }
  f = getExportedValue(ns[w], x)
  #formals(f) = rlist::list.merge(formals(f), nlist(...))
  structure(f, fun = x, package = ns[w])
}


#' @export
#' @rdname as.fun
#' 
as.fun.name <- 
function(x, 
         ...)
{
  as.fun(as.character(x), ...)
}


#' @export
#' @rdname as.fun
#' 
as.fun.call <- 
function(x, 
         ...)
{
  as.fun(as.character(x[[1L]]), ...)
}


#' @export
#' @rdname as.fun
#' 
as.fun.numeric <-
function(x,
         ...)
{
  function(...) { x }
}


#' @export
#' @rdname as.fun
#' 
as.fun.logical <- as.fun.numeric


#' @export
#' @rdname as.fun
#' 
as.fun.factor <- as.fun.numeric


#' @export
#' @rdname as.fun
#' 
as.fun.complex <- as.fun.numeric


#' @export
#' @rdname as.fun
#' 
as.fun.data.frame <- 
function(x, 
         ...)
{
  f <- function(n)
  {
    n <- match.arg(n, names(x))
    x[[n]]
  }
  formals(f) <- list(n = names(x))
  f
}


#' @importFrom stats predict 
#' @export
#' @rdname as.fun
#' 
as.fun.lm <- 
function(x, 
         ...)
{
  ## Name of the X variables 
  ..x <- x
  rm(x)
  ..n <- stats::formula(..x)
  ..n <- all.vars(..n)[-1]
  if ("..x" %in% ..n) {
    stop("the model's formula contains a variable called '..x', 
         'as.fun()' does not work in this specific case")
  }
  if ("..n" %in% ..n) { 
    stop("the model's formula contains a variable called '..n', 
         'as.fun()' does not work in this specific case")
  }
  
  ## Creation of the function to be returned, with no arguments yet
  f <- function()
  {
    df <- as.data.frame(as.list(environment()))
    names(df) <- ..n
    p <- stats::predict(..x, newdata = df, type = default_type(..x), ...)
    if (is.list(p)) {
      if (!is.null(p$fit)) {
        y <- p$fit
      } else if (!is.null(p$pred)) {
        y <- p$pred
      } else {
        stop("cannot find predicted values")
      }
    } else {
      y <- p
    }
    unname(y)
  }

  ## 'l' is the list used to name the arguments of the function 'f()'
  l <- replicate(length(..n), substitute())
  names(l) <- ..n
  formals(f) <- l
  f
}


#' @export
#' @rdname as.fun
#' 
as.fun.rpart <- as.fun.lm
# TODO: essayer de voir si le Y du rpart est un factor... 


#' @importFrom stats as.stepfun
#' @export
#' 
as.fun.isoreg <- 
function(x, 
         ...)
{
  stats::as.stepfun(x, ...)
}


default_type <- 
function(obj)
{
  UseMethod("default_type")
}

default_type.default <- 
function(obj)
{
  "response"
}

default_type.rpart <- 
function(obj)
{
  "vector"
}

default_type.train <- 
function(obj)
{
  "raw"
}
