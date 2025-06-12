#' @name ortresiduals.OEFPIL
#' @title Orthogonal residuals from an OEFPIL object
#' @description Function for calculating orthogonal residuals of an \code{"OEFPIL"} object (i.e. the shortest Euclidean distance between data points and the estimated function from OEFPIL).
#' @usage ortresiduals.OEFPIL(object, min.c)
#'
#' @param object an object of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#' @param min.c a numeric value, for defining minimization interval for the \code{\link{optimize}} function (if not defined, default value 0.05 * range(x) is used). Must be positive.
#'
#' @return Returns an object of type list containing following components
#'
#' \item{x.ores}{a numerical vector of x coordinates of points, where the minimal distance is realized.}
#' \item{o.resid}{a numerical vector of orthogonal residuals (minimal Euclidean distances between data points and estimated function).}
#' \item{SSort}{the orthogonal sum of squares.}
#'
#' @note The value \code{min.c} should not be too small. In that case the minimization interval is too narrow and the result can be misleading (see Example 3).
#'
#' @seealso \code{\link{OEFPIL}}
#'
#' @examples
#' \dontshow{
#' utils::example("coef.OEFPIL",echo=FALSE)}
#' ##-- Continuing the coef.OEFPIL(.) example:
#'
#' ##Example 1 Use ortresiduals.OEFPIL function on the OEFPIL object, with specified value 'min.c'
#' ortresiduals.OEFPIL(st1,5)
#'
#' ##Example 2 Use ortresiduals.OEFPIL function without value 'min.c' (defaut.value = 0.05 * range(x))
#' ortresiduals.OEFPIL(st1)
#'
#' ##Example 3 Choice of too narrow interval. Misleading result!
#'ortresiduals.OEFPIL(st1,0.5)
#' @export

ortresiduals.OEFPIL <- function(object, min.c){
  ## Function for computing orthogonal residuals of an 'OEFPIL' object
  ## input: object ... 'OEFPIL' object
  ##        min.c ... constant for defining minimization interval for the optimize() function
  ##              (if not defined, default value 0.05 * range(x) is used), min.c must be positive
  ## output: x.ores ... x coordinates of points, where the minimal distance is realized
  ##         o.resid ... orthogonal residuals (minimal Euclidean distances between data points
  ##                     and estimated function)
  ##         SSort ... orthogonal sum of squares

  if(!is.list(object)){
    stop("Input has to be a list.")
  }

  l <- dim(object$cov.m_Est)[1] ## number of parameters

  if(!IsListOK(object[1:l])){
    stop("There are NA or NaN in estimated parameter values.")
  }

  x <- object$contents[[3]] ## x-ova data
  y <- object$contents[[4]] ## y-ova data
  xname <- object$contents$idp.var.name

  ## setting value of argument a (if it is not defined in input)
  if(missing(min.c)){
    rngx <- max(x) - min(x)
    min.c <- 0.05 * rngx
    MES <- paste("Argument 'min.c' was not defined, value ", min.c, " was used for the calculation.",
                 sep = "")
    message(MES)
  }

  if(min.c <= 0){
    stop("min.c must be a positive number")
  }


  for (i in 1:l){
    assign(object$contents$names.of.parameters[i],object[[i]])
  } ## assigning estimated parameter values

  ftomin <- NULL
  ## rewriting main function in optimize requested format - ftomin() function
  formstring <- strsplit(object$contents$input.form.string, "~")[[1]][2]
  formstringx <- gsub(xname, "x", formstring)
  eval(parse(text = paste("ftomin <- function(x){", formstringx, "}", sep = "")))

  fceoptim <- function(f, x0, y0, min.c){
    ## input: f - function to minimize
    ##        x0, y0 - coordinates of the data point
    ##        min.c - constant for defining minimization interval
    ## output: classical output for optimize()

    eudist <- function(x){sqrt((x - x0)^2 + (f(x) - y0)^2)}
    optimize(eudist, c(x0 - min.c, x0 + min.c))

  } ## function for finding minimal Euclidean distance from point [x0,y0] to the curve

  ortsumvec <- c()
  xoptvec <- c()
  for(i in 1:length(x)){
    OPT <- fceoptim(ftomin, x[i], y[i], min.c)
    ortsumvec[i] <- OPT$objective ## vector of minimal Euclidean distances (orthogonal residuals)
    xoptvec[i] <- OPT$minimum ## x coordinates of points, where the minimal distance is realized
  }
  return(list(x.ores = xoptvec, o.resid = ortsumvec, SSort = sum(ortsumvec^2)))
}




