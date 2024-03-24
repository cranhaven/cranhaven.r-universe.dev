#' Format Binomial
#'
#' Inline formula formatting of columns representing binomial summary statistics
#' 
#' @param prop column name representing a proportion
#' @param r column name representing number of binomial events observed
#' @param n column name representing number of observations
#' @param digits.p digits for proportion presentation. see format
#' @param nsmall.p nsmall for proportion presentation.
#' @param digits.n number of digits for n presentation
#' @export
Fb <-
function (prop=NULL, r=NULL, n=NULL, digits.p=4, nsmall.p=2, digits.n=6)
  {

    if (!is.null(n) & !is.null(r))
      {f <- paste(format(round(100*r/n, 2), digits=digits.p, nsmall=nsmall.p), "% (", format(r, digits=digits.n, nsmall=0), ")", sep="")}
    else if (!is.null(prop) & !is.null(r)) 
      {f <- paste(format(round(100*prop, 2), digits=digits.p, nsmall=nsmall.p), "% (", format(r, digits=digits.n, nsmall=0), ")", sep="")}
    else
      {{f <- paste(format(round(100*prop, 2), digits=digits.p, nsmall=nsmall.p), "% ", sep="")}}
    f
  }

#' Format Continuous
#'
#' Inline formula formatting of columns representing summary statistics for Continuous data
#' 
#' @param mn column name representing a mean
#' @param std column name representing standard deviation (Variance, Margin of Error, etc.)
#' @param digits see format
#' @param nsmall see format
#' @param NAmiss present NA as white space
#' @export
Fc <-
function(mn, std, digits=2, nsmall=2, NAmiss=TRUE)
{
  f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(std, digits), digits=digits, nsmall=nsmall), ")", sep="")
  if (NAmiss) { f[is.na(mn)] <- ""}
  f
}

#' Format Interval
#'
#' Inline formula formatting of columns representing lower and upper bound of an interval where the results are of the form 'mn (cil, ciu)'
#'
#' @param cil column name representing lower interval
#' @param ciu column name representing upper interval
#' @param mn column name representing a mean
#' @param digits see format
#' @param nsmall see format
#' @param NAmiss present NA as white space
#' @export
Fci <-
function(cil, ciu, mn=NA, digits=2, nsmall=2, NAmiss=TRUE)
{
  if (is.na(mn)) {f <- paste("(", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  else{f <- paste(format(round(mn, digits), digits=digits, nsmall=nsmall), " (", format(round(cil, digits), digits=digits, nsmall=nsmall), ", ", format(round(ciu, digits), digits=digits, nsmall=nsmall), ")", sep="")}
  if (NAmiss) { f[is.na(cil)] <- ""}
  f
}

#' Format Round
#'
#' Abreviation of rouding function
#'
#' @param x column name
#' @param digits see format
#' @param nsmall see format
#' @param NAmiss present NA as white space
#' @export
Fr <- function(x, digits=2, nsmall=2, NAmiss=TRUE)
{
  f <- format(round(x,digits), digits=digits, nsmall=nsmall)
  if (NAmiss) { f[is.na(x)] <- ""}
  f
}
