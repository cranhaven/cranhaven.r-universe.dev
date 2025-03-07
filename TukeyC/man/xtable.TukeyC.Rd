\name{xtable.TukeyC}
\alias{xtable.TukeyC}

\title{
  xtable method for \code{TukeyC} objects.
}

\description{
  Convert an \code{TukeyC} object to an \code{xtable.TukeyC} object, which can then be printed as a LaTeX or HTML table. This function is an additional method to xtable function of xtable package.
}

\usage{

\method{xtable}{TukeyC}(x, ...)

}

\arguments{
  \item{x}{A given object of the class \code{TukeyC}.}
  \item{\dots}{Further arguments (require by xtable::xtable).}
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr  
  Enio G. Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

\seealso{\code{\link{xtable}}
}

\examples{
data(RCBD) 
\dontrun{
  if(require(xtable)){
    lm1 <- with(RCBD,
                  lm(y ~ blk + tra,
                       data=dfm))

    tk1 <-  TukeyC(lm1,
                   which='tra')
    tb <- xtable(tk1)
    print(tb)
  }
 } 
}

\keyword{package}
\keyword{TukeyC}
