\name{aiStaFit}
\alias{aiStaFit}
\alias{print.aiFit}
\title{Fitting a Static AIDS Model}
\description{Estimate a static AIDS model for a system.}

\usage{aiStaFit(y, share, price, expen, shift = NULL, omit = NULL,
  hom = TRUE, sym = TRUE, AR1 = FALSE, rho.sel = c("all", "mean"), ...)
}
\arguments{
  \item{y }{ a multiple time series data.}
  \item{share }{ names of the share variables.}
  \item{price }{ names of the price variables.}
  \item{expen }{ name of the expenditure variables.}
  \item{shift }{ names of the shifter variables.}
  \item{omit }{ name of the share variable omitted; if not supplied, this is the last one of \code{share}.}
  \item{hom }{ a logical value of homogeneity test. }
  \item{sym  }{ a logical value of symmetry test. }
  \item{AR1 }{ whether first-degree autocorrelation should be corrected.}
  \item{rho.sel }{ if \code{AR1 = TRUE}, there are two ways of computing the autocorrelation coefficient.} 
  \item{\dots }{ additional arguments to be passed.}
}
\details{This estimates a static AIDS model. The data supplied should be in the final format. Autocorrelation in the residuals can be corrected following the treatment in Berndt (1975).}

\value{
Return a list object of class "aiFit" and "aiStaFit" with the following components:
  \item{y }{ data for fitting the static AIDS model. }
  \item{share }{ names of the share variables.}
  \item{price }{ names of the price variables.}
  \item{expen }{ name of the expenditure variables.}
  \item{shift }{ names of the shifter variables.}
  \item{omit }{ name of the share variable omitted; if not supplied, this is the last one of \code{share}.}
  \item{nOmit }{ position of the omitted share variable in the name of share variable. }
  \item{hom }{ a logical value of homogeneity test. }
  \item{sym  }{ a logical value of symmetry test. }

  \item{nShare }{ number of share variables. }
  \item{nExoge }{ number of exogenous variables (lagged share, residual, expenditure, and shifters). }
  \item{nParam }{ number of parameters in one equation. }
  \item{nTotal }{ number of parameters in the whole system estimated. }
  \item{formula }{ formula for estimating the system. }
  \item{res.matrix }{ restriction matrix for \code{hom} or \code{sym}, or both. }
  \item{res.rhs }{ right-hand values for tests of \code{hom} or \code{sym}, or both. }
  \item{est }{ the static AIDS model estimated. }
  \item{AR1 }{ a logical value whether autocorrelation is corrected.}  
  \item{call }{ a record of the system call; this allows \code{update.default} to be used. }
}


\section{Methods}{
  One method is defined as follows. This is the print method related to three functions: \code{aiStaFit}, \code{aiDynFit}, and \code{aiStaHau}. 
  \describe{
    \item{\code{print}:}{print the first several observations of selectec outputs.}
  }
}
   
\references{
Berndt, E.R., and N.E. Savin. 1975. Estimation and hypothesis testing in singular equation systems with autoregressive disturbances. Econometrica 43(5/6):937-957.

Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{aiDiag}}; \code{\link{aiElas}}; \code{\link{summary.aiFit}}; \code{\link{aiDynFit}}; \code{\link{aiStaHau}}; \code{\link{systemfitAR}}.}

\examples{
# see the examples for 'aiDynFit'.
}
\keyword{regression}