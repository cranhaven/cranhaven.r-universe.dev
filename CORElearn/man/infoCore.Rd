\name{infoCore}
\alias{infoCore}

\title{ Description of certain CORElearn parameters}
\description{
 Depending on parameter \code{what} the function prints some information on CORElearn,
 for example codes of available classification (or regression) attribute evaluation heuristics.
 For more complete description of the parameters see \code{\link{helpCore}}.
  }
\usage{
infoCore(what=c("attrEval","attrEvalReg"))
}

\arguments{
\item{what}{Selects the info to be printed.}
 }

\details{
   Depending on the parameter \code{what} the function some information on CORElearn.
    \describe{
    \item{"attrEval"}{Prints codes of all available classification attribute evaluation heuristics. 
              These codes can be used as parameters for attribute evaluation methods in learning. 
              It is internally used for validation of parameters. For more complete 
              information see \code{\link{attrEval}}. }
    \item{"attrEvalReg"}{prints codes of all available regression attribute evaluation heuristics. These codes can be used as parameters for attribute evaluation 
   methods in learning. It is internally used for validation of parameters. For more complete 
              information see \code{\link{attrEval}}.}
   }
}
\value{
  For \code{what="attrEval"} or \code{"attrEvalReg"} function returns vector of codes for all 
  implemented classification or regression attribute evaluation heuristics, respectively. 
}

\author{ Marko Robnik-Sikonja}

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{attrEval}},
\code{\link{helpCore}},
\code{\link{CoreModel}}.
}
\examples{
estClass <- infoCore(what="attrEval")
print(estClass)
infoCore(what="attrEvalReg")

}

