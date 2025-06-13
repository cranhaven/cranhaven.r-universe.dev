\name{internal-functions}
\alias{formulaLCA}
\alias{regressionStep}
\alias{selBWD}
\alias{selFWD}
\alias{selGA}
\alias{fitnessVarsel}
\alias{monitorGA}
\alias{monitor}

\title{
Internal \code{LCAvarsel} functions
}
\description{
Internal functions not to be called by the user.
}

\usage{
formulaLCA(variables, covariates = NULL)

regressionStep(y, X, ctrlReg = controlReg(), independence = FALSE)

selBWD(X, G, ctrlLCA = controlLCA(), ctrlReg = controlReg(),
      independence = FALSE, swap = FALSE, start = NULL, bicDiff = 0,
      covariates = NULL, parallel = FALSE, checkG = TRUE, 
      verbose = interactive())
       
selFWD(X, G, ctrlLCA = controlLCA(), ctrlReg = controlReg(),
      independence = FALSE, swap = FALSE, start = NULL, bicDiff = 0,
      covariates = NULL, parallel = FALSE, checkG = TRUE, 
      verbose = interactive())

selGA(X, G = 1:3, covariates = NULL,
      ctrlGA = controlGA(), ctrlLCA = controlLCA(), ctrlReg = controlReg(),
      checkG = TRUE, independence = FALSE, start = NULL, parallel = FALSE)
      
fitnessVarsel(roles, par)

monitor(info, iter)

monitorGA(object, digits = getOption("digits"), ...)
}

\keyword{internal}