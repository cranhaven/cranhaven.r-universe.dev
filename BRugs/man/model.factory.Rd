\name{modelFactory}
\alias{modelEnable}
\alias{modelDisable}
\title{Enable and disable factories to create updaters}
\description{These functions enable and disable factories that create
  updaters. Currently only supported on Windows, not Linux.
  Linux support should be available in the next OpenBUGS release after
  version 3.2.1.}
\usage{
modelEnable(factory)
modelDisable(factory)
}
\arguments{
  \item{factory}{Character (length 1) name of the factory to be
    disabled/enabled, for example \code{"conjugate gamma"}.

    %%See \url{https://www.openbugs.net/Manuals/ModelMenu.html#Updateroptions}

    for more information.  A list of the currently-used updaters
    in a compiled model is given by \code{\link{infoUpdatersbyName}} or
    \code{\link{infoUpdatersbyDepth}}.

    After enabling or disabling an updater, the model must be compiled
    or re-compiled.
  }
}
\examples{\dontrun{
modelDisable("conjugate gamma")
}}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{factory}
