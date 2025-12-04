\name{secrFit}
\alias{secrFit}


\title{
Spatially Explicit Capture-Recapture
}
\description{
A wrapper for \code{secr::secr.fit}. In \code{secr} v. 4, \code{\link[secr]{secr.fit}} gains a new option, \code{fastproximity}. If TRUE, some data sets are compressed and reconfigured to run much faster. This cannot be implemented for all models. The default is \code{fastproximity=TRUE}. This means that you can have a set of models where some have been reconfigured, others not, and AICs are not comparable across these models. The function \code{secrFit} simply calls \code{secr.fit} with \code{fastproximity = FALSE}, making it easy to run models with consistent settings.
}
\usage{
secrFit (capthist, model = list(D~1, g0~1, sigma~1), mask = NULL, buffer = NULL,
    CL = FALSE, detectfn = NULL, ...)
}
\arguments{
  \item{capthist}{
a \code{capthist} object as defined in package \code{secr} including capture data and detector (trap) layout }
  \item{model}{
list with optional components each symbolically defining a linear predictor for one real parameter using formula notation
}
  \item{mask}{
a mask object or (for a multi-session analysis) a list of mask objects, one for each session
}
  \item{buffer}{
scalar mask buffer radius if mask is not specified (default 100 m)
}
  \item{CL}{
logical, if true then the model is fitted by maximizing the conditional likelihood
}
  \item{detectfn}{
integer code or character string for shape of detection function 0 = halfnormal, 1 = hazard rate etc. - see \code{\link[secr]{detectfn}}
}
  \item{...}{
other arguments to pass to \code{secr.fit}.
}
}
\value{
returns an object of class secr representing the fitted SECR model.
}

\author{
This wrapper by Mike Meredith
}

