\name{F.cr.model.avg}
\alias{F.cr.model.avg}

%- NEED an '\alias' for EACH other topic documented here.

\title{ F.cr.model.avg - Model averaging of mark-recapture parameters. }

\description{
    Computes model averaged estimates of survival, capture probability, or population size
    estimates from a set of previously fitted MRA objects.
}

\usage{
F.cr.model.avg( fits=ls(pattern="^fit"), what="survival", fit.stat="qaicc" )
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{fits}{A character vector of MRA fitted object names.  Each will be retrieved from the global environment
    (i.e., .GlobalEnv) using \code{get} and tested to make sure they are MRA fitted objects.  If not, a warning is 
    issued and the object is ignored.  If the object is an MRA model, it is included in
    model averaging. The default value
    will use any object whose name starts with "fit".
    For example, if fitted objects are named
    "fit1.01", "fit1.02", and "fit1.03", \code{fits} equal to \code{c("fit1.01", "fit1.02", "fit1.03")}
    or \code{ls(pat="^fit1")}, will average statistics in these three objects.  }
  \item{what}{A text string naming the parameter to average.  Choices are "survival" (the 
    default), "capture", and "n.hat".  Only the first character is inspected (e.g., "c" is 
    equivalent to "capture"). }  
  \item{fit.stat}{A string (scalar) naming the model fit statistic to use when computing 
    model weights.  Possible values
    are: "qaicc" (the default), "qaic", "aicc", and "aic".}
}

\details{
    Each model is checked for convergence prior to including in the model averaging process.   
    The test for whether a model converged is
    \code{(fit$exit.code == 1) & (fit$cov.code == 0) & (fit$df > 0)}, where \code{fit} is
    the fitted object. If the model did not converge, 
    it is excluded from model averaging.

    Conditional and unconditional variance estimates are computed following Burnham and 
    Anderson 2002 (pages 150 and 162 and surrounding). 
    
    If \code{what} = "n.hat", the returned object is suitable for printing using \code{print.nhat} and plotting using \code{plot.cjs}. 
    If \code{what} = "survival" or "capture", the returned object is unclassed and the user is responsible for printing and plotting.
}

\value{If \code{what} = "survival" or "capture", the return is a list object containing the following components: 

  \item{fit.table}{A data frame, sorted by \code{fit.stat}, containing model names, fit statistics, delta fit statistics, and 
    model averaging weights. }
  \item{\code{s.hat} or \code{p.hat}}{A matrix of size \code{nan} X \code{ns} containing model averaged estimates of survival 
    or capture probability.}
  \item{\code{se.s.hat} or \code{se.p.hat}}{A matrix of size \code{nan} X \code{ns} containing the unconditional (on model selection) estimate of 
    standard error for the corresponding model averaged statistic in \code{s.hat} or \code{p.hat}. Unconditional variances 
    are computed using formulas in Burnham and Anderson (2002, pages 150 and 162)}
  \item{\code{se.s.hat.conditional} or \code{se.p.hat.conditional}}{A matrix of size \code{nan} X \code{ns} containing the conditional estimate of 
    standard error for the corresponding model averaged statistic in \code{s.hat} or \code{p.hat}. These estimates of variance 
    are conditional on model selection. }
  \item{mod.selection.proportion}{A matrix of size \code{nan} X \code{ns} containing the proportion of variance due to model selection uncertainty. 
    Values in this matrix are simply the difference between unconditional variance and conditional variance, divided by the unconditional variance.}
    
  If \code{what} = "n.hat", the return is a list of class "n.hat" containing the following components: 

  \item{fit.table}{A data frame, sorted by \code{fit.stat}, containing model names, fit statistics, delta fit statistics, and 
    model averaging weights. }
  \item{n.hat}{A vector of length \code{ns} containing model averaged estimates of population size.}
  \item{se.n.hat}{A vector of length \code{ns} containing the unconditional (on model selection) estimate of 
    standard error for the corresponding model averaged population size.}
  \item{se.n.hat.conditional}{A vector of length \code{ns} containing the conditional estimate of 
    standard error for the corresponding model averaged population size. }
  \item{mod.selection.proportion}{A vector of length\code{ns} containing the proportion of variance due to model selection uncertainty. 
    Values in this matrix are simply the difference between unconditional variance and conditional variance, divided by the unconditional variance.}
  \item{n.hat.lower}{A vector of length \code{ns} containing lower 95\% confidence limits for the corresponding population size estimate.}
  \item{n.hat.upper}{A vector of length \code{ns} containing upper 95\% confidence limits for the corresponding population size estimate.}
  \item{nhat.v.meth}{Scalar indicating the type of variance estimate used.  Values are: \code{4} = "(Model averaged Huggins variance)", 
    \code{5} = "(Model averaged Huggins variance + higher terms)", or \code{6} = "(Model averaged McDonald and Amstrup)". See help for 
    \code{F.cjs.estim} for more explanation.}   
    

}



\author{ Original routine by Eric Regehr, US Fish and Wildlife.  
Modified for MRA by Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\references{
    Burnham, K. and D. Anderson (2002) "Model Selection: A practical guide". Cambridge University Press.
}

\seealso{ \code{\link{F.cjs.estim}}, \code{\link{F.huggins.estim}}, \code{\link{F.fit.table}}, 
    \code{\link{plot.cjs}} }

\examples{


## Fit several CJS model to dipper data. Model average survival

## Time varying survival and capture (true CJS model)
data(dipper.histories)
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))
attr(ct,"nan")<-nrow(dipper.histories)
dipper.01 <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), dipper.histories )

## Linear trend in survival
cT <- 1:ncol(dipper.histories)
dipper.02 <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(cT, nan=nrow(dipper.histories)), 
  dipper.histories )

## No trend in survival
dipper.03 <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~1, dipper.histories )

## Model average
mod.avg.surv <- F.cr.model.avg( ls(pat="^dipper.[0-9]"), what="s", fit.stat="aicc" )

mod.avg.n <- F.cr.model.avg( ls(pat="^dipper.[0-9]"), what="n", fit.stat="aicc" )

## Plot
plot(mod.avg.n)



}

\keyword{ survival }% at least one, from doc/KEYWORDS

\keyword{ models }% __ONLY ONE__ keyword per line
