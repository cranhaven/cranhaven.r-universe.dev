\name{F.fit.table}
\alias{F.fit.table}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{ F.fit.table - Produce a summary table of model fit statistics. }

\description{
    A utility function to compile a table of fit statistics from a list of MRA fitted objects contained in the 
    .GlobalEnv (i.e., 'working') environment.  The table produced by this routine 
    contains model name, fit statistics (AICc or QAICc), and is ranked by
    (sorted by) one of these fit statistics.  
}

\usage{
F.fit.table( fits=ls(pattern="^fit"), rank.by= "qaicc", plausible.p=0.01 )
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{fits}{A character vector of MRA fitted object names to include in the 
    summary table. These names do not need to have a common root name.  The default value 
    will use any object whose name starts with "fit" in the working directory (.GlobalEnv).
    An an example, if fitted objects are named 
    "fit1.01", "fit1.02", and "fit1.03", \code{fits} should equal \code{c("fit1.01", "fit1.02", "fit1.03")}, 
    or \code{ls(pat="^fit1")}, assuming no other objects in the working directory start with "fit1".}
  \item{rank.by}{A string (scalar) naming the model fit statistic to include
    in the summary table.  The resulting table is sorted by this statistic. Possible values 
    are: "qaicc" (the default), and "aicc".}
  \item{plausible.p}{A scalar specifying the cut-point in \code{rank.by} weight to use during 
    determination of 'plausible' models. A model is defined to be 'plausible' if it has \code{rank.by} 
    weight greater than \code{plausible.p} OR if 
    the model's log-likelihood is greater than the minimum log likelihood amongst those that 
    have \code{rank.by} weight greater than \code{plausible.p}.  See explanation of \code{plausible} 
    in Value section below.    
  }
}

\details{
    A rudimentary check for convergence is done on each fitted model.  If this routine believes
    a model did not converge, the model is included in the table, but the model's fit 
    statistics are set to \code{Inf}. The test for whether a model converged is 
    \code{(fit$exit.code == 1) & (fit$cov.code == 0) & (fit$df > 0)}, where \code{fit} is 
    the fitted object.

    Fitted objects are pulled from the \code{.GlobalEnv} environment.  Usually, this is the 
    current working directory.  Currently, there is no way to pull fits from another environment, but 
    a savvy R programmer could modify the \code{where} argument of the \code{get} command embedded in 
    this routine.

}

\value{A data frame, sorted by \code{rank.by}, with the following columns
  \item{model.num}{Model number assigned by this routine, equal to the position of the 
    model in the input list of fits.}
  \item{model.name}{Name of the fitted object.}
  \item{converged}{Logical values indicating whether this routine thinks the model 
    converged or not.  Value is TRUE if the this routine thinks the model converged, 
    FALSE otherwise. }
  \item{n.est.parameters}{Number of estimable parameters in the model.  This is MRA's guess at the number 
    of estimable parameters in the model, not length of the coefficient vector.}
  \item{n.coefficients}{Number of coefficients in the model.  This is length of the coefficient 
    vector without regard to number of estimable parameters.  If \code{n.coefficients} > \code{n.est.parameters}, 
    the model is not full rank, and at least one coefficient is probably not estimable.}
  \item{loglike}{value of the log likelihood evaluated at the maximum likelihood parameters.}  
  \item{aicc}{AIC of the model including the small sample correction = 
    AIC + (2*\code{df}*(\code{df}+1)) / (\code{nan} - \code{df} - 1)}
  \item{delta.aicc}{Difference between AICc for the model and the minimum AICc in the table.}  
  \item{aicc.wgt}{AICc model weights.  These weights equal exp(-.5*(delta.aicc)), scaled to sum to 1.0,}
  \item{qaicc}{QAIC of the model including the small sample correction = 
    QAIC + (2*\code{df}*(\code{df}+1))/(\code{nan} - \code{df} - 1)}
  \item{delta.qaicc}{Difference between QAICc for the model and the minimum QAICc in the table.}
  \item{qaicc.wgt}{QAICc model weights.  These weights equal exp(-.5*(delta.qaicc)), scaled to sum to 1.0,}
  \item{plausible}{Indicates `plausible' models as defined by Bromaghin et al. (2013).  The value 
    in this column is TRUE if the model has \code{rank.by} weight greater than \code{plausible.p} OR if 
    the model's log-likelihood is greater than the minimum log likelihood amongst those that 
    have \code{rank.by} weight greater than \code{plausible.p}. The second condition in this scheme includes a model structure as 'plausible' 
    when its log-likelihood is relatively high but it has been heavily penalized by the number of parameters.  When 
    the likelihood is parameterized to contain two or more linear models, this second condition is a reasonable 
    criterion when model selection is done in a step-wise fashion on each model separately (see Bromaghin et al., 2013). 
    }

}

\references{ Bromaghin, J.F., McDonald, T. L., and Amstrup, S. C., (2013) "Plausible Combinations: An 
    improved methods to evaluate the covariate structure of Cormack-Jolly-Seber mark-recapture models", 
    Open Journal of Ecology, v.3, p. 11-22.  (included in vignettes)
}

\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }


\seealso{ \code{\link{F.cjs.estim}}, \code{\link{F.huggins.estim}}, \code{vignette("Bromaghin_etal_2013_OJE")} }

\examples{

## Fit several CJS model to dipper data. Summarize fits.

## Time varying survival and capture (true CJS model)
data(dipper.histories)
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))
attr(ct,"nan")<-nrow(dipper.histories)
dipr.01 <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), dipper.histories )

## Linear trend in survival
cT <- 1:ncol(dipper.histories)
dipr.02 <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(cT, nan=nrow(dipper.histories)), 
  dipper.histories )

## No trend in survival
dipr.03 <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~1, dipper.histories )

## Summary table
F.fit.table( ls(pat="^dipr") )


}

\keyword{ survival }% at least one, from doc/KEYWORDS

\keyword{ models }% __ONLY ONE__ keyword per line
