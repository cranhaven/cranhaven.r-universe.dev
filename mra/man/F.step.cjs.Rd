\name{F.step.cjs}
\alias{F.step.cjs}

\title{F.step.cjs - Stepwise model selection for CJS models.}

\description{Conducts automated stepwise model selection of CJS models.  Selection includes
forward steps followed by backward looks.  Steps consider covariates in both the survival 
and probability of capture models. 
}


\usage{
F.step.cjs(cap.covars, surv.covars, fit.crit = "qaicc", signif.drop = 0, 
  signif.increase = 0, plot = TRUE, ...)
}


\arguments{
  \item{cap.covars}{A character vector containing the names of single covariates or combinations of covariates 
                    to consider in the capture equation. See Details.}
  \item{surv.covars}{A character vector containing the names of single covariates or combinations of covariates 
                    to consider in the survival equation. See Details.}
  \item{fit.crit}{A character scalar specifying the model fit criterion to use during each step. This criterion 
                    will be used to rank variables 
                    during the forward steps and backward looks.  Possible values are "qaicc" (the default), 
                    "aic", "qaic", and "aicc".  During forward (addition) steps, the variable(s) that 
                    decreased \code{fit.crit} the most will be added to the model, assuming that the decrease is greater
                    than or equal to \code{abs(signif.drop)} (see \code{signif.drop} for more). 
                    During backward (elimination) looks, the variable that increases \code{fit.crit} the most will be removed 
                    from the model, assuming 
                    that the increase is greater than or equal to \code{abs(signif.increase)}.  }
  \item{signif.drop}{A scalar specifying the decrease in \code{fit.crit} that should be considered "significant" during 
                    forward steps. 
                    This argument controls stopping and is functionally equivalent to the alpha-to-enter  
                    parameter of classical stepwise routines. Stepwise selection stops
                    when the minimum \code{fit.crit} on the current iteration minus the minimum \code{fit.crit} 
                    on the previous iteration is greater than or equal to \code{signif.drop}. 
                    This means \code{signif.drop} should be less than or equal to 0, unless for some odd reason, 
                    steps that add variables without predictive abilities are desired.
                    If \code{signif.drop} = 0 (the default), steps are halted when no variables decrease \code{fit.crit}.  
                    Thus, the default method yields the minimum \code{fit.crit} model among those considered during 
                    stepwise selection.  If \code{signif.drop} = -2, steps are halted when \code{fit.crit} decreases by less than 
                    2 (or increases) between the 
                    current and previous steps.  }
  \item{signif.increase}{A scalar specifying the increase in \code{fit.crit} that should be is considered 
                    "significant" during backward looks.  This argument controls elimination, 
                    but not stoppage.  A variable in the current model is a candidate for removal if, 
                    upon removal, \code{fit.crit} 
                    increases by less than or equal to \code{signif.increase}.  This means \code{signif.increase}
                    should be greater than or equal to 0 to be meaningful, unless no variables should be removed, 
                    in which case set \code{signif.increase = -Inf} (or some other negative number). If 2 or more 
                    variables are candidates for removal during a single backward look step, the variable which 
                    increases \code{fit.crit} the most will be removed (the other will remain).
                    If \code{signif.increase} = 0 (the default), variables are left in the model if they 
                    increase \code{fit.crit} when removed.  If \code{signif.increase} = 2, variables in the current model 
                    are left in the model if they increase \code{fit.crit} by more than 2 units when removed.}
  \item{plot}{A scalar specifying whether the minimum \code{fit.crit} at the end on each forward-backward step should be plotted. 
                    This plot can be useful for assessing which variable(s) cause relatively "big" and which cause 
                    relatively "little" jumps in \code{fit.crit}.}
  \item{\dots}{Additional arguments to \code{F.cjs.estim}, like histories= , nhat.v.meth=, c.hat=, control=, etc.}
}


\details{

Elements of both the \code{cap.covars} and \code{surv.covars} 
arguments can specify the names of single covariates or sets of 
covariates to consider as a group to be included or excluded 
together. For example, if \code{cap.covars = c("sex", 
"ageclass1 + ageclass2 + ageclass3" )}, the routine will 
include and exclude \code{sex} as a single covariate with 1 
estimated coefficient and \code{ageclass1 + ageclass2 + 
ageclass3} as a set of 3 covariates (with 3 estimated 
coefficients) to be included and excluded as a set.  This is 
useful if factor covariates are pre-coded as indicator variables. In the 
example above, specifying \code{ageclass1 + ageclass2 + 
ageclass3} would make sense if age of an individual was 
classified into 1 of 4 classes, and \code{ageclassX} was a 
matrix with elements equal to 1 if the individual was in age 
class X during a capture occasion, and 0 otherwise.  

Specifying a term like \code{a + 
b + ab} would ensure that main effect matrices (\code{a} 
and \code{b}) are included whenever the interaction matrix 
(\code{ab}) is included during model selection. However, this way 
of including interactions will only be useful if the main effects 
are not considered separately.  That is, specifying 
\code{cap.covars = c("a", "b", "a + b + ab")} will not work 
because the routine does not know that "a" and "b" are 
components of "a + b + ab".  Nonsense models like 
"a + b + ab + a + b" could result. Thus, this routine is 
likely only useful for terms that do not include interactions. 
A useful way to proceed in this case may be to use stepwise 
to select a model of main effects, then consider interactions.  

Time varying and individual varying variables can be specified 
using the \code{tvar} and \code{ivar} functions in the elements 
of \code{cap.covars} and \code{surv.covars}. For example, 
specifying \code{"tvar(year,nan=nrow(ch))"} as an element of 
\code{surv.covars}, where \code{year = 1:ncol(ch)} and 
\code{ch} is the capture history matrix,  will fit a linear 
year effect (1 coefficient) when this element is added during 
stepwise selection.  Likewise, factors are preserved during 
selection. If \code{year} in the above example had been a 
factor (i.e., \code{year = as.factor(1:ncol(ch))}), separate 
effects for each year (\code{ncol(ch) - 1} coefficients) would 
have been fitted when this effect came up for consideration 
during stepwise selection.   

The variable to add or eliminate is selected after all variables 
in both the capture and survival models are considered.  That is, the
variable that decreases (or increases) \code{fit.crit} the most over both 
models is added. 
Selection does not iteratively fix one model and select variables 
in the other.  For example, on one step, the variable that decreases \code{fit.crit}
the most may be a survival covariate, while on the next step
the variable that decreases \code{fit.crit}
the most may be a capture covariate.    

}

\value{The final CJS model resulting from application of stepwise model selection.  This object 
is a valid MRA CJS model and has class 'cjs'.  See help for \code{F.cjs.estim} for a 
description of the components of this object. }

\author{
Trent McDonald, WEST-INC, tmcdonald@west-inc.com
}

\seealso{
\code{\link{F.cjs.estim}}, \code{\link{F.fit.table}}
}

\examples{

#   Aquire data and intermediate variables
data(dipper.histories)
data(dipper.males)
ch <- dipper.histories
males <- dipper.males
ns <- ncol(ch)
nan <- nrow(ch)

#   Construct covariates
small.t <- as.factor( paste("T",1:ns, sep=""))
post.flood <- as.numeric( 1:ns >= 4 )
year <- 1:ns - (ns / 2)
males.postflood <- outer( c(males), post.flood ) # individual and time varying

print(dim(males.postflood))

#   Attach attributes to covariates.  For convienence only.
attr(small.t, "nan") <- nan
attr(small.t, "drop") <- c(1,2)
attr(year, "nan") <- nan
attr(post.flood, "nan") <- nan
attr(males, "ns") <- ns

#   Define pool of variables to be considered in each model
cap.vars <- c("tvar(small.t)","tvar(year)")
surv.vars <- c("tvar(small.t)","tvar(year)", "tvar(post.flood)", "ivar(males)",
  "males.postflood")

#   Do stepwise selection
final.fit <- F.step.cjs( cap.vars, surv.vars, histories=ch, 
  control=mra.control(maxfn=500, cov.meth=2) )


}


\keyword{ survival }
\keyword{ models }

