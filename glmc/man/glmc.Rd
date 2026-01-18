\name{glmc}
\alias{glmc}
\title{Fitting Generalised Linear Models Subject to Population Constraints}
%\alias{glmc}
%\alias{glmc.control}
%\alias{weights.glmc}
%\alias{print.glmc}
\concept{regression}
\concept{logistic}
\concept{log-linear}
\concept{loglinear}
\description{
  \code{glmc} is used to fit generalised linear models
  where the parameters are subject to population constraints.
  The model is specified by giving a symbolic description of
  the linear predictor, a description of the error distribution,
  and a matrix of constraints on the parameters.
}
\usage{
glmc(formula,family = gaussian, data, na.action, 
     start = NULL,etastart, mustart, offset,control = glmc.control(\dots),
     model = TRUE,glm.method= "glm.fit",optim.method="Nelder-Mead",
     emplik.method="Owen",optim.hessian=FALSE,x = FALSE, y = TRUE,
     Amat=NULL, confn=NULL,\dots) 

%glmc(formula, family = gaussian, data, weights = NULL, subset = NULL,
%    na.action, start = NULL, etastart = NULL, mustart = NULL,
%    offset = NULL, control = glmc.control(\dots), model = TRUE,
%    method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL,
%    Amat=NULL,confn=NULL
%    \dots)

%glmc.fit(x, y, weights = rep(1, nobs),
%        start = NULL, etastart = NULL, mustart = NULL,
%        offset = rep(0, nobs), family = gaussian(),
%        control = glmc.control(), intercept = TRUE,
%        Amat=NULL)

%\method{weights}{glmc}(object, type = c("prior", "working"), \dots)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{family}{a description of the error distribution and link
    function to be used in the model. This can be a character string
    naming a family function, a family function or the result of a call
    to a family function.  (See \code{\link{family}} for details of
    family functions.)}

  \item{data}{an optional data frame containing the variables
    in the model.  By default the variables are taken from
    \code{environment(formula)}, typically the environment from which
    \code{glmc} is called. All variables named \code{constraints} is automatically assumed to be the value of the population constraints.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.}

  \item{start}{starting values for the parameters in the linear predictor.}

  \item{etastart}{starting values for the linear predictor.}

  \item{mustart}{starting values for the vector of means.}

  \item{offset}{this can be used to specify an \emph{a priori}
    known component to be included in the linear predictor
    during fitting.}

  \item{control}{a list of parameters for controlling the fitting
    process.  See the documentation for \code{\link{glmc.control}}
    for details.}

  \item{model}{a logical value indicating whether \emph{model frame}
    should be included as a component of the returned value.}

  \item{glm.method}{the method to be used in fitting the model.
    The default method \code{"glm.fit"} uses iteratively re-weighted
    least squares (IWLS).  The only current alternative is
    \code{"model.frame"} which returns the model frame and does no fitting.}

  \item{emplik.method}{the method used to maximise the empirical likelihood to compute the weights. The default is \code{"Owen"}, due to Art Owen. Current alternative is \code{"CSW"}, due to Chen, Sitter and Wu.}

  \item{optim.method}{the method used to maximise over the parameters. See \code{\link{optim}} for more details.}
  \item{optim.hessian}{Logical. If True returns a numerically calculated Hessian Matrix from the optim step.} 
  \item{x, y}{For \code{glmc}:
    logical values indicating whether the response
    vector and model matrix used in the fitting process
    should be returned as components of the returned value.

    For \code{glmc.fit}: \code{x} is a design matrix of dimension \code{n
      * p}, and \code{y} is a vector of observations of length \code{n}.
  }

%  \item{contrasts}{an optional list. See the \code{contrasts.arg}
%    of \code{model.matrix.default}.}
   
  \item{Amat}{a matrix of population constraints on the parameters.}
             
  \item{confn}{a function returning the value of the population constraints on the parameter.  Allows parameter dependent population constraints.}
 
 % \item{object}{an object inheriting from class \code{"glmc"}.}
 % \item{type}{character, partial matching allowed.  Type of weights to
 %    extract from the fitted model object.}

  %\item{intercept}{logical. Should an intercept be included?}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  A typical predictor has the form \code{response ~ terms} where
  \code{response} is the (numeric) response vector and \code{terms} is a
  series of terms which specifies a linear predictor for \code{response}.

%  For \code{binomial} models the response can also be specified as a
%  \code{\link{factor}} (when the first level denotes failure and all
%  others success) or as a two-column matrix with the columns giving the
%  numbers of successes and failures.  

  A terms specification of the form
  \code{first + second} indicates all the terms in \code{first} together
  with all the terms in \code{second} with duplicates removed.  A specification of the form \code{first:second} indicates the
  the set of terms obtained by taking the interactions of
  all terms in \code{first} with all terms in \code{second}.
  The specification \code{first*second} indicates the \emph{cross}
  of \code{first} and \code{second}.
  This is the same as \code{first + second + first:second}.

  \code{glmc} uses closely follows \code{glm}, uses \code{glm.fit}, \code{optim} and \code{el.test} form the \code{emplik} library. 

%  \code{glmc.fit} and \code{glmc.fit.null} are the workhorse functions:
%  the former calls the latter for a null model (with no intercept).

  If more than one of \code{etastart}, \code{start} and \code{mustart}
  is specified, the first in the list will be used.
}
\value{
  \code{glmc} returns an object of class inheriting from \code{"glmc"}
  which inherits from the class \code{"lm"}. See later in this section.

%  The function \code{\link{summary}} (i.e., \code{\link{summary.glmc}}) can
%  be used to obtain or print a summary of the results and the function
%  \code{\link{anova}} (i.e., \code{\link{anova.glmc}})
%  to produce an analysis of variance table.

  The generic accessor functions \code{\link{coefficients}},
  \code{effects}, \code{fitted.values} and \code{residuals} can be used to
  extract various useful features of the value returned by \code{glmc}.

%  \code{weights} extracts a vector of weights, one for each case in the fit (after subsetting and \code{na.action}).

  An object of class \code{"glmc"} is a list containing at least the
  following components:

  \item{coefficients}{a named vector of coefficients}
  \item{residuals}{the \emph{working} residuals, that is the residuals
    in the final iteration of the IWLS fit.}
  \item{fitted.values}{the fitted mean values, obtained by transforming
    the linear predictors by the inverse of the link function.}
  \item{rank}{the numeric rank of the fitted linear model.}
  \item{family}{the \code{\link{family}} object used.}
  \item{linear.predictors}{the linear fit on link scale.}
  \item{deviance}{up to a constant, minus twice the maximised
    log-likelihood.  Where sensible, the constant is chosen so that a
    saturated model has deviance zero.}
  \item{aic}{Akaike's \emph{An Information Criterion}, minus twice the
    maximised log-likelihood plus twice the number of coefficients (so
    assuming that the dispersion is known.)}
  \item{null.deviance}{The deviance for the null model, comparable with
    \code{deviance}. The null model will include the offset, and an
    intercept if there is one in the model}
  \item{iter}{the number of iterations of IWLS used.}
  \item{weights}{the \emph{working} weights, that is the weights
    in the final iteration of the IWLS fit from the glm step (if there is one).}
  \item{final.weights}{the weights maximising the empirical likelihood}
  \item{df.residual}{the residual degrees of freedom.}
  \item{df.null}{the residual degrees of freedom for the null model.}
  \item{y}{the \code{y} vector used. (It is a vector even for a binomial
    model.)}
  \item{converged}{logical. Was the IWLS algorithm judged to have converged?}
  \item{boundary}{logical. Is the fitted value on the boundary of the
    attainable values?}
  \item{call}{the matched call.}
  \item{formula}{the formula supplied.}
  \item{terms}{the \code{\link{terms}} object used.}
  \item{data}{the \code{data argument}.}
  \item{offset}{the offset vector used.}
  \item{control}{the value of the \code{control} argument used.}
  \item{glm.method}{the name of the fitter function used in the final \code{glm} call, in \R always
    \code{"glm.fit"}.}
  \item{emplik.method}{the name of the method used to maximise the empirical likelihood.}
  \item{optim.method}{the name of the method supplied to the optim function for the outer maximisation over the parameters.}.
%  \item{contrasts}{(where relevant) the contrasts used.}
  \item{xlevels}{(where relevant) a record of the levels of the factors
    used in fitting.}

  In addition, non-empty fits will have components \code{qr}, \code{R}
  and \code{effects} relating to the final weighted linear fit.

  Objects of class \code{"glmc"} are normally of class \code{c("glmc","glm",
    "lm")}, that is inherit from class \code{"lm"}, and well-designed
  methods for class \code{"lm"} will be applied to the weighted linear
  model at the final iteration of IWLS.  However, care is needed, as
  extractor functions for class \code{"glmc"} such as
  \code{\link{residuals}} and \code{weights} do \bold{not} just pick out
  the component of the fit with the same name.  In-fact no \code{"type"} specification in \code{weights} retirns the weights which maximises the empirical likelihood.  A call of wieghts with \code{type="prior"} returns a vector of 1 of length equaling the number of odsevations. 

%  If a \code{\link{binomial}} \code{glmc} model is specified by giving a
%  two-column response, the weights returned by \code{prior.weights} are
%  the total numbers of cases (factored by the supplied case weights) and
%  the component \code{y} of the result is the proportion of successes.
}
\seealso{
 % \code{\link{anova.glmc}}, \code{\link{summary.glmc}}, etc. for
  \code{glmc} methods,
  and the generic functions \code{\link{anova}}, \code{\link{summary}},
  \code{\link{effects}}, \code{\link{fitted.values}},
  and \code{\link{residuals}}. \code{\link{optim}}, \code{\link[emplik]{el.test}}, the fitting procedure used by \code{\link{glmc}}. Further, \code{\link{lm}} for non-generalised \emph{linear} models.

%  \code{\link{esoph}}, \code{\link{infert}} and
%  \code{\link{predict.glmc}} have examples of fitting binomial glms.
}
\author{
  The \R implementation of \code{glmc} is written by Sanjay Chaudhuri
  and Mark S Handcock at National University of Singapore and University of Washington, Seattle respectively.
}
\references{
  Owen, A. B.  (2001)
  \emph{Empirical Likelihood.}
  Boca Raton, Fla : Chapman \& Hall/CRC.
 
  Dobson, A. J. (1990)
  \emph{An Introduction to Generalized Linear Models.}
  London: Chapman and Hall.

  Hastie, T. J. and Pregibon, D. (1992)
  \emph{Generalized linear models.}
  Chapter 6 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.

  McCullagh P. and Nelder, J. A. (1989)
  \emph{Generalized Linear Models.}
  London: Chapman and Hall.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  New York: Springer.
}

\examples{
library(glmc)
#Specify the data. 

n <- rbind(c(5903,230),c(5157,350))
mat <- matrix(0,nrow=sum(n),ncol=2)
mat <- rbind(matrix(1,nrow=n[1,1],ncol=1)\%*\%c(0,0),
             matrix(1,nrow=n[1,2],ncol=1)\%*\%c(0,1),
             matrix(1,nrow=n[2,1],ncol=1)\%*\%c(1,0),
             matrix(1,nrow=n[2,2],ncol=1)\%*\%c(1,1))

#Specifying the population constraints.

gfr <- .06179*matrix(1,nrow=nrow(mat),ncol=1)
g <- matrix(1,nrow=nrow(mat),ncol=1)
amat <- matrix(mat[,2]*g-gfr,ncol=1)

# Method 1. Defining constraints in the data frame.

hrh <- data.frame(birth=mat[,2], child=mat[,1], constraints=amat)

gfit <- glmc(birth~child, data=hrh, family="binomial",emplik.method="Owen",
             control=glmc.control(maxit.glm=10,maxit.weights=200,
             itertrace.weights=TRUE,gradtol.weights=10^(-6)))

summary.glmc(gfit)

# Method 2. Defining constraints through a matrix.

gfit<- glmc(mat[,2]~mat[,1],family=binomial(link=logit),
            emplik.method="Owen",control=glmc.control(maxit.glm=10,
            maxit.weights=200,itertrace.weights=TRUE,gradtol.weights=10^(-10)),
            Amat=amat,confn=NULL)

summary.glmc(gfit)

\dontrun{
# Method 3. Defining constraints through a function.

fn <- function(t,Y,X){
grf <- .06179*matrix(1,nrow=nrow(as.matrix(X)),ncol=1)
g <- matrix(1,nrow=nrow(X),ncol=1)
amat <- matrix(Y*g-grf,ncol=1)
return(amat)
}

gfit <- glmc(birth~child,data=hrh,family=binomial(link=logit),
             optim.method="BFGS",emplik.method="Owen",
             control=glmc.control(maxit.glm=10,maxit.optim=10^(8),
             reltol.optim=10^(-10),trace.optim=9,REPORT.optim=1,
             maxit.weights=200,gradtol.weights=10^(-6),itertrace.weights=FALSE),
             optim.hessian=TRUE,Amat=NULL,confn=fn)

summary.glmc(gfit)
    } 
}
\keyword{models}
\keyword{regression}

