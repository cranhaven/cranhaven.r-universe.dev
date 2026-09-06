\name{gamlssNP}
\alias{gamlssNP}

\title{A function to fit finite mixtures using the gamlss family of distributions}
\description{
This function will fit a finite (or normal) mixture distribution where the kernel distribution can belong to 
any gamlss family of distributions using the EM algorithm.
The function is based  on functions \code{alldist()} and \code{allvc} of the \code{npmlreg} package of 
Jochen Einbeck, John Hinde and Ross Darnell. 
}
\usage{
gamlssNP(formula, random = ~1, family = NO(), data = NULL, K = 4, 
          mixture = c("np", "gq"), 
          tol = 0.5, weights, pluginz, control = NP.control(...), 
          g.control = gamlss.control(trace = FALSE, ...), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula defining the response and the fixed effects for the \code{mu} parameters}
  \item{random}{a formula defining the random part of the model}
  \item{family}{a gamlss family object }
  \item{data}{the data frame which for this function is mandatory even if it the data are attached}
  \item{K}{the number of mass points/integretion points (supported values are 1:10,20)  }
  \item{mixture}{the mixing distribution, "np" for non-parametric or "gq" for Gaussian Quadrature}
  \item{tol}{the toletance scalar ussualy between zero and one}
  \item{weights}{prior weights}
  \item{pluginz}{optional   }
  \item{control}{this sets the control parameters for the EM iterations algorithm.
               The default setting is the \code{NP.control} function }
  \item{g.control}{the gamlss control function, \code{gamlss.control}, passed to the gamlss fit}
 \item{\dots}{for extra arguments}    
} 

\details{ 
The function \code{gamlssNP()} is a modification of the R functions 
\code{alldist()} and \code{allvc} created by Jochen Einbeck and John Hinde. 
Both functions were originally created by Ross Darnell (2002). Here the two 
functions are merged to one \code{gamlssNP} and allows finite mixture from 
gamlss family of distributions.
  
The following are comments from the original Einbeck and  Hinde
documentation. 

"The nonparametric maximum likelihood (NPML) approach was 
introduced in Aitkin (1996) as a tool to fit overdispersed 
generalized linear models. Aitkin (1999) extended this method to 
generalized linear models with shared random effects arising through 
variance component or repeated measures structure. Applications are 
two-stage sample designs, when firstly the primary sampling units 
(the upper-level units, e.g. classes) and then the secondary 
sampling units (lower-level units, e.g. students) are selected, or 
longitudinal data. 
Models of this type have also been referred to as 
multi-level models (Goldstein, 2003). This R function is restricted 
to 2-level models. The idea of NPML is to approximate the unknown 
and unspecified distribution of the random effect by a discrete 
mixture of k exponential family densities, leading to a simple 
expression of the marginal likelihood, which can then be maximized 
using a standard EM algorithm. When option 'gq' is set, then 
Gauss-Hermite masses and mass points are used and considered as 
fixed, otherwise they serve as starting points for the EM algorithm. 
The position of the starting points can be concentrated or extended 
by setting tol smaller or larger than one, respectively. Variance 
component models with random coefficients (Aitkin, Hinde & Francis, 
2005, p. 491) are also possible, in this case the option 
random.distribution is restricted to the setting 'np' . The weights 
have to be understood as frequency weights, i.e. setting all weights 
equal to 2 will duplicate each data point and hence double the 
disparity and deviance. Warning: There might be some options and 
circumstances which had not been tested and where the weights do not 
work."
Note that in keeping with the gamlss notation disparity is called global deviance. 
 } 


\value{
The function \code{gamlssNP} produces an object of class "gamlssNP". 
This object contain several components. 
 \item{family}{the name of the gamlss family}
 \item{type}{the type of distribution which in this case is "Mixture" }
 \item{parameters}{the parameters for the kernel gamlss family distribution}
 \item{call}{the call of the gamlssNP function}
 \item{y}{the response variable}
 \item{bd}{the binomial demominator, only for BI and BB models}
 \item{control}{the NP.control settings}
 \item{weights}{the vector of weights of te expanded fit}
 \item{G.deviance}{the global deviance} 
 \item{N}{the number of observations in the fit} 
 \item{rqres}{a function to calculate the normalized (randomized) quantile
          residuals of the object (here is the gamlss object rather than gamlssNP and it should change??)} 
 \item{iter}{the number of external iterations in the last gamlss fitting (?? do we need this?)} 
 \item{type}{the type of the distribution or the response variable here set to "Mixture"} 
 \item{method}{which algorithm is used for the gamlss fit, RS(), CG() or mixed()} 
 \item{contrasts}{the type of contrasts use in the fit} 
 \item{converged}{whether the gamlss fit has  converged} 
 \item{residuals}{ the normalized (randomized) quantile residuals of the model} 
 \item{mu.fv}{the fitted values of the extended mu model, also  sigma.fv, nu.fv,
          tau.fv for the other parameters if present}
 \item{mu.lp}{the linear predictor of the extended mu model, also  sigma.lp, nu.lp,
          tau.lp for the other parameters if present} 
 \item{mu.wv}{the working variable of the extended mu model, also  sigma.wv, nu.wv,
          tau.wv for the other parameters if present}
 \item{mu.wt}{the working weights of the mu model, also  sigma.wt, nu.wt,
          tau.wt for the other parameters if present} 
 \item{mu.link}{the link function for the mu model, also  sigma.link,
          nu.link, tau.link for the other parameters if present} 
 \item{mu.terms}{the terms for the mu model, also  sigma.terms, nu.terms,
          tau.terms for the other parameters if present} 
 \item{mu.x}{the design matrix for the mu, also  sigma.x, nu.x, tau.x for
          the other parameters if present}
 \item{mu.qr}{the QR decomposition of the mu model, also sigma.qr, nu.qr,
          tau.qr for the other parameters if present} 
 \item{mu.coefficients}{the linear coefficients of the mu model, also 
          sigma.coefficients, nu.coefficients, tau.coefficients for the
          other parameters if present}
 \item{mu.formula}{the formula for the mu model, also  sigma.formula,
          nu.formula, tau.formula for the other parameters if present} 
 \item{mu.df}{the mu degrees of freedom also  sigma.df, nu.df, tau.df for
          the other parameters if present}
 \item{mu.nl.df}{the non linear degrees of freedom, also sigma.nl.df,
          nu.nl.df, tau.nl.df for the other parameters if present}
 \item{df.fit}{the total degrees of freedom use by the model} 
 \item{df.residual}{the residual degrees of freedom left after the model is
          fitted}        
 \item{data}{the original data set} 
 \item{EMiter}{the number of EM iterations}
 \item{EMconverged}{whether the EM has converged} 
 \item{allresiduals}{the residuas for the long fit}
 \item{mass.points}{the estimates mass point (if "np" mixture is used)} 
 \item{K}{the number of mass points used}
 \item{post.prob}{contains a matrix of posteriori probabilities,} 
 \item{prob}{the estimated mixture probalilities}
 \item{aic}{the Akaike information criterion }
 \item{sbc}{the Bayesian information criterion} 
 \item{formula}{the formula used in the expanded fit}
 \item{random}{the random effect formula}
 \item{pweights}{prior weights} 
 \item{ebp}{the Empirical Bayes Predictions (Aitkin, 1996b) on the scale of the
linear predictor}
                        
Note that in case of Gaussian quadrature, the coefficient given at 'z' in coefficients corresponds
to the standard deviation of the mixing distribution. 

As a by-product, gamlssNP produces a plot showing the global deviance against the iteration number. 
Further, a plot with the EM trajectories is given. 
The x-axis corresponds to the iteration number, and the y-axis to the value of the mass points at a particular iteration. 
This plot is not produced when mixture is set to "gq"

}
\references{ 

Aitkin, M. and Francis, B. (1995). Fitting overdispersed generalized linear models by 
nonparametric maximum likelihood. GLIM Newsletter 25 , 37-45.

Aitkin, M. (1996a). A general maximum likelihood analysis of overdispersion in generalized linear models. 
                  Statistics and Computing 6 , 251-262.
                  
Aitkin, M. (1996b). Empirical Bayes shrinkage using posterior random effect means from nonparametric maximum 
                likelihood estimation in general random effect models. Statistical Modelling: 
                Proceedings of the 11th IWSM 1996 , 87-94.
                
Aitkin, M., Francis, B. and Hinde, J. (2005) Statistical Modelling in GLIM 4. Second Edition, 
              Oxford Statistical Science Series, Oxford, UK.
              
Einbeck, J. & Hinde, J. (2005). A note on NPML estimation for exponential family regression models 
 with unspecified dispersion parameter. Technical Report IRL-GLWY-2005-04, National University of Ireland, Galway.

Einbeck, J. Darnell R. and  Hinde J. (2006) npmlreg: Nonparametric maximum likelihood estimation for random effect
models, R package version 0.34

Hinde, J. (1982). Compound Poisson regression models. Lecture Notes in Statistics 14 ,109-121. 

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos based on function created by Jochen Einbeck John Hinde and Ross Darnell}

\seealso{\code{\link[gamlss]{gamlss}}, \code{\link[gamlss.dist]{gamlss.family}}}
\examples{
data(enzyme)
# equivalent model using gamlssNP
mmNP1 <- gamlssNP(act~1, data=enzyme, random=~1,family=NO, K=2)
mmNP2 <- gamlssNP(act~1, data=enzyme, random=~1, sigma.fo=~MASS, family=NO, K=2)
AIC(mmNP1, mmNP2)
}
\keyword{regression}
