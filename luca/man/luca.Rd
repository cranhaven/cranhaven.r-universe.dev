\name{luca}
\alias{luca}
\title{ Likelihood-based case-control inference Under Covariate Assumptions (LUCA)}
\description{
In genetic association studies, there is increasing interest in
understanding the joint effects of genetic and
nongenetic factors.
For rare diseases, the case-control study is the
standard design and logistic regression
is the standard method of inference. However, the power to detect
statistical interaction is a concern,
even with relatively large samples. LUCA implements
maximum likelihood inference under
\enumerate{
  \item independence of the genetic factor and nongenetic attributes in 
the control population,
  \item independence of the genetic factor and nongenetic attributes, plus 
Hardy-Weinberg proportions (HWP) in control genotype frequencies, or
  \item simple dependence between the genetic and nongenetic covariates in 
the control population.
}
Maximum likelihood under covariate assumptions offers improved
precision of interaction estimators compared to the standard logistic
regression approach which makes no assumptions on the distribution
of covariates.
}
\usage{
luca(pen.model, gLabel, dat, HWP = FALSE, dep.model = NULL)
}
\arguments{
  \item{pen.model}{ an R formula specifying the disease penetrance model
relating a genetic factor and a number of nongenetic attributes (the
predictors or transformations thereof) to disease status. A typical
\code{pen.model}
has the form \code{d ~ g + a + g:a} where \code{d} is a binary disease
response, \code{g} is a genetic factor, \code{a} is a
(possibly continuous) nongenetic factor and \code{g:a} is the interaction
between the genetic and nongenetic factors.}
%%When \code{a} includes all the terms in \code{a}, the model \code{d ~ g + a + g:a} is equivalent to \code{d ~ g*a}.}
  \item{gLabel}{ a character string specifying the name of the genetic factor in \code{
pen.model}. }
  \item{dat}{ a data frame containing the variables in \code{pen.model},
currently, with \emph{no} default value. Each row of \code{dat} is
considered as one multivariate observation for a subject.  Note that the
genetic term must be a \code{\link{factor}} object, and also needs to
be a \code{\link[genetics]{genotype}} object in some cases (as described
in the following arguments). Currently, the disease response variable must
be \emph{numeric} with values 0 (unaffected) and 1 (affected).
Also, note that missing values are not allowed in the data frame. }
 %- if not found in \code{dat}, the variables are taken from \code{environment(pen.model)} 
  \item{HWP}{ a logical value indicating whether the genotype frequencies
in controls should be assumed to follow Hardy-Weinberg proportions.
When \code{TRUE}, the genetic term must be a \code{\link[genetics]{genotype}}
object.}
  \item{dep.model}{ an R formula specifying the dependence between the
genetic factor and nongenetic attributes. (See the Details section below for
more on the dependence model.) When \code{NULL} (default),
it indicates independence between the genetic factor and nongenetic
attributes in controls. The argument \code{HWP} is ignored for a
\emph{non-null} \code{dep.model}.  The genetic factor must be a
\code{\link[genetics]{genotype}} object when
\code{dep.model} is provided. }
  }

\details{
Inference for association parameters is obtained by fitting a
conditional logistic regression model
with appropriate match-sets comprised of
\dQuote{pseudo-individuals} having all possible values of the genetic
factor and disease status but common value of the nongenetic attribute.
The function \code{\link[survival]{coxph.fit}} from the \code{survival}
package is used to fit the conditional logistic regression.

A dependence model such as \code{g ~ a} specifies a polychotomous 
regression model for the genetic factor \code{g} as a function of the  
nongenetic attribute \code{a}. The polychotomous regression for \code{g} 
given \code{a} holds when the conditional distribution of \code{a} given 
\code{g} is from the exponential family of distributions, with a constant
dispersion parameter across the levels of the genetic factor.  
Alternately, \code{g} and \code{a} may be conditionally independent 
given a third variable \code{a2}. Typically, \code{a2} is also a term in 
the penetrance model (\code{pen.model}). To model conditional independence 
of \code{g} and \code{a} given \code{a2}, specify the dependence model 
(\code{dep.model}) as \code{g ~ a2}. See Shin, McNeney and Graham (2007) 
for details.  \code{luca} also allows dependence models of the form 
\code{g ~ a1 + a2 + ...} for multiple attributes \code{a1}, \code{a2}, ... 
However, there is no formal justification for the use of such a model to capture the 
dependence between \code{g} and multiple nongenetic attributes.
}

%The disease response is typically a vector with two values (e.g., 0 or 1). The higher numeric value, higher factor level or longer character string denotes a case (i.e., affected) by default.  The function \code{\link{I}}() can be used to specify which value represents the case. For example, \code{I(d == case.value)} can replace \code{d} in \code{pen.model}, \code{d ~ g + a + g:a}.  

\value{
An object of class \code{"luca"} with the following components:
  \item{ call }{ the function call }
  \item{ coefficients }{ estimates of parameters in the 
covariate model (lebelled as \code{covmod.XX}) and the penetrance model
(labelled as \code{penmod.YY} where \code{YY} denotes the name of a term
in the model).
The covariate model parameters depend on the covariate assumptions and are
1) control-population log-odds for each level of the genetic
factor relative to a baseline level under independence,
2) control-population log-odds for each allele relative to a baseline allele
under independence plus HWP, or
3) the parameters from the polychotomous regression model under
dependence (see the Details section for
a description of this model).
}
  \item{ var }{the variance-covariance matrix of the parameter estimates. }
%%\item{ loglik }{ a vector of length 2 containing the log-likelihood for the initial values and for the 
%%final values of the coefficients. }
%%  \item{ score }{ value of the efficient score test for the initial values of the coefficients. }
\item{ iter }{ number of iterations in the iterative search for parameter estimates }
%% \item{ linear.predictors }{ the vector of linear predictors, one per each of the \dQuote{pseudo-individual}. }
%%  \item{ residuals }{ the martingale residuals. }
%%  \item{ means }{ the vector of column means of the design matrix. }
%%  \item{ method }{ the computation method used. }

The function \code{\link{summary.luca}} (or \code{summary}) can be used to obtain a summary of the results in a similar style to the \code{lm} and \code{glm} summaries.
}
%\note{ The inferences of model parameters are obtained by fitting a conditional logistic regression model with appropriate match-sets.  This can be easily done using a stratified Cox proportional hazards model with an R function such as \code{\link{coxph}} or \code{\link{clogit}}.   
 %}

\author{Ji-Hyung Shin, Brad McNeney, Jinko Graham}

\references{ Shin J-H, McNeney B, Graham J (2007).
Case-Control Inference of Interaction between Genetic and Nongenetic 
Risk Factors under Assumptions on Their Distribution. 
Statistical Applications in Genetics and Molecular Biology 6(1), 
Article 13. Available at: http://www.bepress.com/sagmb/vol6/iss1/art13.
}

\section{Warning}{Inference is not robust to misspecification
of the covariate assumptions. There should be strong \emph{a priori} evidence
to support any assumptions that are made. Alternately, \code{luca} may be used
to screen for \dQuote{interesting} interactions that are followed up
with logistic regression using data from a larger study.
}

\seealso{ \code{\link{summary.luca}}, \code{\link{glm}}, \code{\link{coxph}}, \code{\link{clogit}} }

\examples{
data(lucaDat)
# typical penetrance model:
pen.model<-formula(d~I(allele.count(g,"C"))+a+a2+I(allele.count(g,"C")):a)

#1. Assuming independence and HWP	
fitHWP<-luca(pen.model=pen.model, gLabel="g", dat=lucaDat, HWP=TRUE)
fitHWP$coef
fitHWP$var
summary.luca(fitHWP) # OR 'summary(fitHWP)'

#2. Assuming independence only
fitDefault<-luca(pen.model=pen.model, gLabel="g", dat=lucaDat)
fitDefault$coef
fitDefault$var

#3. Allowing for dependence between genetic and nongenetic factors

# General dependence model
fitDep1<-luca(pen.model=pen.model, gLabel="g", dat=lucaDat, 
 dep.model=formula(g~a))
fitDep1$coef
fitDep1$var

# When 'g' and 'a' are conditioanally independent given the third variable 'a2':
fitDep2<-luca(pen.model=pen.model, gLabel="g", dat=lucaDat,
 dep.model=formula(g~a2))
fitDep2$coef
fitDep2$var
}

\keyword{methods}
