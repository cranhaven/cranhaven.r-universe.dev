\name{ivr}
\alias{ivr}
\title{Two-Stage Least Squares (2SLS) Instrumental Variable Regression}
\usage{
ivr(formula, data = list(), endog, iv, contrasts = NULL, details = FALSE, ...)
}

\arguments{
\item{formula}{model formula.}
\item{data}{name of the data frame used. To be specified if variables are not stored in environment.}
\item{endog}{character vector of endogenous (to be instrumented) regressors.}
\item{iv}{character vector of predetermined/exogenous instrumental variables NOT already included in the model formula.}
\item{contrasts}{an optional list. See the \code{contrasts.arg} of \code{\link[stats]{model.matrix.default}}.}
\item{details}{logical value indicating whether details should be printed out by default.}
\item{...}{further arguments that \code{lm.fit()} understands.}
}

\value{
A list object including:
\tabular{ll}{
\code{adj.r.squ} \tab adjusted coefficient of determination (adj. R-squared).\cr
\code{coefficients} \tab IV-estimators of model parameters.\cr
\code{data/model} \tab matrix of the variables' data used.\cr
\code{data.name} \tab name of the data frame used.\cr
\code{df} \tab degrees of freedom in the model (number of observations minus rank).\cr
\code{exogenous} \tab exogenous regressors.\cr
\code{f.hausman} \tab exogeneity test: F-value for simultaneous significance of all instrument parameters. If H0: "Instruments are exogenous" is rejected, usage of IV-regression can be justified against OLS.\cr
\code{f.instr} \tab weak instrument test: F-value for significance of instrument parameter in first stage of 2SLS regression. If H0: "Instrument is weak" is rejected, instruments are usually considered sufficiently strong.\cr
\code{fitted.values} \tab fitted values of the IV-regression.\cr
\code{fsd} \tab first stage diagnostics (weakness of instruments).\cr
\code{has.const} \tab logical value indicating whether model has a constant (internal purposes).\cr
\code{instrumented} \tab name of instrumented regressors.\cr
\code{instruments} \tab name of instruments.\cr
\code{model.matrix} \tab the model (design) matrix.\cr
\code{ncoef} \tab integer, giving the rank of the model (number of coefficients estimated).\cr
\code{nobs} \tab number of observations.\cr
\code{p.hausman} \tab according p-value of exogeneity test.\cr
\code{p.instr} \tab according p-value of weak instruments test.\cr
\code{p.values} \tab vector of p-values of single parameter significance tests.\cr
\code{r.squ} \tab coefficient of determination (R-squared).\cr
\code{residuals} \tab residuals in the IV-regression.\cr
\code{response} \tab the endogenous (response) variable.\cr
\code{shea} \tab Shea's partial R-squared quantifying the ability to explain the endogenous regressors.\cr
\code{sig.squ} \tab estimated error variance (sigma-squared).\cr
\code{ssr} \tab sum of squared residuals.\cr
\code{std.err} \tab vector of standard errors of the parameter estimators.\cr
\code{t.values} \tab vector of t-values of single parameter significance tests.\cr
\code{ucov} \tab the (unscaled) variance-covariance matrix of the model's estimators.\cr
\code{vcov} \tab the (scaled) variance-covariance matrix of the model's estimators.\cr
\code{modform} \tab the model's regression R-formula.\cr
}
}
\description{
Performs a two-stage least squares regression on a single equation including endogenous regressors Y and exogenous regressors X on the right hand-side. Note that by specifying the set of endogenous regressors Y by \code{endog} the set of remaining regressors X are assumed to be exogenous and therefore automatically considered as part of the instrument in the first stage of the 2SLS. These variables are not to be specified in the \code{iv} argument. Here only instrumental variables outside the equation under consideration are specified.
}
\examples{
## Numerical Illustration 20.1 in Auer (2023)
ivr(contr ~ score, endog = "score", iv = "contrprev", data = data.insurance, details = TRUE)

## Replicating an example of Ani Katchova (econometric academy)
## (https://www.youtube.com/watch?v=lm3UvcDa2Hc)
## on U.S. Women's Labor-Force Participation (data from Wooldridge 2013)
library(wooldridge)
data(mroz)

# Select only working women
mroz = mroz[mroz$"inlf" == 1,]
mroz = mroz[, c("lwage", "educ", "exper", "expersq", "fatheduc", "motheduc")]
attach(mroz)

# Regular ols of lwage on educ, where educ is suspected to be endogenous
# hence estimators are biased
ols(lwage ~ educ, data = mroz)

# Manual calculation of ols coeff
Sxy(educ, lwage)/Sxy(educ)

# Manual calculation of iv regression coeff
# with fatheduc as instrument for educ
Sxy(fatheduc, lwage)/Sxy(fatheduc, educ)

# Calculation with 2SLS
educ_hat = ols(educ ~ fatheduc)$fitted
ols(lwage ~ educ_hat)

# Verify that educ_hat is completely determined by values of fatheduc
head(cbind(educ,fatheduc,educ_hat), 10)

# Calculation with ivr()
ivr(lwage ~ educ, endog = "educ", iv = "fatheduc", data = mroz, details = TRUE)

# Multiple regression model with 1 endogenous regressor (educ)
# and two exogenous regressors (exper, expersq)

# Biased ols estimation
ols(lwage ~ educ + exper + expersq, data = mroz)

# Unbiased 2SLS estimation with fatheduc and motheduc as instruments
# for the endogenous regressor educ
ivr(lwage ~ educ + exper + expersq,
    endog = "educ", iv = c("fatheduc", "motheduc"),
    data = mroz)

# Manual 2SLS
# First stage: Regress endog. regressor on all exogen. regressors
# and instruments -> get exogenous part of educ
stage1.mod = ols(educ ~ exper + expersq + fatheduc + motheduc)
educ_hat = stage1.mod$fitted

# Second stage: Replace endog regressor with predicted value educ_hat
# See the uncorrected standard errors!
stage2.mod = ols(lwage ~ educ_hat + exper + expersq, data = mroz)

## Simple test for endogeneity of educ:
## Include endogenous part of educ into model and see if it is signif.
## (is signif. at 10% level)
uhat = ols(educ ~ exper + expersq + fatheduc + motheduc)$resid
ols(lwage ~ educ + exper + expersq + uhat)
detach(mroz)

}
\references{
Auer, L.v. (2023): Ökonometrie - Eine Einführung, 8th ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).

Wooldridge, J.M. (2013): Introductory Econometrics: A Modern Approach, 5th Edition, Cengage Learning, Datasets available for download at \href{https://www.cengage.com/cgi-wadsworth/course_products_wp.pl?fid=M20b&product_isbn_issn=9781111531041}{Cengage Learning}
}
\concept{endogeneity}
\concept{instrument variable}
