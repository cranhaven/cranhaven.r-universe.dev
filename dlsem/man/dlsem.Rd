\name{dlsem}
\alias{dlsem}
\title{Parameter estimation}
\description{Parameter estimation is performed for a distributed-lag linear structural equation model.
A single group factor may be taken into account.}
\usage{dlsem(model.code, group = NULL, time = NULL, exogenous = NULL, data, hac = TRUE,
  gamma.by = 0.05, global.control = NULL, local.control = NULL, log = FALSE,
  diff.options = list(test=NULL, maxdiff=2, ndiff=NULL),
  imput.options = list(tol=0.0001, maxiter=500, maxlag=2, no.imput=NULL), quiet = FALSE)
  }
\arguments{
  \item{model.code}{A list of objects of class \code{formula}, each describing a single regression model. See \code{Details}.}
  \item{group}{The name of the group factor (optional). If \code{NULL}, no groups are considered.}
  \item{time}{The name of the variable indicating the date time (optional). This variable must be either a numeric identificative or a date in format '\%Y/\%m/\%d','\%d/\%m/\%Y', or '\%Y-\%m-\%d'. If \code{time} is \code{NULL} and \code{group} is not \code{NULL}, data are assumed to be temporally ordered within each group.
  If both \code{time} and \code{group} are \code{NULL}, data are assumed to be temporally ordered.}
  \item{exogenous}{The name of exogenous variables (optional). Exogenous variables may be either quantitative or qualitative and must not appear in the model code.}  
  \item{data}{An object of class \code{data.frame} containing data.}
  \item{hac}{Logical. If \code{TRUE}, heteroskedasticity and autocorrelation consistent (HAC) estimation of standard errors by Newey & West (1978) is applied, otherwise OLS standard errors are used. Default is \code{TRUE}.}
  \item{gamma.by}{A real number between 0 and 1 defining the resolution of the grid for the adaptation of gamma lag shapes. Adaptation is more precise with values near 0, but it will also take more time. Default is 0.05.}
  %
  \item{global.control}{A list containing global options for the estimation. The list may consist of any number of components among the following:
    \itemize{
    \item{\code{adapt}: }{a logical value indicating whether adaptation of lag shapes should be performed for all regression models. Default is \code{FALSE};}
    \item{\code{min.gestation}: }{the minimum gestation lag for all covariates. If not provided, it is assumed to be equal to 0. Ignored for a gamma lag shape or if \code{adapt}=\code{FALSE};}
    \item{\code{max.gestation}: }{the maximum gestation lag for all covariates. If not provided, it is assumed to be equal to \code{max.lead} (see below). Ignored for a gamma lag shape or if \code{adapt}=\code{FALSE};}
    \item{\code{max.lead}: }{the maximum lead lag for all covariates. If not provided, it is computed accordingly to the sample size. Ignored if \code{adapt}=\code{FALSE}. Note that the lead lag for a gamma lag shape is determined numerically;}
    \item{\code{min.width}: }{the minimum lag width for all covariates. It cannot be greater than \code{max.lead}. If not provided, it is assumed to be 0. Ignored for a gamma lag shape or if \code{adapt}=\code{FALSE};}
    \item{\code{sign}: }{the sign of parameter \eqn{\theta_i} (either '+' for positive or '-' for negative) for all covariates. If not provided, adaptation will disregard the sign of parameter \eqn{\theta_i}. Ignored if \code{adapt}=\code{FALSE}.}
    }}
  %
  \item{local.control}{A list containing variable-specific options for the estimation.
  These options prevail on the ones contained in \code{global.control}. See \code{Details}.}
  %
  \item{log}{Logical or a vector of characters. If a vector of characters is provided, logarithmic transformation is applied to strictly positive quantitative variables with name matching those characters. If \code{TRUE}, logarithmic transformation is applied to all strictly positive quantitative variables. Default is \code{FALSE}.}
  %
  \item{diff.options}{A list containing options for differencing. The list may consist of any number of components among the following: 
    \itemize{
    \item{\code{test}: }{the unit root test to apply, that may be either \code{"kpss"} for KPSS test or \code{"adf"} for ADF test (see \link{unirootTest}). If \code{NULL} (the default), the choice is for the KPSS test if the number of periods is less than 100, otherwise the ADF test is used.}
    \item{\code{maxdiff}: }{the maximum differencing order to apply. If \code{maxdiff}=0, differencing will not be applied. Default is 3;}
    \item{\code{ndiff}: }{the order of differencing to apply (without performing unit root test). If \code{ndiff}=\code{NULL}, differencing will be applied according to unit root test. Default is \code{NULL}.}
    }
  Differencing is applied to remove unit roots, thus avoiding spurious regression (Granger \& Newbold, 1974).
  The same order of differencing will be applied to all quantitative variables. 
  }
  %
  \item{imput.options}{A list containing options for the imputation of missing values through the Expectation-Maximization algorithm (Dempster et al., 1977).
  The list may consist of any number of components among the following:
    \itemize{
    \item{\code{tol}: }{the tolerance threshold. Default is 0.0001;}
    \item{\code{maxiter}: }{the maximum number of iterations. Default is 500. If \code{maxiter}=0, imputation will not be performed;}
    \item{\code{maxlag}: }{The maximum autoregressive order to consider in the imputation. Default is 3.} 
    \item{\code{no.input}: }{the name of variables to which imputation should not be applied.}  
    }
  Only missing values of quantitative variables will be imputed. Qualitative variables cannot contain missing values.
  }
  %
  \item{quiet}{Logical. If \code{TRUE}, messages on the estimation progress are suppressed. Deafult is \code{FALSE}.}
}
\details{
Each regression model in a distributed-lag linear structural equation model has the form:
\deqn{y_t = \beta_0+\sum_{i=1}^p \sum_{l=0}^{L_i} \beta_{i,l} ~ x_{i,t-l}+\epsilon_t
}
where \eqn{y_t} is the value of the response variable at time \eqn{t},
\eqn{x_{i,t-l}} is the value of the \eqn{i}-th covariate at \eqn{l} time lags before \eqn{t},
and \eqn{\epsilon_t} is the random error at time \eqn{t} uncorrelated with the covariates and with \eqn{\epsilon_{t-1}}.
The set \eqn{(\beta_{i,0},\beta_{i,1},\ldots,\beta_{i,L_i})}
is the lag shape of the \eqn{i}-th covariate.
%
Currently available lag shapes are the endpoint-constrained quadratic lag shape:
\deqn{\beta_{i,l} = \theta_i \left[-\frac{4}{(b_i-a_i+2)^2} l^2+\frac{4(a_i+b_i)}{(b_i-a_i+2)^2} l-\frac{4(a_i-1)(b_i+1)}{(b_i-a_i+2)^2} \right] \hspace{1cm} a_i \leq l \leq b_i}
(otherwise, \eqn{\beta_{i,l}=0});
the quadratic decreasing lag shape:
\deqn{\beta_{i,l} = \theta_i \frac{l^2-2(b_i+1)l+(b_i+1)^2}{(b_i-a_i+1)^2} \hspace{1cm} a_i \leq l \leq b_i}
(otherwise, \eqn{\beta_{i,l}=0});
the linearly decreasing lag shape:
\deqn{\beta_{i,l} = \theta_i \frac{b_i+1-l}{b_i+1-a_i} \hspace{1cm} a_i \leq l \leq b_i}
(otherwise, \eqn{\beta_{i,l}=0});
the gamma lag shape:
\deqn{\beta_{i,l} = \theta_i (l+1)^\frac{a_i}{1-a_i}b_i^l \left[\left(\frac{a_i}{(a_i-1)\log(b_i)}\right)^\frac{a_i}{1-a_i}b_i^{\frac{a_i}{(a_i-1)\log(b_i)}-1}\right]^{-1}}
\deqn{0<a_i<1 \hspace{1cm} 0<b_i<1}
%
See Magrini (2020) for details on these constrained lag shapes.

Formulas cannot contain neither qualitative variables or interaction terms (no ':' or '*' symbols), nor functions excepting the following operators for the specification of lag shapes:
  \itemize{
  \item{\code{ecq}: }{quadratic (2nd order polynomial) lag shape with endpoint constraints;}
  \item{\code{qd}: }{quadratic (2nd order polynomial) decreasing lag shape;}
  \item{\code{ld}: }{linearly decreasing lag shape;}
  \item{\code{gam}: }{gamma lag shape.}
  }
Each operator must have the following three arguments (provided within brackets and separated by commas):
  \enumerate{
  \item{the name of the covariate to which the lag is applied;}
  \item{parameter \eqn{a_i};}
  \item{parameter \eqn{b_i};}
  \item{the group factor (optional). If not provided and argument \code{group} is not \code{NULL}, this is found automatically.}
  }

The formula of regression models with no endogenous covariates may be omitted from argument \code{model.code}.
The group factor and exogenous variables must not appear in any formula.

Argument \code{local.control} must be a named list containing one or more among the following components:
  \itemize{
  \item{\code{adapt}: }{a named vector of logical values, where each component must have
the name of one endogenous variable and indicate whether adaptation of lag shapes should be
performed for the regression model of that variable.}
  \item{\code{min.gestation}: }{a named list. Each component of the list must have
the name of one endogenous variable and be a named vector.
Each component of the named vector must have the name of one covariate in the regression
model of the endogenous variable above and include the minimum gestation lag for its lag shape.}
  \item{\code{max.gestation}: }{the same as \code{min.gestation}, with the exception that
the named vector must include the maximum gestation lag.}
  \item{\code{max.lead}: }{the same as \code{min.gestation}, with the exception that
the named vector must include the maximum lead lag.}
  \item{\code{min.width}: }{the same as \code{min.gestation}, with the exception that
the named vector must include the minimum lag width.}
  \item{\code{sign}: }{the same as \code{min.gestation}, with the exception that
the named vector must include the lag sign (either \code{'+'} for positive or \code{'-'} for negative).}
  Local control options have no default values, and global ones are applied in their absence.
  If some local control options conflict with global ones, only the former are applied.
  }
}
\value{An object of class \code{dlsem}, with the following components:
  \item{estimate}{A list of objects of class \code{lm}, one for each regression model.}
  \item{model.code}{The model code, eventually after adaptation of the lag shapes.}
    \item{call}{A list containing the call for each regression, eventually after adaptation of lag shapes.}
    \item{lag.par}{A list containing the parameters of the lag shapes for each regression, eventually after adaptation.}
  \item{exogenous}{The names of exogenous variables.}
  \item{group}{The name of the group factor. \code{NULL} is returned if \code{group}=\code{NULL}.}
  \item{time}{The name of the variable indicating the date time. \code{NULL} is returned if \code{time}=\code{NULL}.}
  \item{log}{The name of the variables to which logarithmic transformation was applied.}
  \item{ndiff}{The order of differencing applied to the time series.}
  \item{data}{The data after eventual logarithmic transformation and differencing, which were actually used to estimate the model.}
  \item{data.orig}{The dataset provided to argument \code{data}.}
  \item{fitted}{The fitted values.}
  \item{residuals}{The residuals.}
  \item{autocorr}{The estimated order of the residual auto-correlation for each regression model.}
  \item{hac}{Logical indicating whether HAC estimation of standard errors is applied.}
  \item{adaptation}{Variable-specific options used for the adaptation of lag shapes.}
S3 methods available for class \code{dlsem} are:
  \item{print}{provides essential information on the model.}
  \item{summary}{shows summaries of estimation.}
  \item{plot}{displays the directed acyclic graph (DAG) of the model including only the endogenous variables.
  Option \code{conf} controls the confidence level (default is 0.95), while
  option \code{style} controls the style of the plot:
    \itemize{
    \item{\code{style=2} (the default): }{each edge is coloured with respect to the sign of the estimated causal effect (green: positive, red: negative, light grey: not statistically significant)};
    \item {\code{style=1}: }{edges with statistically significant causal effect are shown in black, otherwise they are shown in light grey};
    \item {\code{style=0}: }{all edges are shown in black disregarding statistical significance of causal effects}.
    }}
  \item{nobs}{returns the number of observations for each regression model.}
  \item{npar}{returns the number of parameters for each regression model.}
  \item{coef}{returns the estimates of scale parameters for each regression model.}
  \item{confint}{returns the confidence intervals of scale parameters for each regression model. Argument \code{level} controls the confidence level (default is 0.95).}
  \item{vcov}{returns the covariance matrix of estimates for each regression model.}
  \item{logLik}{returns the log-likelihood for each regression model.}
  \item{fitted}{returns fitted values.}
  \item{residuals}{returns residuals.}
  \item{predict}{returns predicted values. Optionally, a data frame from which to predict may be provided to argument \code{newdata}.
  }
}
\references{
A. P. Dempster, N. M. Laird, and D. B. Rubin (1977). Maximum likelihood from incomplete data via the EM algorithm. \emph{Journal of the Royal Statistical Society}, Series B, 39(1): 1-38.
  
C. W. J. Granger, and P. Newbold (1974). Spurious regressions in econometrics. \emph{Journal of Econometrics}, 2(2): 111-120.
  
A. Magrini (2019). A family of theory-based lag shapes for distributed-lag linear regression. To be appeared on \emph{Italian Journal of Applied Statistics}.

W. K. Newey, and K. D. West (1978). A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance matrix. \emph{Econometrica}, 55(3), 703-708.
}
\seealso{\link{unirootTest}; \link{causalEff}; \link{compareModels}.}
\examples{
data(industry)

# Estimation without adaptation of lag shapes
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",time="Year",exogenous=c("Population","GDP"),
  data=industry,log=TRUE)

# Adaptation of lag shapes (estimation takes some seconds more)
indus.global <- list(adapt=TRUE,max.gestation=5,max.lead=15,min.width=3,sign="+")
## NOT RUN:
# indus.mod <- dlsem(indus.code,group="Region",time="Year",exogenous=c("Population","GDP"),
#   data=industry,global.control=indus.global,log=TRUE)

# Summary of estimation
summary(indus.mod)$endogenous

# DAG with edges coloured according to the sign
plot(indus.mod)

# DAG disregarding statistical significance
plot(indus.mod,style=0)  


### Comparison among alternative models

# Model 2: quadratic decreasing lag shapes
indus.code_2 <- list(
  Job ~ 1,
  Consum~qd(Job),
  Pollution~qd(Job)+qd(Consum)
  )
## NOT RUN:
# indus.mod_2 <- dlsem(indus.code_2,group="Region",time="Year",exogenous=c("Population","GDP"),
#   data=industry,global.control=indus.global,log=TRUE)

# Model 3: linearly decreasing lag shapes
indus.code_3 <- list(
  Job ~ 1,
  Consum~ld(Job),
  Pollution~ld(Job)+ld(Consum)
  )
## NOT RUN:
# indus.mod_3 <- dlsem(indus.code_3,group="Region",time="Year",exogenous=c("Population","GDP"),
#   data=industry,global.control=indus.global,log=TRUE)

# Model 4: gamma lag shapes
indus.code_4 <- list(
  Job ~ 1,
  Consum~gam(Job),
  Pollution~gam(Job)+gam(Consum)
  )
## NOT RUN:
# indus.mod_4 <- dlsem(indus.code_4,group="Region",time="Year",exogenous=c("Population","GDP"),
#   data=industry,global.control=indus.global,log=TRUE)

# comparison of the three models
## NOT RUN:
# compareModels(list(indus.mod,indus.mod_2,indus.mod_3,indus.mod_4))


### A more complex model

data(agres)

# Qualitative exogenous variable
agres$POLICY <- factor(1*(agres$YEAR>=2005))
levels(agres$POLICY) <- c("no","yes")

# Causal levels
context.var <- c("GDP","EMPL_AGR","UAA","PATENT_OTHER","POLICY")
investment.var <- c("GBAORD_AGR","BERD_AGR")
research.var <- c("RD_EDU_AGR","PATENT_AGR")
impact.var <-  c("GVA_AGR","PPI_AGR")
agres.var <- c(context.var,investment.var,research.var,impact.var)

# Constraints on lag shapes
agres.global <- list(adapt=TRUE,max.gestation=5,max.lead=15,sign="+")
agres.local <- list(
  sign=list(
    PPI_AGR=c(GBAORD_AGR="-",BERD_AGR="-",RD_EDU_AGR="-",PATENT_AGR="-")
    )
  )

# Endpoint-constrained quadratic lag shapes (estimation takes a couple of minutes)
auxcode <- c(paste(investment.var,"~1",sep=""),
  paste(research.var,"~",paste("ecq(",investment.var,",,)",
    collapse="+",sep=""),sep=""),
  paste(impact.var,"~",paste("ecq(",c(investment.var,research.var),",,)",
    collapse="+",sep=""),sep=""))
agres.code <- list()
for(i in 1:length(auxcode)) {
  agres.code[[i]] <- formula(auxcode[i])
  }
## NOT RUN:
# agres.mod <- dlsem(agres.code,group="COUNTRY",time="YEAR",exogenous=context.var,
#   data=agres,global.control=agres.global,local.control=agres.local,log=TRUE)
# summary(agres.mod)$endogenous

## Gamma lag shapes (estimation takes some minutes)
auxcode_2 <- c(paste(investment.var,"~1",sep=""),
  paste(research.var,"~",paste("gam(",investment.var,",,)",
    collapse="+",sep=""),sep=""),
  paste(impact.var,"~",paste("gam(",c(investment.var,research.var),",,)",
    collapse="+",sep=""),sep=""))
agres.code_2 <- list()
for(i in 1:length(auxcode_2)) {
  agres.code_2[[i]] <- formula(auxcode_2[i])
  }
## NOT RUN:
# agres.mod_2 <- dlsem(agres.code_2,group="COUNTRY",time="YEAR",exogenous=context.var,
#  data=agres,global.control=agres.global,local.control=agres.local,log=TRUE)
# summary(agres.mod_2)$endogenous
# compareModels(list(agres.mod,agres.mod_2))
}
