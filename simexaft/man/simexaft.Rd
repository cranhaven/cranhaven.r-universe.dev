\name{simexaft}
\alias{simexaft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ SIMEX Algorithm for Accelerated Failure Time Model with Covariates Subject to Measurement Error }
\description{
  Implementation of the SIMEX algorithm for Accelerated Failure Time model with covariates subject to measurement error.
}
\usage{
simexaft(formula = formula(data), data = parent.frame(), 
        SIMEXvariable, repeated = FALSE, repind = list(), 
        err.mat = err.mat, B = 50, lambda = seq(0, 2, 0.1), 
        extrapolation = "quadratic", dist = "weibull")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ specifies the model to be fitted, with the variables coming with data. This argument has the same format as the formula argument in the existing R function "survreg". }
  \item{data}{ optional data frame in which to interpret the varialbes occurring in the formula.  }
  \item{SIMEXvariable}{ the index of the covariate variables that are subject to measurement error. }
  \item{repeated}{ set to TRUE or FALSE to indicate if there are repeated measurements for the mis-measured variables. }
  \item{repind}{ the index of the repeated measurement variables for each mis-measured variable. It has an R list form. If repeated = TRUE, repind must be specify.  }
  \item{err.mat}{ specifies the variables with measurement error, If repeated = FALSE, err.mat must be specify. }
  \item{B}{ the number of simulated samples for the simulation step. The default is set to be 50.  }
  \item{lambda}{ the vector of lambdas, the grids for the extrapolation step.  }
  \item{extrapolation}{ specifies the function form for the extrapolation step. The options are linear, quadratic and both. The default is set to be quadratic.(first 4 letters are enough) }
  \item{dist}{ specifies a parametric distribution that is assumed in AFT model. This argument is the same as the dist option in the existing R function "survreg". These include "weibull", "exponential", "gaussian", "logistic", "lognormal", and "loglogistic". }
}
\details{
  If the SIMEXvariable is repeated measured then you only need to use arguments 
  repeated and repind without mention err.mat. The summary.simex will contain repind. 
}
\value{
  \item{coefficient}{the corrected coefficients of the AFT model}
  \item{se}{the standard deviation of each coefficient}
  \item{pvalue}{the p-value for the hypothesis of that coefficient equal zero}
  \item{scalreg}{the estimate of the scale}
  \item{theta}{the estimates for every B and lambda}
  \item{lambda}{ the vector of lambdas for which the simulation step should be done}
  \item{B}{ the number of simulated samples for the simulation step.}
  \item{formula}{ the model to be fitted in the survreg function}
  \item{err.mat}{ the covariance matrix of the variables with measurement error}
  \item{repind}{ the list contiains the names of the repeat measument variables}
  \item{extrapolation}{ the extrapolation method: linear ,quadratic are implemented (first 4 letters are enough)}
  \item{SIMEXvariable}{ the vector contains the names of the variables with meansurement error}
}

\references{
Genz, A., Bretz, F., Miwa, T., Mi, X., Leisch, F., Scheipl, F. and Hothorn, T. (2011). mvtnorm: Multivariate Normal and t Distributions. R package version 0.9-9991, URL http://CRAN.
R-project.org/package=mvtnorm.

He, W., Yi, G. Y. and Xiong, J. (2007). Accelerated Failure Time Models with Covariates Subject to Measurement Error. \emph{Statistics in Medicine}, 26, 4817-4832.

Therneau, T. and Lumley, T. (2011). survival: Survival Analysis, Including Penalised Likelihood. R package version 2.36-10, URL http://CRAN.R-project.org/package=survival.
}

\author{ Juan Xiong, Wenqing He and Grace Y. Yi}

\seealso{\code{\link[survival]{survreg}}, \code{\link[simexaft]{plotsimexaft}}}

\examples{
library("simexaft")
library("survival")
data("BHS")

dataset <- BHS

dataset$SBP <- log(dataset$SBP - 50)

###Naive AFT approach
formula <- Surv(SURVTIME,DTHCENS) ~ SBP + CHOL + AGE + BMI + SMOKE1 + SMOKE2

out1 <- survreg(formula = formula, data = dataset, dist = "weibull")

summary(out1)


###fit a AFT model with quadratic extrapolation
set.seed(120)

ind <- c("SBP", "CHOL")

err.mat <- diag(rep(0.5625, 2))

out2 <- simexaft(formula = formula, data = dataset, SIMEXvariable = ind, 
        repeated = FALSE, repind = list(), err.mat = err.mat, B = 50,
        lambda = seq(0, 2, 0.1),extrapolation = "quadratic", dist = "weibull")

summary(out2)






\donttest{
    #################### repeated measurements #################################
    data("rhDNase")

    ###true model
    rhDNase$fev.ave <- (rhDNase$fev + rhDNase$fev2)/2

    output1 <- survreg(Surv(time2, status) ~ trt + fev.ave, data = rhDNase, 
                    dist = "weibull")

    summary(output1)


    ####sensitive analysis#####
    set.seed(120)

    fev.error <- rhDNase$fev + rnorm(length(rhDNase$fev), mean = 0, 
                                    sd = 0.15 * sd(rhDNase$fev))

    fev.error2 <- rhDNase$fev2 + rnorm(length(rhDNase$fev2),mean = 0, 
                                    sd = 0.15 * sd(rhDNase$fev2))

    dataset2 <- cbind(rhDNase[, c("time2", "status", "trt")], fev.error, fev.error2)

    formula <- Surv(time2, status) ~ trt + fev.error

    ind <- "fev.error"


    ########naive model using the average FEV value####################
    fev.error.c <- (fev.error + fev.error2)/2

    output2 <- survreg(Surv(time2, status) ~ trt + fev.error.c, data = rhDNase, 
                    dist = "weibull")

    summary(output2)


    ######use simexaft and apply the quadratic extrapolation######
    formula <- Surv(time2, status) ~ trt + fev.error

    output3 <- simexaft(formula = formula, data = dataset2, SIMEXvariable = ind, 
            repeated=TRUE,repind=list(c("fev.error", "fev.error2")), err.mat=NULL, 
            B=50, lambda=seq(0,2, 0.1), extrapolation="quadratic", dist="weibull")
            
    summary(output3)
}

}



