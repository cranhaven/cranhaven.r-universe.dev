## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE------
library(ReturnCurves)

## ----data, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE---------
data(airdata)

## ----margdata, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE-----
# qmarg and constrainedshape set to the default values
expdata <- margtransf(data = airdata, qmarg = rep(0.95, 2), constrainedshape = T)

# attributes of the S4 object
str(expdata)

# head of the data on standard exponential margins
head(expdata@dataexp)

## ----plotsmarghist, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center'----
plot(expdata, which = "hist")

## ----plotsmargts, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center'----
plot(expdata, which = "ts")

## ----plotsmargjoint, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center', fig.height = 2.5----
plot(expdata, which = "joint")

## ----plotsmargall, echo=TRUE, fig.align='center', fig.height = 8, message=FALSE, warning=FALSE, paged.print=FALSE----
plot(expdata, which = "all") # or just plot(expdata)

## ----marggpd, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE------
# nboot and alpha are set to the default values
# blocksize is set to 10 to account for temporal dependence
uncgpd <- marggpd(margdata = expdata, blocksize = 10, nboot = 250, alpha = 0.05)

# attributes of the S4 object
str(uncgpd)

# head of the list elements of slot marggpd for variable X
head(uncgpd@marggpd$model[[1]])
head(uncgpd@marggpd$empirical[[1]])
head(uncgpd@marggpd$lower[[1]])
head(uncgpd@marggpd$upper[[1]])

## ----plotsmarggpd, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center', fig.width = 6.5, fig.height = 2.5----
plot(uncgpd)

## ----adfest, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE-------
# Estimation using Hill estimator without conditional extremes parameters
whill <- seq(0, 1, by = 0.001)
## q and constrained are set to the default values here
lambdah <- adf_est(margdata = expdata, w = whill, method = "hill", 
                   q = 0.95, constrained = F)

# Estimation using Hill estimator with conditional extremes parameters
## q and qalphas are set to the default values
lambdah2 <- adf_est(margdata = expdata, w = whill, method = "hill", q = 0.95,
                    qalphas = rep(0.95, 2), constrained = T)

# Estimation using CL method without conditional extremes parameters
## w, q and constrained are set to the default values here
lambdacl <- adf_est(margdata = expdata, w = seq(0, 1, by = 0.01), method = "cl",
                    q = 0.95, constrained = F)

# Estimation using CL method with conditional extremes parameters
## w, q and qalphas are set to the default values
lambdacl2 <- adf_est(margdata = expdata, w = seq(0, 1, by = 0.01), method = "cl",
                     q = 0.95, qalphas = rep(0.95, 2), constrained = T)

# attributes of the S4 object
str(lambdah)

# head of the vector with adf estimates for the first estimator
head(lambdah@adf)

## ----plotsadfest, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.width = 5, fig.height = 2.5, fig.align = 'center'----
# plot of the ADF estimation based on the unconstrained Hill estimator
plot(lambdah)

## ----adfgof, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE-------

# Goodness of fit of the adf for twp rays w
rays <- c(0.25, 0.75)
## nboot and alpha are set to the default values
## blocksize is set to 10 to account for temporal dependence
gofh <- sapply(rays, adf_gof, adf = lambdah, blocksize = 10, nboot = 250, alpha = 0.05)

# attributes of the S4 object
str(gofh[[1]])

# head of the list elements of slot gof
head(gofh[[1]]@gof$model)
head(gofh[[1]]@gof$empirical)
head(gofh[[1]]@gof$lower)
head(gofh[[1]]@gof$upper)

## ----plotsadfgof, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center', fig.width = 6.5, fig.height = 2.5----
library(gridExtra)
grid.arrange(plot(gofh[[1]]), plot(gofh[[2]]), ncol = 2)

## ----rcest, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE--------
n <- dim(airdata)[1] 
prob <- 10/n
# Estimation using Hill estimator without conditional extremes parameters
whill <- seq(0, 1, by = 0.001)
## q and constrained are set to the default values here
rch <- rc_est(margdata = expdata, w = whill, p = prob, method = "hill", 
              q = 0.95, constrained = F)

# Estimation using Hill estimator with conditional extremes parameters
## q and qalphas are set to the default values
rch2 <- rc_est(margdata = expdata, w = whill, p = prob, method = "hill", q = 0.95,
               qalphas = rep(0.95, 2), constrained = T)

# Estimation using CL method without conditional extremes parameters
## w, q and constrained are set to the default values here
rccl <- rc_est(margdata = expdata, w = seq(0, 1, by = 0.01), p = prob, method = "cl", 
               q = 0.95, constrained = F)

# Estimation using CL method with conditional extremes parameters
## w, q and qalphas are set to the default values
rccl2 <- rc_est(margdata = expdata, w = seq(0, 1, by = 0.01), p = prob, method = "cl", 
                q = 0.95, qalphas = rep(0.95, 2), constrained = T)

# attributes of the S4 object
str(rch)

# head of the vector with adf estimates for the first estimator
head(rch@rc)

## ----plotsrcest, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.width = 5, fig.height = 3, fig.align = 'center'----
# plot of the ADF estimation based on the unconstrained Hill estimator
plot(rch)

## ----rcunc, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE--------
# nangles and alpha set to default
# nboot set to 50 for simplicity
# blocksize is set to 10 to account for temporal dependence
rch_unc <- rc_unc(rch, blocksize = 10, nboot = 50, nangles = 150, alpha = 0.05)

# attributes of the S4 object
str(rch_unc)

# head of the list elements of slot unc
head(rch_unc@unc$median)
head(rch_unc@unc$mean)
head(rch_unc@unc$lower)
head(rch_unc@unc$upper)

## ----plotsrcunc, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center', fig.height=6,fig.width=8----
library(gridExtra)
grid.arrange(plot(rch_unc, which = "rc"), plot(rch_unc, which = "median"), 
             plot(rch_unc, which = "mean"), plot(rch_unc, which = "all"), nrow = 2)

## ----rcgof, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE--------
# nboot, nangles and alpha set to default
# blocksize is set to 10 to account for temporal dependence
rch_gof <- rc_gof(rch, blocksize = 10, nboot = 250, nangles = 150, alpha = 0.05)

# attributes of the S4 object
str(rch_gof)

# head of the list elements of slot gof
head(rch_gof@gof$median)
head(rch_gof@gof$lower)
head(rch_gof@gof$upper)

## ----plotsrcgof, echo=TRUE, message=FALSE, warning=FALSE, paged.print=FALSE, fig.align = 'center', fig.height = 3----
plot(rch_gof)

