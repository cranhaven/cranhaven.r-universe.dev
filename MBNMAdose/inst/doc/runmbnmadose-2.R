## ----setup, include = FALSE---------------------------------------------------
library(MBNMAdose)
library(rmarkdown)
library(knitr)
library(dplyr)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  include=TRUE,
  tidy.opts=list(width.cutoff=80),
  tidy=TRUE
)

## ----results="hide", warning=FALSE--------------------------------------------
# Prepare data using the triptans dataset
tripnet <- mbnma.network(triptans)

# Run an Emax dose-response MBNMA
mbnma <- mbnma.run(tripnet, fun=demax(emax="rel", ed50="rel"), 
                   method="random")

## ----message=FALSE, warning=FALSE---------------------------------------------
# Print neat summary of output
summary(mbnma)

## ----results="hide"-----------------------------------------------------------
# Emax model with single parameter estimated for Emax
emax <- mbnma.run(tripnet, fun=demax(emax="rel", ed50="common"), 
                  method="random")

## -----------------------------------------------------------------------------
summary(emax)

## ----results="hide", message=FALSE, warning=FALSE-----------------------------
# Using the osteoarthritis dataset
pain.df <- osteopain

# Set class equal to agent for all agents
pain.df$class <- pain.df$class

# Set a shared class (NSAID) only for Naproxcinod and Naproxen
pain.df$class[pain.df$agent %in% c("Naproxcinod", "Naproxen")] <- 
  "NSAID"

# Run a restricted cubic spline MBNMA with a random class effect on beta.1
painnet <- mbnma.network(pain.df)
splines <- mbnma.run(painnet, fun=dspline(type="bs", knots=2), class.effect = list(beta.1="random"))

## ----eval=FALSE---------------------------------------------------------------
#  # Using the depression SSRI dataset
#  depnet <- mbnma.network(ssri)
#  
#  # An example specifying a quadratic dose-response function
#  quadfun <- ~ (beta.1 * dose) + (beta.2 * (dose^2))
#  
#  quad <- mbnma.run(depnet, fun=duser(fun=quadfun, beta.1 = "rel", beta.2 = "rel"))

## ----eval=FALSE---------------------------------------------------------------
#  # Using the depression SSRI dataset
#  depnet <- mbnma.network(ssri)
#  
#  dr.funs <- dmulti(list(
#    "Placebo"=dfpoly(degree=2),
#    "citalopram"=dfpoly(degree=2),
#    "escitalopram"=dfpoly(degree=2),
#    "fluoxetine"=dspline(type="ns",knots=2),
#    "paroxetine"=dfpoly(degree=2),
#    "sertraline"=dspline(type="ns",knots=2)
#    ))
#  
#  multifun <- mbnma.run(depnet, fun=dr.funs, method="common", n.iter=50000)
#  summary(multifun)

## ----eval=FALSE---------------------------------------------------------------
#  dspline(type="bs", knots=3)
#  # ...is equivalent to
#  dspline(type="bs", knots=c(0.25,0.5,0.75))
#  
#  # Using a natural cubic spline on the SSRI dataset
#  depnet <- mbnma.network(ssri)
#  ns <- mbnma.run(depnet, fun=dspline(type="ns", knots=c(0.25,0.5,0.75)))

## -----------------------------------------------------------------------------
print(mbnma$model.arg$priors)

## ----eval=FALSE---------------------------------------------------------------
#  # Define replacement prior
#  new.priors <- list(
#    sd = "dnorm(0, 1) T(0,)"
#    )
#  
#  # Run an MBNMA model with new priors
#  emax <- mbnma.run(alognet, fun=demax(), method="random",
#                     priors=new.priors)

## ----eval=FALSE---------------------------------------------------------------
#  ed50.priors <- list(ed50 = c(
#    Celebrex="dnorm(100, 0.0025) T(0,)",
#    Etoricoxib="dnorm(20, 0.01) T(0,)",
#    Lumiracoxib="dnorm(50, 0.0025) T(0,)",
#    Naproxcinod="dnorm(500, 0.0004) T(0,)",
#    Naproxen="dnorm(500, 0.0004) T(0,)",
#    Rofecoxib="dnorm(35, 0.04) T(0,)",
#    Tramadol="dnorm(200, 0.0004) T(0,)",
#    Valdecoxib="dnorm(4, 0.04) T(0,)"
#    ))
#  
#  # Using the osteoarthritis dataset
#  mbnma <- mbnma.run(painnet, fun=demax(emax="rel", ed50="rel"),
#                     priors=ed50.priors)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Generate dataset without placebo
noplac.gout <- 
  gout[!gout$studyID %in% c(2001, 3102),] # Drop two-arm placebo studies
noplac.gout <- 
  noplac.gout[noplac.gout$agent!="Plac",] # Drop placebo arm from multi-arm studies

# Create mbnma.network object
noplac.net <- mbnma.network(noplac.gout)

## -----------------------------------------------------------------------------
# Plot network
plot(noplac.net, label.distance=5)

## -----------------------------------------------------------------------------
# Network plot at the agent level illustrates how doses can connect using MBNMA
plot(noplac.net, level="agent", remove.loops = TRUE, label.distance = 4)

## -----------------------------------------------------------------------------
# Network plot assuming connectivity via two doses
# Allows estimation of a single-parameter dose-response function
plot(noplac.net, level="agent", remove.loops = TRUE, label.distance = 4,
     doselink=2)

# Network plot assuming connectivity via three doses
# Allows estimation of a two-parameter dose-response function
plot(noplac.net, level="agent", remove.loops = TRUE, label.distance = 4,
     doselink=3)

## ----nonparam, results="hide"-------------------------------------------------
nonparam <- mbnma.run(tripnet, fun=dnonparam(direction="increasing"), method="random")

## -----------------------------------------------------------------------------
print(nonparam)

## ----results="hide"-----------------------------------------------------------
tripnet <- mbnma.network(triptans)
trip.emax <- mbnma.run(tripnet, fun=demax(emax="rel", ed50="rel")) 

## ----results="hide"-----------------------------------------------------------
# Plot boxplots of residual deviance contributions (scatterplot is the default)
devplot(trip.emax, plot.type = "box")

## ----results="hide", warning=FALSE--------------------------------------------
# Plot fitted and observed values with treatment labels
fitplot(trip.emax)

