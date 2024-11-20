## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  eval=rmarkdown::pandoc_available("1.12.3")
)

library(MBNMAtime)
library(rmarkdown)
library(knitr)
library(dplyr)
#load(system.file("extdata", "vignettedata.rda", package="MBNMAtime", mustWork = TRUE))

## ---- results="hide", message=FALSE, eval=FALSE-------------------------------
#  # Run an Emax time-course MBNMA using the osteoarthritis dataset
#  mbnma <- mb.run(network.pain,
#                  fun=temax(pool.emax="rel", method.emax="common",
#                            pool.et50="abs", method.et50="common"),
#                  rho="dunif(0,1)", covar="varadj")

## ---- results="hide", message=FALSE, echo=FALSE-------------------------------
# Run an Emax time-course MBNMA using the osteoarthritis dataset
network.pain <- mb.network(osteopain)

mbnma <- mb.run(network.pain,
                fun=temax(pool.emax="rel", method.emax="common",
                          pool.et50="abs", method.et50="common"),
                rho="dunif(0,1)", covar="varadj", n.iter=3000)

## ---- results="hide", message=FALSE, eval=rmarkdown::pandoc_available("1.12.3")----
# Specify placebo time-course parameters
ref.params <- list(emax=-2)

# Predict responses for a selection of treatments using a stochastic E0 and
# placebo parameters defined in ref.params to estimate the network reference treatment effect
pred <- predict(mbnma, treats=c("Pl_0", "Ce_200", "Du_90", "Et_60", 
                                        "Lu_400", "Na_1000", "Ox_44", "Ro_25",
                                        "Tr_300", "Va_20"),
                        E0=~rnorm(n, 8, 0.5), ref.resp=ref.params)

print(pred)

## ---- results="hide", message=FALSE, eval=rmarkdown::pandoc_available("1.12.3")----
# Generate a dataset of network reference treatment responses over time
placebo.df <- network.pain$data.ab[network.pain$data.ab$treatment==1,]

# Predict responses for a selection of treatments using a deterministic E0 and 
#placebo.df to model the network reference treatment effect
pred <- predict(mbnma, treats=c("Pl_0", "Ce_200", "Du_90", "Et_60", 
                                        "Lu_400", "Na_1000", "Ox_44", "Ro_25",
                                        "Tr_300", "Va_20"),
                        E0=10, ref.resp=placebo.df)

print(pred)

## ---- message=FALSE, eval=rmarkdown::pandoc_available("1.12.3")---------------
plot(pred, overlay.ref=TRUE, disp.obs=TRUE)

## ---- fig.height=3, results="hide", eval=FALSE--------------------------------
#  # Fit a quadratic time-course MBNMA to the Obesity dataset
#  network.obese <- mb.network(obesityBW_CFB, reference = "plac")
#  
#  mbnma <- mb.run(network.obese,
#                  fun=tpoly(degree=2,
#                            pool.1 = "rel", method.1="common",
#                            pool.2="rel", method.2="common"))
#  
#  # Define stochastic values centred at zero for network reference treatment
#  ref.params <- list(beta.1=~rnorm(n, 0, 0.05), beta.2=~rnorm(n, 0, 0.0001))
#  
#  # Predict responses within the range of the data
#  pred.obese <- predict(mbnma, times=c(0:50), E0=100, treats = c(1,4,15),
#                          ref.resp=ref.params)
#  
#  # Plot predictions
#  plot(pred.obese, disp.obs = TRUE)

## ---- fig.height=3, results="hide", echo=FALSE, message=FALSE-----------------
# Fit a quadratic time-course MBNMA to the Obesity dataset
network.obese <- mb.network(obesityBW_CFB, reference = "plac")

mbnma <- mb.run(network.obese,
                fun=tpoly(degree=2,
                          pool.1 = "rel", method.1="common",
                          pool.2="rel", method.2="common"), n.iter=3000)

# Define stochastic values centred at zero for network reference treatment
ref.params <- list(beta.1=~rnorm(n, 0, 0.05), beta.2=~rnorm(n, 0, 0.0001))

# Predict responses within the range of the data
pred.obese <- predict(mbnma, times=c(0:50), E0=100, treats = c(1,4,15),
                        ref.resp=ref.params)

# Plot predictions
plot(pred.obese, disp.obs = TRUE)

## ---- results="hide", warning=FALSE-------------------------------------------
# Overlay predictions from lumped NMAs between 5-8 and between 8-15 weeks follow-up
plot(pred, overlay.nma=c(5,8,15), n.iter=20000)

## -----------------------------------------------------------------------------
# Predict responses within the range of data
pred.obese <- predict(mbnma, times=c(0:50),
                      E0=0, ref.resp = NULL)

# Rank predictions at 50 weeks follow-up
ranks <- rank(pred.obese, time=50)

summary(ranks)
plot(ranks)

