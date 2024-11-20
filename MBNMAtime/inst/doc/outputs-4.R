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

## ---- result="hide"-----------------------------------------------------------
# Run a quadratic time-course MBNMA using the alogliptin dataset
network.alog <- mb.network(alog_pcfb)

mbnma <- mb.run(network.alog, 
                fun=tpoly(degree=2,
                          pool.1="rel", method.1="random",
                          pool.2="rel", method.2="common"
                          )
)

## -----------------------------------------------------------------------------
# Calculate relative effects between 3 treatments
allres <- get.relative(mbnma, time=20,
                       treats = c("alog_100", "alog_50", "placebo"))
print(allres)

## ---- echo=FALSE, results='asis', fig.cap="2-stage MBNMA: For clarity, 95%CrIs are not shown in the plots or tables but these are calculated and computed in `get.relative()`. Thick connecting lines in network plots indicate comparisons with rich time-course data that can be modelled with a more complex function (e.g. B-spline), thin connecting lines in network plots indicate comparisons with sparse time-course data that can only be modelled with a less complex function (e.g. BEST-ITP). Comparisons between treatments in different subnetworks that are not the network reference must be excluded (red dashed line in network plot)."----
knitr::include_graphics("2stageMBNMA.png", dpi=250)

## ---- results="hide", fig.show="hold", eval=FALSE-----------------------------
#  # Using the osteoarthritis dataset
#  network.pain <- mb.network(osteopain, reference = "Pl_0")
#  
#  # Run a first-order fractional polynomial time-course MBNMA
#  mbnma <- mb.run(network.pain,
#                  fun=tfpoly(degree=1,
#                            pool.1="rel", method.1="random",
#                            method.power1=0.5))
#  
#  # Plot a box-plot of deviance contributions (the default)
#  devplot(mbnma, n.iter=1000)

## ---- echo=FALSE, results="hide", fig.show="hold"-----------------------------
# Using the osteoarthritis dataset
network.pain <- mb.network(osteopain, reference = "Pl_0")

# Run a first-order fractional polynomial time-course MBNMA
mbnma <- mb.run(network.pain, 
                fun=tfpoly(degree=1,
                          pool.1="rel", method.1="random",
                          method.power1=0.5), n.iter=5000)

# Plot a box-plot of deviance contributions (the default)
devplot(mbnma, n.iter=500)

## ---- eval=FALSE--------------------------------------------------------------
#  # Plot fitted and observed values with treatment labels
#  fitplot(mbnma, n.iter=1000)

## ---- results="hide"----------------------------------------------------------
# Run a quadratic time-course MBNMA using the alogliptin dataset
mbnma <- mb.run(network.alog, 
                fun=tpoly(degree=2,
                          pool.1="rel", method.1="random",
                          pool.2="rel", method.2="common"
                          )
)

plot(mbnma)

## ---- include=FALSE, eval=rmarkdown::pandoc_available("1.12.3")---------------
load(system.file("extdata", "ranks.rda", package="MBNMAtime", mustWork = TRUE))

## ---- results="hide", eval=rmarkdown::pandoc_available("1.12.3")--------------
# Using the osteoarthritis dataset
network.pain <- mb.network(osteopain, reference = "Pl_0")

# Identify quantile for knot at 1 week
timequant <- 1/max(network.pain$data.ab$time)

# Run a piecewise linear time-course MBNMA with a knot at 1 week
mbnma <- mb.run(network.pain,
                fun=tspline(type="ls", knots = timequant,
                            pool.1 = "rel", method.1="common",
                            pool.2 = "rel", method.2="common"))


# Rank results based on AUC (calculated 0-10 weeks), more negative slopes considered to be "better"
ranks <- rank(mbnma, param=c("auc"), 
                    int.range=c(0,10),  lower_better = TRUE, n.iter=1000)

## ---- echo=FALSE, eval=FALSE, include=FALSE-----------------------------------
#  save(ranks, file="inst/extdata/ranks.rda")

## ---- eval=rmarkdown::pandoc_available("1.12.3")------------------------------
print(ranks)

## ---- eval=rmarkdown::pandoc_available("1.12.3")------------------------------
# Ranking histograms for AUC
plot(ranks)

## ---- eval=rmarkdown::pandoc_available("1.12.3")------------------------------
# Cumulative ranking for all ranked parameters
cumrank(ranks)

