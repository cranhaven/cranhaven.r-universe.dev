## ----eval=FALSE---------------------------------------------------------------
# ## Install from CRAN
# install.packages("dsdp")

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Import dsdp
library(dsdp)

## ----eval=TRUE----------------------------------------------------------------
## Import ggplot2 if necessary.
library(ggplot2)

## ----eval=TRUE----------------------------------------------------------------
## Create gaussmodel object from a data set mix2gauss$n200
gm1 <- gaussmodel(data=mix2gauss$n200)

## ----eval=FALSE---------------------------------------------------------------
# ## Create gaussmodel object from a data set mix2gaussHist$n200p and
# ## its frequencies mix2gaussHist$n200f
# gm2 <- gaussmodel(mix2gaussHist$n200p, mix2gaussHist$n200f)

## ----eval=TRUE----------------------------------------------------------------
## Display the summary of a data set
summary(gm1)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
## Draw a histogram of the data set
plot(gm1)

## ----eval=FALSE---------------------------------------------------------------
# ## Output is omitted for brevity
# plot(gm1, bins=50)
# ``` -->
# 
# ### Providing the set of parameters
# Before estimation, we need to provide a set of parameters,
# means, standard deviations, and degrees of polynomials,
#  to compute the coefficients of
# polynomials.
# 

## ----eval=TRUE----------------------------------------------------------------
## A vector of degrees of polynomials
deglist <- c(2, 4, 6)
## A vector of means in Gaussian distributions
mulist <- c(-0.5, 0, 0.5)
## A vector of standard deviations in Gaussian distributions
sdlist <- c(0.75, 1.0, 1.25)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
gm1 <- estimate(gm1, deglist=deglist, mulist=mulist, sdlist=sdlist, scaling=TRUE)

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(gm1, nmax=5, estonly=TRUE)

## ----eval=FALSE, results="hide", message=FALSE--------------------------------
# ## This is demonstration for recomputation
# ## Not Executed
# gm1 <- estimate(gm1, deglist=deglist, mulist=mulist, sdlist=sdlist, scaling=TRUE,
#           recompute=TRUE, stepsize=c(0.4, 0.2))

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(gm1)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(gm1, scaling=TRUE)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
gm1 <- estimate(gm1, c(4, 6, 8), seq(0, 0.5, by=0.1), seq(0.5, 1, by=0.1),
        scaling=TRUE)

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(gm1, nmax=5, estonly=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(gm1)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
gm1 <- estimate(gm1, c(4, 6, 8), seq(0, 0.2, by=0.05), seq(0.6, 0.8, by=0.05),
          scaling=TRUE)

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(gm1, nmax=5, estonly=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(gm1)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
gm1 <- estimate(gm1, c(4, 6, 8), seq(0, 0.2, by=0.025), seq(0.7, 0.8, by=0.01),
          scaling=TRUE)

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(gm1, nmax=5, estonly=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(gm1)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(gm1, cum=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
x <- seq(-4, 4, by=0.1)
## Compute the density of 1st estimate
y_pdf <- func(gm1, x, n=1)
## Compute the cumulative distribution of 1st estimate
y_cdf <- func(gm1, x, cdf=TRUE, n=1)

## ----eval=TRUE----------------------------------------------------------------
em1 <- expmodel(mixexpgamma$n200)

## ----eval=FALSE---------------------------------------------------------------
# em2 <- expmodel(mixExpGammaHist$n800p, mixExpGammaHist$n800f)

## ----eval=TRUE----------------------------------------------------------------
## Display the summary of a data set
summary(em1)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
## Draw a histogram of the data set
plot(em1)

## ----eval=TRUE----------------------------------------------------------------
## A vector of degrees of polynomials
deglist <- c(2, 3, 4)
## A vector of rate parameters of exponential distributions
lmdlist <- c(0.5, 1, 2, 4)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
em1 <- estimate(em1, deglist=deglist, lmdlist=lmdlist)

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(em1, nmax=5, estonly=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(em1)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
em1 <- estimate(em1, c(3, 4, 5, 6), c(1, 2, 4, 8))

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(em1, nmax=5, estonly=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(em1)

## ----eval=TRUE, results="hide", message=FALSE---------------------------------
## Do estimation
## Output messages are suppressed for brevity
em1 <- estimate(em1, c(5, 6), seq(3, 4, by=0.25))

## ----eval=TRUE----------------------------------------------------------------
## Show the summary of results up to 5
summary(em1, nmax=5, estonly=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(em1)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
plot(em1, cum=TRUE)

## ----eval=TRUE, out.height="80%", out.width="70%"-----------------------------
x <- seq(0, 14, by=0.1)
## Compute the density of 1st estimate
y_pdf <- func(em1, x, n=1)
## Compute the cumulative distribution of 1st estimate
y_cdf <- func(em1, x, cdf=TRUE, n=1)

