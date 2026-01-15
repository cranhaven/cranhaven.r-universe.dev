## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$out.lines)) {
    x <- xfun::split_lines(x)
    if (length(x) > n) {
      # truncate the output
      x <- c(head(x, n), "....\n")
    }
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})
##
### Load packages here:
##
### Figure counter:
(
  function() {
    log <- list(
      labels = character(),
      captions = character()
    )
    list(
      register = function(label, caption) {
        log$labels <<- c(log$labels, label)
        log$captions <<- c(log$captions, caption)
        invisible(NULL)
      },
      getNumber = function(label) {
        which(log$labels == label)
      },
      getCaption = function(label) {
        a <- which(log$labels == label)
        cap <- log$captions[a]
        cat(sprintf("Fig. %d. %s\n\n---\n",a,cap))
        invisible(NULL)
      }
    )
  }
)() -> fc
fc$register(
  "sef1",
  paste(
    "Examples of pMEM spatial eigenfunctions of order 1 with descriptor (back",
    "markers) and prediction scores (red markers). The black continuous line",
    "is calculated for 1-m intervals to show the continuity of the spatial",
    "eigenfunctions."
  )
)
fc$register(
  "depth",
  paste(
    "Spatially-explicit predictions of channel depth (black solid line) with",
    "observations used training (black markers) and testing (red markers) the",
    "model."
  )
)
fc$register(
  "velocity",
  paste(
    "Spatially-explicit predictions of current velocity (black solid line)",
    "with observations used training (black markers) and testing (red markers)",
    "the model."
  )
)
fc$register(
  "substrate",
  paste(
    "Spatially-explicit predictions of mean substrate grain size (black solid",
    "line) with observations used training (black markers) and testing (red",
    "markers) the model."
  )
)

## -----------------------------------------------------------------------------
data("salmon", package = "pMEM")

## -----------------------------------------------------------------------------
library("pMEM")         ## To calculate pMEM
library("magrittr")     ## For its very handy pipe operateur (%>%)
library("glmnet")       ## To calculate elastic net regression

## -----------------------------------------------------------------------------
set.seed(1234567890)                     ## For the drawing to be repeatable.
sort(sample(nrow(salmon),25)) -> test    ## Drawing the testing set (N = 25).
(1:nrow(salmon))[-test] -> train         ## The training set is the remainder.

## -----------------------------------------------------------------------------
genSEF(
  x = salmon$Position[train],   ## The set of locations.
  m = genDistMetric(),          ## The distance metric function.
  f = genDWF("linear", 500)     ## The distance weighting function.
) -> sef0

sef0                            ## Show the resulting object.

## ----fig.width=7.25, fig.height=10--------------------------------------------
## A regular transect of points 1 m apart:
salmon$Position %>%
  {seq(min(.) - 20, max(.) + 20, 1)} -> xx

## Custom plotting function:
plotSEF <- function(sef, xTrain, xTest, xx, wh, ...) {
  plot(x = xx, y = predict(sef, xx, wh), type = "l", ...)
  points(x = xTrain, y = as.matrix(sef, wh), pch=21, bg="black")
  points(x = xTest, y = predict(sef, xTest)[,wh], pch=21, bg="red")
  invisible(NULL)
}

## Storing the graphical parameters:
p <- par(no.readonly = TRUE)

## Changing the graphical parameters:
par(mfrow=c(3,2), mar=c(4.6,4.6,3,1))

## Generate a six-inset plot:
for(fun in c("power","hyperbolic","spherical","exponential","Gaussian",
             "hole_effect"))
  genSEF(
    x = salmon$Position[train],
    m = genDistMetric(),
    f = genDWF(fun, 250, 0.75)
  ) %>%
    plotSEF(salmon$Position[train], salmon$Position[test], xx, 1,
            xlab="Location (m)", ylab="pMEM score", main=fun, lwd=2)

## ----echo=FALSE, results='asis'-----------------------------------------------
fc$getCaption("sef1")

## -----------------------------------------------------------------------------
## Restoring the graphical parameters:
par(p)

## -----------------------------------------------------------------------------
getMinMSE(
  U = as.matrix(sef0),
  y = salmon$Depth[train],
  Up = predict(sef0, salmon$Position[test]),
  yy = salmon$Depth[test],
  complete = FALSE
)

## -----------------------------------------------------------------------------
objf <- function(par, m, fun, x, xx, y, yy, lb, ub) {
  
  ## Bound the parameter values within the lb -> ub intervals:
  par <- (ub - lb) * (1 + exp(-par))^(-1) + lb
  ## This step is necessary to prevent pitfalls during optimization.
  
  ## Calculate the SEF under the conditions requested
  if(fun %in% c("power","hyperbolic")) {
    sef <- genSEF(x, m, genDWF(fun, range = par[1L], shape = par[2L]))
  } else
    sef <- genSEF(x, m, genDWF(fun, range = par[1L]))
  
  ## Calculate the minMSE model
  res <- getMinMSE(as.matrix(sef), y, predict(sef, xx), yy, FALSE)
  
  ## The objective criterion is the out of the sample mean squared error:
  res$mse
}

## -----------------------------------------------------------------------------
objf(
  par = c(0),
  m = genDistMetric(),
  fun = "linear",
  x = salmon$Position[train],
  xx = salmon$Position[test],
  y = salmon[["Depth"]][train],
  yy = salmon[["Depth"]][test],
  lb = c(10),
  ub = c(1000)
) -> res

res

## ----eval=FALSE---------------------------------------------------------------
#  sefTrain <- list()  ## For storing the "SEMap" objects.
#  mseRes <- list()    ## For storing results from function getMinMSE().
#  sel <- list()       ## For storing selected pMEM eigenfunctions.
#  lm <- list()        ## For storing the linear models.
#  prd <- list()       ## For storing the predictions.

## ----echo=FALSE---------------------------------------------------------------
load(file = "Using_pMEM.rda")
mseRes <- list()
sel <- list()
lm <- list()
prd <- list()

## -----------------------------------------------------------------------------
estimateSEF <- function(x, xx, y, yy, lower, upper) {
  
  res <- list(optim = list())  ## A list to contain the results.
  
  ## This loop tries the seven DWF one by one, estimating 'dmax' (and, when
  ## necessary, 'shape') using simulated annealing.
  for(fun in c("linear","power","hyperbolic","spherical","exponential",
               "Gaussian","hole_effect")) {
    optim(
      par = c(0,if(fun %in% c("power","hyperbolic")) 0), fn = objf,
      method = "SANN", m = genDistMetric(), fun = fun,
      x = x, xx = xx, y = y, yy = yy,
      lb = c(lower[1],if(fun %in% c("power","hyperbolic")) lower[2]),
      ub = c(upper[1],if(fun %in% c("power","hyperbolic")) upper[2])
      
    ) -> res$optim[[fun]]
  }
  
  ## Extract the minimum values from the list of optimization:
  unlist(
    lapply(
      res$optim,
      function(x) x$value
    )
  ) -> res$bestval
  
  ## Find which DWF had the minimum objective criterion value:
  names(
    which.min(
      res$bestval
    )
  ) -> res$fun
  
  ## Back-transform the parameter values:
  res %>%
    {.$optim[[.$fun]]$par} %>%
    {(upper - lower) * (1 + exp(-.))^(-1) + lower} -> res$par
  
  ## Calculate the SEF using the optimized DWF parameters:
  res %>%
    {genSEF(
      x = x,
      m = genDistMetric(),
      f = genDWF(.$fun, .$par[1L], if(length(.$par) > 1) .$par[1L])
    )} -> res$sef
  
  ## Return the result list:
  res
}

## ----eval=FALSE---------------------------------------------------------------
#  estimateSEF(
#    x = salmon$Position[train],
#    xx = salmon$Position[test],
#    y = salmon[["Depth"]][train],
#    yy = salmon[["Depth"]][test],
#    lower = c(20,0.25),
#    upper = c(1000,1.75)
#  ) -> sefTrain[["Depth"]]

## -----------------------------------------------------------------------------
## Calculate the channel depth model:
sefTrain[["Depth"]]$sef %>%
  {getMinMSE(
    U = as.matrix(.),
    y = salmon[["Depth"]][train],
    Up = predict(., salmon$Position[test]),
    yy = salmon[["Depth"]][test]
  )} -> mseRes[["Depth"]]

## Extract the selected SEF:
mseRes[["Depth"]] %>% {sort(.$ord[1:.$wh])} -> sel[["Depth"]]

## -----------------------------------------------------------------------------
## Calculate a linear model from the selected SEF:
lm(
  formula = y~.,
  data = cbind(
    y = salmon[["Depth"]][train],
    as.data.frame(sefTrain[["Depth"]]$sef, wh=sel[["Depth"]])
  )
) -> lm[["Depth"]]

## Calculate the predictions:
predict(
  lm[["Depth"]],
  newdata = as.data.frame(
    predict(
      object = sefTrain[["Depth"]]$sef,
      newdata = xx,
      wh = sel[["Depth"]]
    )
  )
) -> prd[["Depth"]]

## ----fig.width=6, fig.height=6------------------------------------------------
plot(x=xx, y=prd[["Depth"]], type="l",
     ylim=range(salmon[["Depth"]], prd[["Depth"]]), las=1,
     ylab="Channel depth (m)", xlab="Location along the transect (m)")
points(x = salmon$Position[train], y = salmon[["Depth"]][train], pch=21,
       bg="black")
points(x = salmon$Position[test], y = salmon[["Depth"]][test], pch=21, bg="red")

## ----echo=FALSE, results='asis'-----------------------------------------------
fc$getCaption("depth")

## ----eval=FALSE---------------------------------------------------------------
#  ## Estimate the most adequate predictive Moran's eigenvector map:
#  estimateSEF(
#    x = salmon$Position[train],
#    xx = salmon$Position[test],
#    y = salmon[["Velocity"]][train],
#    yy = salmon[["Velocity"]][test],
#    lower = c(20,0.25),
#    upper = c(1000,1.75)
#  ) -> sefTrain[["Velocity"]]

## -----------------------------------------------------------------------------
## Calculate the current velocity model:
sefTrain[["Velocity"]]$sef %>%
  {getMinMSE(
    U = as.matrix(.),
    y = salmon[["Velocity"]][train],
    Up = predict(., salmon$Position[test]),
    yy = salmon[["Velocity"]][test]
  )} -> mseRes[["Velocity"]]

## Extract the selected SEF:
mseRes[["Velocity"]] %>% {sort(.$ord[1:.$wh])} -> sel[["Velocity"]]

## Calculate a linear model from the selected SEF:
lm(
  formula = y~.,
  data = cbind(
    y = salmon[["Velocity"]][train],
    as.data.frame(sefTrain[["Velocity"]]$sef, wh=sel[["Velocity"]])
  )
) -> lm[["Velocity"]]

## Calculate the predictions:
predict(
  lm[["Velocity"]],
  newdata = as.data.frame(
    predict(
      object = sefTrain[["Velocity"]]$sef,
      newdata = xx,
      wh = sel[["Velocity"]]
    )
  )
) -> prd[["Velocity"]]

## ----fig.width=6, fig.height=6------------------------------------------------
plot(x=xx, y=prd[["Velocity"]], type="l",
     ylim=range(salmon[["Velocity"]], prd[["Velocity"]]), las=1,
     ylab="Velocity (m/s)", xlab="Location along the transect (m)")
points(x = salmon$Position[train], y = salmon[["Velocity"]][train], pch=21,
       bg="black")
points(x = salmon$Position[test], y = salmon[["Velocity"]][test], pch=21,
       bg="red")

## ----echo=FALSE, results='asis'-----------------------------------------------
fc$getCaption("velocity")

## ----eval=FALSE---------------------------------------------------------------
#  ## Estimate the most adequate predictive Moran's eigenvector map:
#  estimateSEF(
#    x = salmon$Position[train],
#    xx = salmon$Position[test],
#    y = salmon[["Substrate"]][train],
#    yy = salmon[["Substrate"]][test],
#    lower = c(20,0.25),
#    upper = c(1000,1.75)
#  ) -> sefTrain[["Substrate"]]

## -----------------------------------------------------------------------------
## Calculate the mean substrate grain size model:
sefTrain[["Substrate"]]$sef %>%
  {getMinMSE(
    U = as.matrix(.),
    y = salmon[["Substrate"]][train],
    Up = predict(., salmon$Position[test]),
    yy = salmon[["Substrate"]][test]
  )} -> mseRes[["Substrate"]]

## Extract the selected SEF:
mseRes[["Substrate"]] %>% {sort(.$ord[1:.$wh])} -> sel[["Substrate"]]

## Calculate a linear model from the selected SEF:
lm(
  formula = y~.,
  data = cbind(
    y = salmon[["Substrate"]][train],
    as.data.frame(sefTrain[["Substrate"]]$sef, wh=sel[["Substrate"]])
  )
) -> lm[["Substrate"]]

## Calculate the predictions:
predict(
  lm[["Substrate"]],
  newdata = as.data.frame(
    predict(
      object = sefTrain[["Substrate"]]$sef,
      newdata = xx,
      wh = sel[["Substrate"]]
    )
  )
) -> prd[["Substrate"]]

## ----echo=FALSE, fig.width=6, fig.height=6------------------------------------
plot(x=xx, y=prd[["Substrate"]], type="l",
     ylim=range(salmon[["Substrate"]], prd[["Substrate"]]), las=1,
     ylab="Mean grain size (mm)",
     xlab="Location along the transect (m)")
points(x = salmon$Position[train], y = salmon[["Substrate"]][train], pch=21,
       bg="black")
points(x = salmon$Position[test], y = salmon[["Substrate"]][test], pch=21,
       bg="red")

## ----echo=FALSE, results='asis'-----------------------------------------------
fc$getCaption("substrate")

## -----------------------------------------------------------------------------
objf2 <- function(par, m, fun, x, xx, y, yy, w, ww, lb, ub) {
  par <- (ub - lb) * (1 + exp(-par))^(-1) + lb
  if(fun %in% c("power","hyperbolic")) {
    sef <- genSEF(x, m, genDWF(fun, range = par[3L], shape = par[4L]))
  } else
    sef <- genSEF(x, m, genDWF(fun, range = par[3L]))
  glm1 <- glmnet(x = cbind(w, as.matrix(sef)), y = y, family = "poisson",
                 alpha = par[1L], lambda = par[2L])
  pp <- predict(glm1, newx = cbind(ww, predict(sef, xx)), type="response")
  -2*sum(dpois(yy, pp, log = TRUE))
}

## -----------------------------------------------------------------------------
## Implement a list of orthogonal polynomial objects:
plist <- list()
plist[["Depth"]] <- poly(salmon[train,"Depth"],2)
plist[["Velocity"]] <- poly(salmon[train,"Velocity"],2)
plist[["Substrate"]] <- poly(salmon[train,"Substrate"],2)

## The matrix of auxiliary descriptor for the training set:
cbind(
  as.matrix(plist[["Depth"]]),
  as.matrix(plist[["Velocity"]]),
  as.matrix(plist[["Substrate"]])
) -> w

## Generate suitable column names:
c("Depth^1","Depth^2",
  "Velocity^1","Velocity^2",
  "Substrate^1","Substrate^2") -> colnames(w)

## The matrix of auxiliary descriptor for the testing set:
cbind(
  predict(plist[["Depth"]], newdata=salmon[test,"Depth"]),
  predict(plist[["Velocity"]], newdata=salmon[test,"Velocity"]),
  predict(plist[["Substrate"]], newdata=salmon[test,"Substrate"])
) -> ww

## Copying the column names:
colnames(ww) <- colnames(w)

## -----------------------------------------------------------------------------
objf2(
  par = c(0, 0, 0, 0),
  m = genDistMetric(),
  fun = "Gaussian",
  x = salmon$Position[train],
  xx = salmon$Position[test],
  y = salmon[["Abundance"]][train],
  yy = salmon[["Abundance"]][test],
  w = w,
  ww = ww,
  lb = c(0,0,20,0.25),
  ub=c(1,1,1000,1.75)
) -> res2

res2

## -----------------------------------------------------------------------------
estimateSEF2 <- function(x, xx, y, yy, w, ww, lower, upper) {
  
  res <- list(optim = list())  ## A list to contain the results.
  
  ## This loop tries the seven DWF one by one, estimating 'dmax' (and, when
  ## necessary, 'shape') using simulated annealing.
  for(fun in c("linear","power","hyperbolic","spherical","exponential",
               "Gaussian","hole_effect")) {
    optim(
      par = c(0,0,0,if(fun %in% c("power","hyperbolic")) 0), fn = objf2,
      method = "SANN", m = genDistMetric(), fun = fun,
      x = x, xx = xx, y = y, yy = yy, w = w, ww = ww,
      lb = c(lower[1:3],if(fun %in% c("power","hyperbolic")) lower[4]),
      ub = c(upper[1:3],if(fun %in% c("power","hyperbolic")) upper[4])
    ) -> res$optim[[fun]]
  }
  
  ## Extract the minimum values from the list of optimization:
  unlist(
    lapply(
      res$optim,
      function(x) x$value
    )
  ) -> res$bestval
  
  ## Find which DWF had the minimum objective criterion value:
  names(
    which.min(
      res$bestval
    )
  ) -> res$fun
  
  ## Back-transform the parameter values:
  res %>%
    {.$optim[[.$fun]]$par} %>%
    {(upper[1:length(.)] - lower[1:length(.)]) * (1 + exp(-.))^(-1) +
        lower[1:length(.)]} -> res$par
  
  ## Calculate the SEF using the optimized DWF parameters:
  res %>%
    {genSEF(
      x = x,
      m = genDistMetric(),
      f = genDWF(.$fun, .$par[3], if(length(.$par) > 3) .$par[4])
    )} -> res$sef
  
  ## Return the result list:
  res
}

## ----eval=FALSE---------------------------------------------------------------
#  estimateSEF2(
#    x = salmon$Position[train],
#    xx = salmon$Position[test],
#    y = salmon[["Abundance"]][train],
#    yy = salmon[["Abundance"]][test],
#    w = w,
#    ww = ww,
#    lower = c(0,0,20,0.25),
#    upper = c(1,1,1000,1.75)
#  ) -> sefTrain[["Abundance"]]

## -----------------------------------------------------------------------------
cbind(w, as.matrix(sefTrain[["Abundance"]]$sef)) %>%
  glmnet(
    y = salmon$Abundance[train], family = "poisson",
    alpha = sefTrain[["Abundance"]]$par[1L],
    lambda = sefTrain[["Abundance"]]$par[2L]) -> lm[["Abundance"]]

## Model coefficients:
coef(lm[["Abundance"]])

## -----------------------------------------------------------------------------
lm[["Abundance"]] %>%
  predict(
    cbind(
      predict(plist[["Depth"]], prd[["Depth"]]),
      predict(plist[["Velocity"]], prd[["Velocity"]]),
      predict(plist[["Substrate"]], prd[["Substrate"]]),
      predict(sefTrain[["Abundance"]]$sef, xx)
    ),
    type="response"
  ) ->  prd[["Abundance"]]

## ----fig.width=6, fig.height=6------------------------------------------------
plot(x=xx, y=prd[["Abundance"]], type="l",
     ylim=range(salmon[["Abundance"]], prd[["Abundance"]]), las=1,
     ylab="Parr abundance (fish)",
     xlab="Location along the transect (m)")
points(x = salmon$Position[train], y = salmon[["Abundance"]][train], pch=21,
       bg="black")
points(x = salmon$Position[test], y = salmon[["Abundance"]][test], pch=21,
       bg="red")

