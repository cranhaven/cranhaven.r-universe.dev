## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  eval = TRUE,
  echo = TRUE,
  fig.height = 3*0.65,
  fig.width = 4,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)
library(LMoFit)

## -----------------------------------------------------------------------------
FLOW_AMAX

## -----------------------------------------------------------------------------
sample_lmoments <- get_sample_lmom(x = FLOW_AMAX)
knitr::kable(sample_lmoments, digits = 2, caption = "Sample L-moments and L-moment ratios")

## -----------------------------------------------------------------------------
# Fitting of BrIII distribution
parameters <- fit_BrIII(sl1 = 436, st2 = 0.144, st3 = 0.103)
scale <- parameters$scale
shape1 <- parameters$shape1
shape2 <- parameters$shape2

## -----------------------------------------------------------------------------
knitr::kable(parameters[1:3], digits = 2, caption = "Estimated parameters of BrIII distribution")

## -----------------------------------------------------------------------------
scale <- 322; shape1 <- 6.22; shape2 <- 0.12
probability <- pBrXII(x = 500, para = c(scale, shape1, shape2))
probability

## -----------------------------------------------------------------------------
probability <- pBrXII(x = c(400, 450, 500, 550, 600, 1000), para = c(scale, shape1, shape2))
probability

## -----------------------------------------------------------------------------
location <- 388; scale <- 99; shape <- -0.11
density <- dgev(x = 200, para = c(location, scale, shape))
density

## -----------------------------------------------------------------------------
density <- dgev(x = c(100, 150, 200, 250, 300), para = c(location, scale, shape))
density

## -----------------------------------------------------------------------------
scale <- 565.29; shape1 <- 5.75; shape2 <- 0.13

## -----------------------------------------------------------------------------
qua <- qBrIII(u = 0.99, para = c(scale, shape1, shape2))
qua

## -----------------------------------------------------------------------------
qua <- qBrIII(RP = 100, para = c(scale, shape1, shape2))
qua

## -----------------------------------------------------------------------------
qua <- qBrIII(RP = c(5, 10, 25, 50, 100, 200), para = c(scale, shape1, shape2))
qua

## -----------------------------------------------------------------------------
scale <- 322; shape1 <- 6.22; shape2 <- 0.12
return_period <- tBrXII(x = 800, para = c(scale, shape1, shape2))
return_period

## -----------------------------------------------------------------------------
return_period <- tBrXII(x = c(500, 600, 700, 800), para = c(scale, shape1, shape2))
return_period

## ---- fig.cap = "Theoretical L-space of `BrIII` Distribution"-----------------
lspace_BrIII

## ---- fig.cap = "Theoretical L-space of `BrXII` Distribution"-----------------
lspace_BrXII

## ---- fig.cap = "Theoretical L-space of `GG` Distribution"--------------------
lspace_GG 

## ---- fig.cap = "Example of single L-points on the L-space of `BrIII` Distribution"----
com_sam_lspace(sample = FLOW_AMAX, type = "s", Dist = "BrIII")

## ---- fig.cap = "Example of single L-points on the L-space of `BrXII` Distribution"----
com_sam_lspace(sample = FLOW_AMAX, type = "s", Dist = "BrXII")

## ---- fig.cap = "Example of single L-points on the L-space of `GG` Distribution"----
com_sam_lspace(sample = FLOW_AMAX, type = "s", Dist = "GG")

## -----------------------------------------------------------------------------
colnames(FLOW_AMAX_MULT) <- paste0("site.", 1:10)
knitr::kable(head(FLOW_AMAX_MULT), caption = "The first few observations of streamflow at 10 sites")

## ---- fig.cap = "Example of multiple L-points on the L-space of `BrIII` Distribution"----
com_sam_lspace(sample = FLOW_AMAX_MULT, type = "m", Dist = "BrIII", shape = 20)

## ---- fig.cap = "Example of multiple L-points on the L-space of `BrXII` Distribution"----
com_sam_lspace(sample = FLOW_AMAX_MULT, type = "m", Dist = "BrXII", shape = 20)

## ---- fig.cap = "Example of multiple L-points on the L-space of `GG` Distribution"----
com_sam_lspace(sample = FLOW_AMAX_MULT, type = "m", Dist = "GG", shape = 20)

## -----------------------------------------------------------------------------
flags_BrIII <- con_sam_lspace(sample = FLOW_AMAX_MULT, type = "m", Dist = "BrIII")
knitr::kable(head(flags_BrIII), caption = "Flags obtained for BrIII's L-space")
flags_GG <- con_sam_lspace(sample = FLOW_AMAX_MULT, type = "m", Dist = "GG")
knitr::kable(head(flags_GG), caption = "Flags obtained for GG's L-space")

## -----------------------------------------------------------------------------
counter_BrIII <- nrow(flags_BrIII[flags_BrIII$condition == "lpoint_inside_lspace",])
paste0("the number of L-points inside the L-space of BrIII = ", counter_BrIII)
counter_GG <- nrow(flags_GG[flags_GG$condition == "lpoint_inside_lspace",])
paste0("the number of L-points inside the L-space of GG = ", counter_GG)

## -----------------------------------------------------------------------------
con_sam_lspace(sample = FLOW_AMAX, type = "s", Dist = "BrIII")

## -----------------------------------------------------------------------------
# Step 1
sample <- FLOW_AMAX
samlmoms <- get_sample_lmom(x = sample)

## -----------------------------------------------------------------------------
# Step 2
parameters <- fit_BrIII(sl1 = samlmoms$sl1, st2 = samlmoms$st2, st3 = samlmoms$st3)

## -----------------------------------------------------------------------------
# Step 3
quantile <- qBrIII(RP = c(5, 10, 25, 50, 100),
                    para = c(parameters$scale, parameters$shape1, parameters$shape2))
prob <- pBrIII(x = quantile, 
               para = c(parameters$scale, parameters$shape1, parameters$shape2))
dens <- dBrIII(x = quantile, 
               para = c(parameters$scale, parameters$shape1, parameters$shape2))
T_yrs <- tBrIII(x = quantile, 
                para = c(parameters$scale, parameters$shape1, parameters$shape2))

## -----------------------------------------------------------------------------
output <- cbind(Q = round(quantile, digits = 0), 
                CDF = round(prob, digits = 4),
                PDF = round(dens, digits = 5),
                T_yrs)
knitr::kable(output, caption = "Example of fitting BrIII distribution to FLOW_AMAX")

