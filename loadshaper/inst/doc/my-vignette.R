## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ch1----------------------------------------------------------------------
library(loadshaper)

## ----flow, echo=FALSE, fig.cap="Load Shape Scaling Process \\label{fig:flow}", out.width = '80%'----
knitr::include_graphics("flow.png")

## ----flow2, echo=FALSE, fig.cap="Load Shape Scaling Process Using Logistic Method \\label{fig:flow2}", out.width = '100%'----
knitr::include_graphics("flow2.png")

## -----------------------------------------------------------------------------
library(loadshaper)
# load ERCOT COAST load for the year 2019
loads <- ercot[ercot$Year == 2019, ]$COAST
# simple line plot
plot(loads, type = "l", col = "salmon", ylab = "Load (MW)")
grid()

## -----------------------------------------------------------------------------
lin_loadshape <- lslin(loads, target_lf = 0.5, 
                          target_max = 5000)
print(class(lin_loadshape))

## -----------------------------------------------------------------------------
summary(lin_loadshape)

## -----------------------------------------------------------------------------
# ordered per unit load
x_opu <- sort(loads / max(loads), decreasing = TRUE)
n <- length(x_opu)

# minimum possible target load factor
min_lf <- (sum(x_opu) - mean(c(1:n) * x_opu))/(n-1)
print(round(min_lf, 3))


## -----------------------------------------------------------------------------
# setting target load factor to theoretical minimum
summary(lslin(loads, target_lf = min_lf))


# setting target load factor less than theoretical minimum
summary(lslin(loads, target_lf =  0.2))                       

## -----------------------------------------------------------------------------
# scenario 1
summary(lslin(loads, target_lf = 0.65))

## -----------------------------------------------------------------------------
# scenario 2
summary(lslin(loads, target_lf = 0.95))

## -----------------------------------------------------------------------------
log_loadshape <- lslog(loads, target_lf = 0.5, target_max = 100)
print(class(log_loadshape))

## -----------------------------------------------------------------------------
summary(log_loadshape)

## -----------------------------------------------------------------------------
summary(lslog(loads, target_lf = 0.9))

## -----------------------------------------------------------------------------
# scatter plot, per unit load
plot(lin_loadshape, scatter = TRUE)

## -----------------------------------------------------------------------------
# scatter plot, actual load
plot(lin_loadshape, scatter = TRUE, case = 3)

## -----------------------------------------------------------------------------
# per unit load duration curve
plot(log_loadshape, case = 1)

## -----------------------------------------------------------------------------
# per unit load d
plot(log_loadshape, case = 2)

## -----------------------------------------------------------------------------
# linear method, acf
print(lscore(lin_loadshape, type = "acf"))

## -----------------------------------------------------------------------------
# logistic method, acf
print(lscore(log_loadshape, type = "acf"))

## -----------------------------------------------------------------------------
# linear method, pacf
print(lscore(lin_loadshape, type = "pacf"))

## -----------------------------------------------------------------------------
# logistic method, pacf
print(lscore(log_loadshape, type = "pacf"))

