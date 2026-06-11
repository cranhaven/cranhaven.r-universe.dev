## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  fig.width = 5.75, 
  fig.height = 4.5
)

# Load packages
library("dvmisc")
library("microbenchmark")
library("printr")
library("knitr")

# Other options
set.seed(123)

## ------------------------------------------------------------------------
x <- rnorm(1000)
all.equal(diff(range(x)), truerange(x))
as.data.frame(print(microbenchmark(diff(range(x)), truerange(x), times = 500)))

## ------------------------------------------------------------------------
bmi <- round(runif(100, min = 15, max = 45), 1)
table(bmi3(bmi))
table(bmi4(bmi, labels = FALSE))

## ------------------------------------------------------------------------
MLE <- c()
s2 <- c()
for (ii in 1: 1000) {
   x <- rnorm(n = 25)
   MLE[ii] <- sum((x - mean(x))^2) / 25
   s2[ii] <- sum((x - mean(x))^2) / 24
 }
kable(sumsim(estimates = cbind(MLE, s2), truth = 1))

## ---- fig.height = 4.25, fig.width = 7.75--------------------------------
library("RcppRoll")

lengths <- c(10, 100, 1000, 10000)
multiples1 <- multiples2 <- c()
for (ii in 1: 4) {
  n <- lengths[ii]
  x <- rnorm(n)
  medians <- summary(microbenchmark(roll_mean(x, 5), moving_mean(x, 5),
                                    roll_mean(x, n / 5), moving_mean(x, n / 5),
                                    times = 50))$median
  multiples1[ii] <- medians[1] / medians[2]
  multiples2[ii] <- medians[3] / medians[4]
}
par(mfrow = c(1, 2))
plot(1: 4, multiples1, type = "b", col = "blue", main = "5-unit MA", 
     ylab = "Speed multiple", xlab = "Vector length", xaxt = "n", 
     ylim = c(0, max(multiples1) * 1.05))
axis(side = 1, at = 1: 4, labels = lengths)
abline(h = 1)

plot(1: 4, multiples2, type = "b", col = "blue", main = "length(x)/5-unit MA", 
     ylab = "Speed multiple", xlab = "Vector length", xaxt = "n", 
     ylim = c(0, max(multiples2) * 1.05))
axis(side = 1, at = 1: 4, labels = lengths)
abline(h = 1)

## ------------------------------------------------------------------------
# Histogram for 1,000 values from Bin(8, 0.25)
x <- rbinom(n = 1000, size = 5, prob = 0.25)
histo(x, dis = "binom", size = 5, colors = "blue", points_list = list(type = "b"))

# Histogram for 10,000 values from lognormal(0, 0.35) and various fitted PDFs.
x <- rlnorm(n = 10000, meanlog = 0, sdlog = 0.35)
histo(x, c("lnorm", "norm", "gamma"), main = "X ~ Lognormal(0, 0.35)")

