library(MASS)
set.seed(1)
dat <- mvrnorm(5000, c(0, 0), matrix(c(1, .5, .5, 1), 2))
## True kendall's tau is 2 * asin(.5) / pi
system.time(print(kendall(dat)))
system.time(print(cor(dat, method = "kendall")))
