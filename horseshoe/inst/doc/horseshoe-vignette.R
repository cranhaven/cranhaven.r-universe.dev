## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)

## ----eval = F------------------------------------------------------------
#  install.packages("horseshoe")

## ----setup---------------------------------------------------------------
library(horseshoe)

## ----fig.width = 6, fig.height= 5----------------------------------------
tau.values <- c(0.005, 0.05, 0.5)
y.values <- seq(-5, 5, length = 100)
df <- data.frame(tau = rep(tau.values, each = length(y.values)),
                 y = rep(y.values, 3),
                 post.mean = c(HS.post.mean(y.values, tau = tau.values[1], Sigma2=1), 
                               HS.post.mean(y.values, tau = tau.values[2], Sigma2=1), 
                               HS.post.mean(y.values, tau = tau.values[3], Sigma2=1)) )

ggplot(data = df, aes(x = y, y = post.mean, group = tau, color = factor(tau))) + 
  geom_line(size = 1.5) + 
  scale_color_brewer(palette="Dark2") + 
  geom_abline(lty = 2) + geom_hline(yintercept = 0, colour = "grey") + 
  theme_classic() + ylab("") + labs(color = "Tau") +
  ggtitle("Horseshoe posterior mean for three values of tau") 
                 

## ----fig.width = 6, fig.height= 4----------------------------------------
df <- data.frame(index = 1:50,
                 truth <- c(rep(5, 10), rep(0, 40)),
                 y <- truth + rnorm(50) #observations
                 )

ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = y), size = 2, col = "blue") +
  theme_classic() + ylab("") +
  ggtitle("Black = truth, Blue = observations")


## ------------------------------------------------------------------------
(tau.est <- HS.MMLE(df$y, Sigma2 = 1))

## ----fig.width = 6, fig.height= 4----------------------------------------
post.mean <- HS.post.mean(df$y, tau.est, 1)
df$post.mean <- post.mean

ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = y), size = 2, col = "blue") +
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean), size = 2, col = "red") +
  ggtitle("Black = truth, Blue = observations, Red = estimates")

## ---- results = 'hide'---------------------------------------------------
hs.object <- HS.normal.means(df$y, method.tau = "truncatedCauchy", method.sigma = "Jeffreys")

## ----fig.width = 6, fig.height= 4----------------------------------------
df$post.mean.full <- hs.object$BetaHat

ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) + 
  geom_point(aes(x = index, y = y), size = 2, col = "blue") +
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean.full), size = 2, col = "red") +
  ggtitle("Black = truth, Blue = observations, Red = estimates")

## ----fig.width = 6, fig.height= 4----------------------------------------
df$lower.CI <- hs.object$LeftCI
df$upper.CI <- hs.object$RightCI

ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) + 
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean.full), size = 2, col = "red") +
  geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI), width = .1, col = "red") +
  ggtitle("Black = truth, Red = estimates with 95% credible intervals")

## ------------------------------------------------------------------------
df$selected.CI <- HS.var.select(hs.object, df$y, method = "intervals")

## ----fig.width = 6, fig.height= 4----------------------------------------
ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) +
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean.full, col = factor(selected.CI)), 
             size = 2) +
  geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI, col = factor(selected.CI)),
                width = .1) +
  theme(legend.position="none") +
  ggtitle("Black = truth, Blue = selected as signal, Red = selected as noise")

## ----fig.width = 6, fig.height= 4----------------------------------------
df$selected.thres <- HS.var.select(hs.object, df$y, method = "threshold")


ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) +
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean.full, col = factor(selected.thres)), 
             size = 2) +
  geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI, col = factor(selected.thres)),
                width = .1) +
  theme(legend.position="none") +
  ggtitle("Black = truth, Blue = selected as signal, Red = selected as noise")

## ------------------------------------------------------------------------
X <- matrix(rnorm(50*100), 50)
beta <- c(rep(6, 10), rep(0, 90))
y <- X %*% beta + rnorm(50)

## ----fig.width = 6, fig.height= 4----------------------------------------
hs.object <- horseshoe(y, X, method.tau = "truncatedCauchy", method.sigma ="Jeffreys")

df <- data.frame(index = 1:100,
                 truth = beta,
                 post.mean = hs.object$BetaHat,
                 lower.CI <- hs.object$LeftCI,
                 upper.CI <- hs.object$RightCI
                 )

ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) + 
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean), size = 2, col = "red") +
  geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI), width = .1, col = "red") +
  ggtitle("Black = truth, Red = estimates with 95% credible intervals")

## ------------------------------------------------------------------------
df$selected.CI <- HS.var.select(hs.object, df$y, method = "intervals")

## ----fig.width = 6, fig.height= 4----------------------------------------
ggplot(data = df, aes(x = index, y = truth)) + 
  geom_point(size = 2) +
  theme_classic() + ylab("") +
  geom_point(aes(x = index, y = post.mean, col = factor(selected.CI)), 
             size = 2) +
  geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI, col = factor(selected.CI)),
                width = .1) +
  theme(legend.position="none") +
  ggtitle("Black = truth, Blue = selected as signal, Red = selected as noise")

