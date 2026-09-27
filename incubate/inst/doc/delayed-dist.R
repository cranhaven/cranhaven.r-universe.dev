## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("incubate")

library("dplyr")
library("ggplot2")
library("patchwork")

## ----plot_logit_transf, echo=FALSE, fig.align='center', fig.width=8, fig.height=3----
x1 <- 7

ggplot() + xlim(0, 7) +
  geom_function(fun = \(alpha1) -log1p((x1 - 2 * alpha1)/alpha1), n = 1001) +
  labs(x = expression(alpha), y = expression(tilde(alpha))) |

  ggplot() + xlim(-9, 9) +
  geom_function(fun = \(alpha1_tr) x1 / (expm1(-alpha1_tr)+2), n = 1001) +
  labs(x = expression(tilde(alpha)), y = expression(alpha),
       caption = bquote("transformation for data with " * x[(1)] == .(x1)))

## -----------------------------------------------------------------------------
(x <- rexp_delayed(n = 24, delay1 = 5, rate1 = .2, cens = .3))
class(x)
sum(x[,2] == 0)/length(x)

## -----------------------------------------------------------------------------
(x1 <- rweib_delayed(n = 24, delay1 = 5, scale1 = 3.5, shape1 = 1.7, cens = .3))
class(x1)
sum(x1[,2] == 0) / length(x1)

(x2 <- rweib_delayed(n = 24, delay1 = 5, scale1 = 3.5, shape1 = .4, cens = .3))
sum(x2[,2] == 0) / length(x2)

survival::survfit(c(x1, x2) ~ rep(c("x1","x2"), each = 24)) |> 
  plot(mark.time=T, conf.int=F, lty = 1:2, col = 1:2)
  

