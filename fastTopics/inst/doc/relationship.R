## ----knitr-opts, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment = "#",collapse = TRUE,results = "hold",
                      fig.align = "center")

## ----load-pkgs, message=FALSE, warning=FALSE----------------------------------
library(fastTopics)
library(ggplot2)
library(cowplot)

## ----fit-poisson-nmf-model, message=FALSE-------------------------------------
set.seed(1)
dat  <- simulate_count_data(80,100,3)
fit1 <- fit_poisson_nmf(dat$X,k = 3,numiter = 200,verbose = "none")
rbind(dim(dat$X),
      dim(fit1$L),
      dim(fit1$F))

## ----plot-loglik, fig.height=3, fig.width=3.5---------------------------------
quickplot(x = 1:200,y = fit1$progress$loglik,geom = c("point","line"),
  xlab = "iteration",ylab = "loglik") + theme_cowplot(12)

## ----poisson2multinom-1-------------------------------------------------------
fit.multinom <- poisson2multinom(fit1)

## ----poisson2multinom-2-------------------------------------------------------
range(rowSums(fit.multinom$L))
range(colSums(fit.multinom$F))

## ----loglik-poisson-vs-multinom, fig.height=3, fig.width=3.25-----------------
ll.pnmf     <- loglik_poisson_nmf(dat$X,fit1)
ll.multinom <- loglik_multinom_topic_model(dat$X,fit.multinom) +
               dpois(rowSums(dat$X),fit.multinom$s,log = TRUE)
quickplot(x = ll.pnmf,y = ll.multinom) + theme_cowplot(12)

## ----multinom2poisson-1, fig.height=3, fig.width=3.25-------------------------
fit.multinom["s"] <- NULL
fit2    <- multinom2poisson(fit.multinom,dat$X)
loglik1 <- loglik_poisson_nmf(dat$X,fit1)
loglik2 <- loglik_poisson_nmf(dat$X,fit2)
quickplot(x = loglik1,y = loglik2) + theme_cowplot(12)

## ----multinom2poisson-2, fig.height=3, fig.width=3.25-------------------------
s           <- rowSums(dat$X)
fit3        <- fit.multinom
fit3$L      <- s * fit3$L
class(fit3) <- c("poisson_nmf_fit","list")
loglik3     <- loglik_poisson_nmf(dat$X,fit3)
quickplot(x = loglik1,y = loglik3) + theme_cowplot(12)

