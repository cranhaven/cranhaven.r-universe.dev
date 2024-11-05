## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(faux)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot) # for multi-panel plots

## -----------------------------------------------------------------------------
dat <- sim_design(within = list(vars = c("dv", "predictor")),
                mu = list(dv = 100, predictor = 0),
                sd = list(dv = 10, predictor = 1),
                r = 0.5, plot = FALSE)

## ----echo = FALSE-------------------------------------------------------------
ggplot(dat, aes(predictor, dv)) + 
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

## -----------------------------------------------------------------------------
dat <- sim_design(within = list(vars = c("dv", "pred1", "pred2")),
                mu = list(dv = 100, pred1 = 0, pred2 = 0),
                sd = list(dv = 10, pred1 = 1, pred2 = 1),
                r = c(0.5, 0, -0.2), plot = FALSE)

## ----echo = FALSE-------------------------------------------------------------
d1 <- ggplot(dat, aes(pred1, dv)) + 
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

d2 <- ggplot(dat, aes(pred2, dv)) + 
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

pred <- ggplot(dat, aes(pred1, pred2)) + 
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

cowplot::plot_grid(d1, d2, pred, ncol = 3)

## -----------------------------------------------------------------------------
#      pre_pred, post_dv, post_pred
r <- c(     0.0,     0.8,        NA, # pre_dv
                     0.0,       0.3, # pre_pred
                                0.5) # post_dv
                                        
lim <- faux::pos_def_limits(r)
r[[3]] <- mean(c(lim$min, lim$max))

dat <- sim_design(within = list(time = c("pre", "post"),
                                vars = c("dv", "pred")),
                mu = list(pre_dv = 100, pre_pred = 0, 
                          post_dv = 110, post_pred = 0.1),
                sd = list(pre_dv = 10, pre_pred = 1,
                          post_dv = 10, post_pred = 1),
                r = r, plot = FALSE)

## -----------------------------------------------------------------------------
long_dat <- dat %>%
  pivot_longer(cols = -id, names_to = "var", values_to = "value") %>%
  separate(var, c("time", "var")) %>%
  pivot_wider(names_from = var, values_from = value)

## ----within-cont, echo = FALSE------------------------------------------------
ggplot(long_dat, aes(pred, dv, color = time)) +
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

## -----------------------------------------------------------------------------
dat <- sim_design(between = list(group = c("A", "B")),
                  within = list(vars = c("dv", "predictor")),
                  mu = list(A = c(dv = 100, predictor = 0),
                            B = c(dv = 110, predictor = 0)),
                  sd = list(A = c(dv =  10, predictor = 1),
                            B = c(dv =  10, predictor = 1)),
                  r  = list(A = 0.5, B = 0), plot = FALSE)

## ----cont-cat, echo = FALSE---------------------------------------------------
ggplot(dat, aes(predictor, dv, color = group)) + 
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

## -----------------------------------------------------------------------------
dat <- sim_design(between = list(group = c("A", "B")), 
                  mu = list(A = 100, B = 120), sd = 10, plot = FALSE)

## -----------------------------------------------------------------------------
dat$pred <- rnorm_pre(dat$y, 0, 1, 0.5)

## ----add-pred, echo = FALSE---------------------------------------------------
ggplot(dat, aes(pred, y, color = group)) +
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

## -----------------------------------------------------------------------------
A <- filter(dat, group == "A") %>%
  mutate(pred = rnorm_pre(y, 0, 1, -0.5))
B <- filter(dat, group == "B") %>%
  mutate(pred = rnorm_pre(y, 0, 1, 0.5))

dat <- bind_rows(A, B)

## ----echo = FALSE-------------------------------------------------------------
ggplot(dat, aes(pred, y, color = group)) +
  geom_point() + geom_smooth(formula = 'y~x', method = lm)

## -----------------------------------------------------------------------------
dat <- sim_design(2, r = 0.5, plot = FALSE)
dat$B <- rnorm_pre(dat[, 2:3], r = c(A1 = 0.5, A2 = 0))
cor(dat[, 2:4])

## ---- error = TRUE, perl = FALSE----------------------------------------------
dat$C <- rnorm_pre(dat[, 2:4], r = c(A1 = 0.9, A2 = 0.9, B = -0.9))

