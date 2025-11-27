## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 5
)

## ----load_libraries, echo = FALSE---------------------------------------------
library(registr)
have_ggplot2 = requireNamespace("ggplot2", quietly = TRUE)
if (have_ggplot2) {
  library(ggplot2)
  theme_set(theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))
}

## ----Berkeley data------------------------------------------------------------
dat = registr::growth_incomplete

# sort the data by the amount of trailing incompleteness
ids    = levels(dat$id)
dat$id = factor(dat$id, levels = ids[order(sapply(ids, function(curve_id) {
	max(dat$index[dat$id == curve_id])
}))])

if (have_ggplot2) {
  # spaghetti plot
  ggplot(dat, aes(x = index, y = value, group = id)) +
    geom_line(alpha = 0.2) +
    xlab("t* [observed]") + ylab("Derivative") +
    ggtitle("First derivative of growth curves")
}

## ----Berkeley data 2 lasagna, fig.height = 5.5--------------------------------
if (have_ggplot2) {
  ggplot(dat, aes(x = index, y = id, col = value)) + 
    geom_line(lwd = 2.5) +
    scale_color_continuous("Derivative", high = "midnightblue", low = "lightskyblue1") +
    xlab("t* [observed]") + ylab("curve") +
    ggtitle("First derivative of growth curves") +
    theme(panel.grid  = element_blank(),
          axis.text.y = element_blank())
}

## ----application 1------------------------------------------------------------
reg1 = registr(Y = dat, family = "gaussian")

if (have_ggplot2) {
  ggplot(reg1$Y, aes(x = tstar, y = index, group = id)) + 
    geom_line(alpha = 0.2) +
    xlab("t* [observed]") + ylab("t [registered]") +
    ggtitle("Estimated warping functions")
}

## ----application 1 lasagna, fig.height = 5.5----------------------------------
if (have_ggplot2) {
  ggplot(reg1$Y, aes(x = index, y = id, col = value)) + 
    geom_line(lwd = 2.5) +
    scale_color_continuous("Derivative", high = "midnightblue", low = "lightskyblue1") +
    xlab("t [registered]") + ylab("curve") +
    ggtitle("Registered curves") +
    theme(panel.grid  = element_blank(),
          axis.text.y = element_blank())
}

## ----application 1 spaghetti--------------------------------------------------
if (have_ggplot2) {
  ggplot(reg1$Y, aes(x = index, y = value, group = id)) +
    geom_line(alpha = 0.3) +
    xlab("t [registered]") + ylab("Derivative") +
    ggtitle("Registered curves")
}

## ----application 2------------------------------------------------------------
reg2 = registr(Y = dat, family = "gaussian",
							 incompleteness = "full", lambda_inc = 0)

if (have_ggplot2) {
  ggplot(reg2$Y, aes(x = tstar, y = index, group = id)) + 
    geom_line(alpha = 0.2) +
    xlab("t* [observed]") + ylab("t [registered]") +
    ggtitle("Estimated warping functions")
}

## ----application 2 lasagna, fig.height = 5.5----------------------------------
if (have_ggplot2) {
  ggplot(reg2$Y, aes(x = index, y = id, col = value)) + 
    geom_line(lwd = 2.5) +
    scale_color_continuous("Derivative", high = "midnightblue", low = "lightskyblue1") +
    xlab("t [registered]") + ylab("curve") +
    ggtitle("Registered curves") +
    theme(panel.grid  = element_blank(),
          axis.text.y = element_blank())
}

## ----application 2 spaghetti--------------------------------------------------
if (have_ggplot2) {
  ggplot(reg2$Y, aes(x = index, y = value, group = id)) +
    geom_line(alpha = 0.3) +
    xlab("t [registered]") + ylab("Derivative") +
    ggtitle("Registered curves")
}

## ----application 3------------------------------------------------------------
reg3 = registr(Y = dat, family = "gaussian",
							 incompleteness = "full", lambda_inc = 5)

if (have_ggplot2) {
  ggplot(reg3$Y, aes(x = tstar, y = index, group = id)) + 
    geom_line(alpha = 0.2) +
    xlab("t* [observed]") + ylab("t [registered]") +
    ggtitle("Estimated warping functions")
}

## ----application 3 lasagna, fig.height = 5.5----------------------------------
if (have_ggplot2) {
  ggplot(reg3$Y, aes(x = index, y = id, col = value)) + 
    geom_line(lwd = 2.5) +
    scale_color_continuous("Derivative", high = "midnightblue", low = "lightskyblue1") +
    xlab("t [registered]") + ylab("curve") +
    ggtitle("Registered curves") +
    theme(panel.grid  = element_blank(),
          axis.text.y = element_blank())
}

## ----application 3 spaghetti--------------------------------------------------
if (have_ggplot2) {
  ggplot(reg3$Y, aes(x = index, y = value, group = id)) +
    geom_line(alpha = 0.3) +
    xlab("t [registered]") + ylab("Derivative") +
    ggtitle("Registered curves")
}

## ----application 4------------------------------------------------------------
reg4 = registr(Y = dat, family = "gaussian",
							 incompleteness = "full", lambda_inc = .025)

if (have_ggplot2) {
  ggplot(reg4$Y, aes(x = tstar, y = index, group = id)) + 
    geom_line(alpha = 0.2) +
    xlab("t* [observed]") + ylab("t [registered]") +
    ggtitle("Estimated warping functions")
}

## ----application 4 joint------------------------------------------------------
reg4_joint = register_fpca(Y = dat, family = "gaussian",
                           incompleteness = "full", lambda_inc = .025,
                           npc = 4)

## ----application 4 joint spaghetti--------------------------------------------
if (have_ggplot2) {
  ggplot(reg4_joint$Y, aes(x = t_hat, y = value, group = id)) +
    geom_line(alpha = 0.3) +
    xlab("t [registered]") + ylab("Derivative") +
    ggtitle("Registered curves")
}

## ----application 4 joint FPC plot, fig.height=6, fig.width=7------------------
if (have_ggplot2) {
  plot(reg4_joint$fpca_obj)
}

