## ----echo = FALSE, include = FALSE-----------------------------------------
library("drda")
options(prompt = "R> ", continue = "+ ", width = 77, digits = 4, useFancyQuotes = FALSE)

colpal <- c(
  "#000000", "#004949", "#009292", "#ff6db6", "#ffb6db",
  "#490092", "#006ddb", "#b66dff", "#6db6ff", "#b6dbff",
  "#920000", "#924900", "#db6d00", "#24ff24", "#ffff6d"
)

set.seed(4336273)

## ----logistic5, echo = FALSE, fig.height = 10, fig.width = 10, fig.cap = "Generalized (5-parameter) logistic function with various choices of parameters."----
fnl5 <- function(x, y) {
  a <- y[1]
  d <- y[2]
  e <- y[3]
  p <- y[4]
  n <- y[5]

  a + d * (1 + n * exp(-e * (x - p)))^(-1 / n)
}

old_par <- par(mfrow = c(2, 2))

curve(
  fnl5(x, c(0.9, -0.8, 1, 0, 1)), from = -10, to = 10, col = colpal[1],
  ylab = expression(paste("f(x;", psi, ")")), xlab = "x", ylim = c(0, 1),
  main = expression(paste("Varying ", phi, " parameter", sep = ""))
)
mtext(expression(paste(alpha, "= 0.9,", delta, "= -0.8,", eta, "= 1,", nu, "= 1")))
curve(fnl5(x, c(0.9, -0.8, 1, 5, 1)), add = TRUE, col = colpal[2])
curve(fnl5(x, c(0.9, -0.8, 1, 2, 1)), add = TRUE, col = colpal[3])
curve(fnl5(x, c(0.9, -0.8, 1, -2, 1)), add = TRUE, col = colpal[4])
curve(fnl5(x, c(0.9, -0.8, 1, -5, 1)), add = TRUE, col = colpal[5])
abline(h = c(0.1, 0.9), lty = 2)
legend(
  "topright", title = expression(phi), legend = c(5, 2, 0, -2, -5),
  col = colpal[c(2:3, 1, 4:5)], lty = 1, bg = "white"
)

curve(
  fnl5(x, c(0.95, -0.9, 1, 0, 1)), from = -10, to = 10, col = colpal[1],
  ylab = expression(paste("f(x;", psi, ")")), xlab = "x", ylim = c(0, 1),
  main = expression(paste("Varying ", nu, " parameter", sep = ""))
)
mtext(expression(paste(alpha, "= 0.95,", delta, "= -0.9,", eta, "= 1,", phi, "= 0")))
curve(fnl5(x, c(0.95, -0.9, 1, 0, 0.1)), add = TRUE, col = colpal[2])
curve(fnl5(x, c(0.95, -0.9, 1, 0, 0.5)), add = TRUE, col = colpal[3])
curve(fnl5(x, c(0.95, -0.9, 1, 0, 2.5)), add = TRUE, col = colpal[4])
curve(fnl5(x, c(0.95, -0.9, 1, 0, 5)), add = TRUE, col = colpal[5])
abline(h = c(0.05, 0.95), lty = 2)
legend(
  "topright", title = expression(nu), legend = c(0.1, 0.5, 1, 2.5, 5),
  col = colpal[c(2:3, 1, 4:5)], lty = 1, bg = "white"
)

curve(
  fnl5(x, c(1, -1, 1, 0, 1)), from = -10, to = 10, col = colpal[1],
  ylab = expression(paste(mu, "(x;", psi, ")")), xlab = "x", ylim = c(0, 1),
  main = expression(paste("Varying ", eta, " parameter", sep = ""))
)
mtext(expression(paste(alpha, "= 1,", delta, "= -1,", phi, "= 0,", nu, "= 1")))
curve(fnl5(x, c(1, -1, 0.25, 0, 1)), add = TRUE, col = colpal[2])
curve(fnl5(x, c(1, -1, 0.5, 0, 1)), add = TRUE, col = colpal[3])
curve(fnl5(x, c(1, -1, 2, 0, 1)), add = TRUE, col = colpal[4])
curve(fnl5(x, c(1, -1, 5, 0, 1)), add = TRUE, col = colpal[5])
abline(h = c(0, 1), lty = 2)
legend(
  "topright", title = expression(eta), legend = c(0.25, 0.5, 1, 2, 5),
  col = colpal[c(2:3, 1, 4:5)], lty = 1, bg = "white"
)

curve(
  fnl5(x, c(0, 0, 1, 0, 1)), from = -10, to = 10, col = colpal[1],
  ylab = expression(paste(mu, "(x;", psi, ")")), xlab = "x", ylim = c(-1, 1),
  main = expression(paste("Varying ", delta, " parameter", sep = ""))
)
mtext(expression(paste(alpha, "= 0,", eta, "= 1,", phi, "= 0,", nu, "= 1")))
curve(fnl5(x, c(0, -1, 1, 0, 1)), add = TRUE, col = colpal[2])
curve(fnl5(x, c(0, -0.5, 1, 0, 1)), add = TRUE, col = colpal[3])
curve(fnl5(x, c(0, 0.5, 1, 0, 1)), add = TRUE, col = colpal[4])
curve(fnl5(x, c(0, 1, 1, 0, 1)), add = TRUE, col = colpal[5])
abline(h = c(-1, 1), lty = 2)
legend(
  "bottomright", title = expression(delta), legend = c(-1, -0.5, 0, 0.5, 1),
  col = colpal[c(2:3, 1, 4:5)], lty = 1, bg = "white"
)

par(old_par)

## ----objfnproblem, echo = FALSE, fig.height = 6, fig.width = 10, fig.cap = "Problematic real data (cell line: BT-20, compound: BI-2536, dataset: CTRPv2) \\citep{rees_2016_ncb_correlating, seashore-ludlow_2015_cd_harnessing, basu_2013_cell_interactive}. A) 4-parameter logistic function as fitted by the BFGS algorithm. Starting point \\(\\boldsymbol{\\psi} = (\\alpha, \\delta, \\eta, \\phi)^{\\top} = (1, -1, 1, 0)^{\\top}\\). B) Contour plot of the residual sum of squares \\(g(\\boldsymbol{\\psi})\\) with respect to parameters \\(\\eta\\) and \\(\\phi\\). Fixed parameters \\(\\alpha = 1\\) and \\(\\delta = -1\\)."----
fig_x <- c(
  -6.90775527898214, -6.21460809842219, -5.49676830527187, -4.81589121730374,
  -4.13516655674236, -3.44201937618241, -2.7333680090865, -2.04022082852655,
  -1.34707364796661, -0.653926467406664, 0, 0.741937344729377,
  1.43508452528932, 2.11625551480255, 2.83321334405622, 3.49650756146648
)

fig_y <- c(
  0.9953, 1.074, 0.6401, 0.5836,
  0.5796, 0.6442, 0.5219, 0.625,
  0.5991, 0.652, 0.6246, 0.6743,
  0.577, 0.6559, 0.5197, 0.1061
)

fig_fn <- function(x, y) {
  y[1] + y[2] / (1 + exp(-y[3] * (x - y[4])))
}

fig_rss <- function(x) {
  mu <- fig_fn(fig_x, x)
  sum((fig_y - mu)^2) / 2
}

# paste(drda(fig_y ~ fig_x)$coefficients, collapse = ", ")

fig_theta_drda <- c(
  1.03465000001277, -0.468265384629308, 38.4804130263685, -5.540362804892
)

# paste(optim(c(1, -1, 1, 0), fig_rss)$par, collapse = ", ")

fig_theta_optim <- c(
  5.3683688666083, -9.1123759209514, 0.0181568606688768, -6.11160283470708
)

N <- 400
eta_set <- seq(0, 2, length.out = N)
phi_set <- seq(-20, 20, length.out = N)
rss_val <- matrix(
  apply(expand.grid(eta_set, phi_set), 1, function(x) fig_rss(c(1, -1, x))),
  nrow = N, ncol = N
)

old_par <- par(mfrow = c(1, 2))

plot(
  fig_x, fig_y, type = "p", xlab = "log(dose)", ylab = "Percent viability",
  ylim = c(0, 1.2)
)
curve(
  fig_fn(x, fig_theta_drda),
  add = TRUE, lty = 2, lwd = 2, col = "#EE6677FF"
)
curve(
  fig_fn(x, fig_theta_optim),
  add = TRUE, lty = 2, lwd = 2, col = "#4477AAFF"
)
legend(
  "bottomleft", legend = c("True estimate", "BFGS"), lty = 2, lwd = 2,
  bg = "white", col = c("#EE6677FF", "#4477AAFF"), bty = "n"
)
title("A)", adj = 0)

contour(
  x = eta_set, y = phi_set, z = rss_val,
  levels = c(0.2, 0.4, 0.6, 0.8, 1.0, 1.5, 3.0, 3.4),
  xlab = expression(eta), ylab = expression(phi),
)
title("B)", adj = 0)

par(old_par)

## --------------------------------------------------------------------------
dose <- rep(c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100), each = 3)
relative_viability <- c(
  0.877362, 0.812841, 0.883113, 0.873494, 0.845769, 0.999422, 0.888961,
  0.735539, 0.842040, 0.518041, 0.519261, 0.501252, 0.253209, 0.083937,
  0.000719, 0.049249, 0.070804, 0.091425, 0.041096, 0.000012, 0.092564
)

## --------------------------------------------------------------------------
library("drda")
fit_ll4 <- drda(relative_viability ~ dose, mean_function = "loglogistic4")

## --------------------------------------------------------------------------
log_dose <- log(dose)
fit_l4 <- drda(relative_viability ~ log_dose)

## --------------------------------------------------------------------------
test_data <- data.frame(d = dose, x = log_dose, y = relative_viability)

fit_ll4 <- drda(relative_viability ~ dose, mean_function = "loglogistic4")
fit_ll4 <- drda(y ~ d, data = test_data, mean_function = "loglogistic4")
fit_ll4 <- drda(y ~ d, data = test_data, mean_function = "ll4")

fit_l4 <- drda(relative_viability ~ log_dose)
fit_l4 <- drda(y ~ x, data = test_data)
fit_l4 <- drda(y ~ x, data = test_data, mean_function = "l4")

## --------------------------------------------------------------------------
summary(fit_l4)

## --------------------------------------------------------------------------
coef(fit_l4) # or fit_l4$coefficients

## --------------------------------------------------------------------------
sigma(fit_l4) # or fit_l4$sigma

## --------------------------------------------------------------------------
coef(fit_ll4) # or fit_ll4$coefficients

## --------------------------------------------------------------------------
deviance(fit_l4)
vcov(fit_l4)
residuals(fit_l4)
logLik(fit_l4)
predict(fit_l4)
predict(fit_l4, newdata = log(c(0.002, 0.2, 2)))

## --------------------------------------------------------------------------
fit_l2 <- drda(y ~ x, data = test_data, mean_function = "logistic2")
anova(fit_l2)

## --------------------------------------------------------------------------
fit_gz <- drda(y ~ x, data = test_data, mean_function = "gompertz")
fit_l4 <- drda(y ~ x, data = test_data, mean_function = "logistic4")
anova(fit_l2, fit_gz, fit_l4)

## --------------------------------------------------------------------------
weights <- c(
  0.990868, 1.095238, 0.974544, 0.973318, 1.107001, 1.012844, 1.052806,
  1.019427, 1.032544, 0.919827, 0.971385, 0.959019, 1.037789, 1.006835,
  0.969383, 0.935633, 1.016597, 1.011085, 0.982307, 1.066032, 0.959870
)
fit_wl4 <- drda(y ~ x, data = test_data, weights = weights)
summary(fit_wl4)

## --------------------------------------------------------------------------
weights(fit_wl4)
residuals(fit_wl4, type = "weighted")

## --------------------------------------------------------------------------
lb <- c(1, -1, 0, -Inf)
ub <- c(1, -1, 5,  Inf)
fit_cnstr <- drda(
  y ~ x, data = test_data, lower_bound = lb, upper_bound = ub
)
summary(fit_cnstr)

## --------------------------------------------------------------------------
fit_cnstr <- drda(
  y ~ x, data = test_data, lower_bound = lb, upper_bound = ub,
  start = c(1, -1, 0.6, -2), max_iter = 10000
)

## ----plot_logi5, fig.pos = "H", fig.height = 4, fig.width = 9, fig.cap = ""----
fit_l5 <- drda(y ~ x, data = test_data, mean_function = "logistic5")
plot(fit_l5)

## ----plot_multi, fig.pos = "H", fig.height = 4, fig.width = 9, fig.cap = ""----
plot(
  fit_l2, fit_l4, fit_gz, base = "10", level = 0.9, xlim = c(-10, 5),
  ylim = c(-0.1, 1.1), xlab = "Dose", ylab = "Relative viability",
  cex = 0.9, legend = c("2-p logistic", "4-p logistic", "Gompertz")
)

## ----plot_log_multi, fig.pos = "H", fig.height = 4, fig.width = 9, fig.cap = ""----
fit_ll2 <- drda(y ~ d, data = test_data, mean_function = "loglogistic2")
fit_lgz <- drda(y ~ d, data = test_data, mean_function = "loggompertz")
plot(
  fit_ll2, fit_ll4, fit_lgz, base = "10", level = 0.9,
  xlim = c(0, 100), ylim = c(-0.1, 1.1), xlab = "Dose",
  ylab = "Relative viability", cex = 0.9,
  legend = c("2-p log-logistic", "4-p log-logistic", "log-Gompertz")
)

## --------------------------------------------------------------------------
naac(fit_l4)

## --------------------------------------------------------------------------
naac(fit_l4, xlim = c(-2, 2), ylim = c(0.1, 0.9))

## --------------------------------------------------------------------------
effective_dose(fit_l4, y = c(0.75, 0.95))

## --------------------------------------------------------------------------
effective_dose(fit_l4, y = c(0.75, 0.95), type = "absolute")

