## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  fig.width = 7,
  fig.height = 5
)

library(kDGLM)
# devtools::load_all()

## ----echo=FALSE---------------------------------------------------------------
data.year <- data.frame(
  Date = unique(gastroBR$Date),
  Admissions = NA,
  Population = NA
)
for (i in seq_along(data.year$Date)) {
  index <- gastroBR$Date == data.year$Date[i]
  data.year$Admissions[i] <- sum(gastroBR$Admissions[index])
  data.year$Population[i] <- sum(gastroBR$Population[index])
}

plot(data.year$Date, log10(data.year$Admissions / data.year$Population),
  type = "l", ylab = "log Admissions rate", xlab = "Date"
)

## ----echo=TRUE----------------------------------------------------------------
structure <- polynomial_block(
  rate = 1, order = 2, D = c(0.95, 0.975),
  name = "Trend"
)

## ----echo=TRUE----------------------------------------------------------------
outcome <- Poisson(
  lambda = "rate",
  data = data.year$Admissions,
  offset = data.year$Population
)

## ----echo=TRUE----------------------------------------------------------------
fitted.model <- fit_model(structure, outcome)

## ----echo=TRUE----------------------------------------------------------------
summary(fitted.model)
plot(fitted.model, plot.pkg = "base")

## ----echo=TRUE----------------------------------------------------------------
structure <- polynomial_block(
  rate = 1, order = 2, D = c(0.95, 0.975),
  name = "Trend"
) +
  harmonic_block(
    rate = 1, period = 12, D = 0.98,
    name = "Season"
  )

## ----echo=TRUE----------------------------------------------------------------
fitted.model <- fit_model(structure, outcome)

## ----echo=TRUE----------------------------------------------------------------
summary(fitted.model)
plot(fitted.model, plot.pkg = "base")

## ----echo=TRUE----------------------------------------------------------------
structure <- polynomial_block(
  rate = 1, order = 2, D = c(0.95, 0.975),
  name = "Trend"
) +
  harmonic_block(
    rate = 1, period = 12, D = 0.98,
    name = "Season"
  ) +
  noise_block(rate = 1, name = "Noise")

## ----echo=TRUE----------------------------------------------------------------
structure <- polynomial_block(
  rate = 1, order = 2, D = c(0.95, 0.975),
  name = "Trend", monitoring = c(TRUE, TRUE)
) +
  harmonic_block(
    rate = 1, period = 12, D = 0.98,
    name = "Season"
  ) +
  noise_block(rate = 1, name = "Noise")

## ----echo=TRUE----------------------------------------------------------------
# To activate the automated monitoring it is enough to set the p.monit argument to a valid value
fitted.model <- fit_model(structure, outcome, p.monit = 0.05)
summary(fitted.model)
plot(fitted.model, plot.pkg = "base")

## ----results='hide'-----------------------------------------------------------
structure <- polynomial_block(
  rate = 1, order = 2, D = c(0.95, 0.975),
  name = "Trend"
) +
  harmonic_block(
    rate = 1, period = 12, D = 0.98,
    name = "Season"
  ) +
  noise_block(rate = 1, R1 = "H", name = "Noise") # Setting the initial variance as a unknown parameter

structure <- structure |>
  intervention(time = 124, var.index = c(1:2), D = 0.005)

search.model <- fit_model(
  structure, outcome,
  H = seq.int(0, 0.04, l = 101),
  metric.cutoff = 0
)
fitted.model <- search.model$model

## -----------------------------------------------------------------------------
summary(fitted.model)

## -----------------------------------------------------------------------------
plot(fitted.model, plot.pkg = "base")

## -----------------------------------------------------------------------------
search.result <- search.model$search.data[order(search.model$search.data$H), ]

H.vals <- search.result$H
log.prior <- dgamma(H.vals, 1, 1, log = TRUE)
log.like <- search.result$log.like
l.fx <- log.prior + log.like
pre.fx <- exp(l.fx - max(l.fx))
fx <- pre.fx / sum(pre.fx * (H.vals[2] - H.vals[1]))
plot(H.vals, fx,
  type = "l", xlab = "H", ylab = "Density",
  main = "Posterior density for the unknown hyperparameter H"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
require(geobr)
require(tidyverse)
require(sf)
require(spdep)

br.base <- read_state(
  year = 2019,
  showProgress = FALSE
)

plot.data <- br.base |>
  left_join(
    gastroBR |>
      filter(format(Date, "%Y") == "2019") |>
      select(UF, Population, Admissions) |>
      group_by(UF) |>
      summarize(
        Population = max(Population),
        Admissions = sum(Admissions)
      ) |>
      rename(abbrev_state = UF),
    by = "abbrev_state"
  )

(ggplot() +
  geom_sf(data = plot.data, aes(fill = log10(Admissions / Population))) +
  scale_fill_distiller(expression(log[10] * "(admissions/population)"),
    limits = c(-4, -2.5),
    palette = "RdYlBu",
    labels = ~ round(., 2)
  ) +
  theme_void() +
  theme(legend.position = "bottom"))

## ----results='hide'-----------------------------------------------------------
adj.matrix <- br.base |>
  poly2nb() |>
  nb2mat(style = "B")

CAR.structure <- polynomial_block(rate = 1, D = 0.98, name = "CAR") |>
  block_mult(27) |>
  block_rename(levels(gastroBR$UF)) |>
  zero_sum_prior() |>
  CAR_prior(scale = "Scale", rho = 1, adj.matrix = adj.matrix)

shared.structure <- polynomial_block(
  RO = 1, AC = 1, AM = 1, RR = 1, PA = 1, AP = 1,
  TO = 1, MA = 1, PI = 1, CE = 1, RN = 1, PB = 1,
  PE = 1, AL = 1, SE = 1, BA = 1, MG = 1, ES = 1,
  RJ = 1, SP = 1, PR = 1, SC = 1, RS = 1, MS = 1,
  MT = 1, GO = 1, DF = 1,
  order = 2, D = c(0.95, 0.95),
  name = "Common"
) |>
  intervention(time = 124, var.index = c(1:2), D = 0.005)

base.structure <- (harmonic_block(rate = 1, order = 1, period = 12, D = 0.98, name = "Season") +
  noise_block(rate = 1, R1 = 0.007, name = "Noise")) |>
  block_mult(27) |>
  block_rename(levels(gastroBR$UF))

inputs <- list(shared.structure, CAR.structure, base.structure)
for (uf in levels(gastroBR$UF)) {
  reg.data <- gastroBR |> filter(UF == uf)
  inputs[[uf]] <- Poisson(lambda = uf, data = reg.data$Admissions, offset = reg.data$Population)
}
# inputs$Scale <- 10**seq.int(-5, 1, l = 21)
inputs$Scale <- 0.01
model.search <- do.call(fit_model, inputs)
# fitted.model <- model.search$model
fitted.model <- model.search

## -----------------------------------------------------------------------------
plot(fitted.model)

## ----fig.height=10, fig.width=7, warning=FALSE, fig.cap='The time series of hospital admissions by gastroenteritis of some Brazilian states from 2010 to 2022. Notice that the proposed model can capture the general behavior of all series, while simultaneously capturing the dependence between regions through the shared component $\\theta_{1,t}$ and the local effects $S_i$.'----
(plot(fitted.model, outcomes = c("MG", "SP", "ES", "RJ", "CE", "BA", "RS", "SC", "AM", "AC"), lag = 1, plot.pkg = "ggplot2") +
  scale_color_manual("", values = rep("black", 10)) +
  scale_fill_manual("", values = rep("black", 10)) +
  facet_wrap(~Serie, ncol = 2, scale = "free_y") +
  coord_cartesian(ylim = c(NA, NA)) +
  guides(color = "none", fill = "none") +
  theme(legend.position = "top"))

## ----eval=FALSE, fig.height=12, include=FALSE---------------------------------
# (plot(fitted.model, outcomes = c("MG", "SP", "ES", "RJ", "CE", "BA", "RS", "SC", "AM", "AC"), lag = -1, plot.pkg = "ggplot2") +
#   scale_color_manual("", values = rep("black", 10)) +
#   scale_fill_manual("", values = rep("black", 10)) +
#   facet_wrap(~Serie, ncol = 2, scale = "free_y") +
#   coord_cartesian(ylim = c(NA, NA)) +
#   guides(color = "none", fill = "none") +
#   theme(legend.position = "top")) |> save.fig(file = "vignettes/plot23.pdf")

## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, echo=TRUE-------
# CAR.var.index <- which(grepl("CAR", fitted.model$var.labels))
# get_map <- function(index) {
#   plot.data <-
#     cbind(
#       Date = gastroBR$Date[index],
#       Effect = fitted.model$mts[1, index] + fitted.model$mts[CAR.var.index, index],
#       br.base
#     )
# 
#   (ggplot() +
#     geom_sf(data = plot.data, aes(fill = Effect)) +
#     ggtitle(format(gastroBR$Date[index], "%m, %Y")) +
#     scale_fill_distiller("", limits = c(-2, 2), palette = "RdYlBu") +
#     theme_void())
# }
# 
# for (i in 1:fitted.model$t) {
#   get_map(index = i)
# }

## ----message=FALSE, warning=FALSE, eval=FALSE, include=FALSE------------------
# CAR.var.index <- which(grepl("CAR", fitted.model$var.labels))
# get_map <- function(index) {
#   plot.data <-
#     cbind(
#       Date = gastroBR$Date[index],
#       Effect = fitted.model$mts[1, index] + fitted.model$mts[CAR.var.index, index],
#       br.base
#     )
# 
#   (ggplot() +
#     geom_sf(data = plot.data, aes(fill = Effect)) +
#     ggtitle(format(gastroBR$Date[index], "%Y")) +
#     scale_fill_distiller("log rate of admissions\nper habitant",
#       limits = c(-12.7, -7.7),
#       palette = "RdYlBu",
#       labels = ~ round(., 2)
#     ) +
#     theme_void() +
#     theme(legend.position = "bottom"))
# }
# 
# 
# my_maps <- paste0("~/temp/m_", formatC(1:fitted.model$t, width = 3, flag = "0"), ".png")
# for (i in seq.int(1, fitted.model$t, 6)) {
#   get_map(index = i)
#   ggsave(my_maps[i], width = 4, height = 4)
# }

## ----eval=FALSE, include=FALSE------------------------------------------------
# max <- -Inf
# min <- Inf
# for (i in 1:fitted.model$t) {
#   cur <- fitted.model$mts[1, i] + fitted.model$mts[CAR.var.index, i]
#   max <- max(max, cur)
#   min <- min(min, cur)
# }
# max
# min

## ----message=FALSE, warning=FALSE, fig.cap='The $\\log_{10}$ hospital admissions rate by gastroenteritis in Brazilian states at 4 key moments: (a) January of 2010, were our data begins; (b) March of 2020, the month were the first case of COVID-19 was registered in Brazil and before public response; (c) April of 2020, the first month of the pandemic period; and (d) December of 2022, the end of the period of study and roughly 2 years after the beginning of the pandemic. Notice that from (a) to (b) 10 years had passed and we see that a steady and smoothly yearly reductions of hospital admissions led to a significantly reduction of the rate of hospital. In contrast, from (b) to (c), only 1 month had passed, but we see a reduction that, proportionally, is event greater than from (a) to (b). Lastly, from (c) to (d), after roughly 2 years, the rate of hospital admissions seems to be going back to what was seen in (c).'----
smoothed.values <- coef(fitted.model)
plot.data <- data.frame()
labels <- list(
  "2010-01-01" = "(a) January, 2010",
  "2020-03-01" = "(b) March, 2020",
  "2020-04-01" = "(c) April, 2020",
  "2022-12-01" = "(d) December, 2022"
)
for (date in c("2010-01-01", "2020-03-01", "2020-04-01", "2022-12-01")) {
  index <- min(which(gastroBR$Date == date))
  plot.data <- rbind(
    plot.data,
    cbind(
      Date = labels[[date]],
      Effect = smoothed.values$lambda.mean[order(order(levels(reg.data$UF))), index] / log(10),
      br.base
    )
  )
}

(ggplot() +
  geom_sf(data = plot.data, aes(fill = Effect)) +
  facet_wrap(~Date, strip.position = "bottom") +
  scale_fill_distiller("$\\log_{10}$ rate",
    limits = c(-6, -3),
    palette = "RdYlBu",
    labels = ~ round(., 2)
  ) +
  theme_void() +
  theme(legend.position = "bottom"))

## ----message=FALSE, warning=FALSE---------------------------------------------
labels <- list(
  "2015-01-01" = "January, 2015",
  "2019-12-01" = "December, 2019"
)
plot.data <- data.frame()
for (date in c("2015-01-01", "2019-12-01")) {
  index <- min(which(gastroBR$Date == date))

  forecast.vals <- coef(fitted.model, lag = 3, t.eval = index, eval.pred = TRUE)

  mean.pred <- forecast.vals$data$Prediction
  reg.data <- gastroBR %>%
    filter(Date == date) %>%
    mutate(Tx = log10(Admissions / Population))

  plot.data <- rbind(
    plot.data,
    cbind(
      Date = labels[[date]],
      Effect = log10(mean.pred) - log10(reg.data$Population),
      br.base,
      Label = "Prediction"
    )
  )
  plot.data <- rbind(
    plot.data,
    cbind(
      Date = labels[[date]],
      Effect = reg.data$Tx, br.base,
      Label = "Observed"
    )
  )
}
plot.data$Label <- factor(plot.data$Label, levels = unique(plot.data$Label))

(ggplot() +
  geom_sf(data = plot.data, aes(fill = Effect)) +
  facet_wrap(Label ~ Date, strip.position = "bottom") +
  scale_fill_distiller("log10 rate",
    limits = c(-5.5, -3.5),
    palette = "RdYlBu",
    labels = ~ round(., 2)
  ) +
  theme_void() +
  theme(legend.position = "bottom"))

