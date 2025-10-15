## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(flexFitR)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(purrr)
data(dt_potato)
head(dt_potato) |> kable()

## -----------------------------------------------------------------------------
fun_logistic <- function(t, L, k, t0) L / (1 + exp(-k * (t - t0)))

## -----------------------------------------------------------------------------
plots <- c(40, 166)

## -----------------------------------------------------------------------------
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fun_logistic",
    parameters = c(L = 100, k = 4, t0 = 40),
    subset = plots
  )

## -----------------------------------------------------------------------------
print(mod_1)

## ----fig.width= 8, fig.height=4, fig.alt="plot fit"---------------------------
plot(mod_1, id = plots)

## ----fig.alt="plot fit 2", fig.width= 8---------------------------------------
points <- predict(mod_1, x = 55, type = "point", se_interval = "confidence")
points |> kable()

## ----fig.alt="plot fit 2", fig.width= 8---------------------------------------
mod_1 |>
  plot(id = plots, type = 3) +
  color_palette(palette = "jco") +
  geom_point(data = points, aes(x = x_new, y = predicted.value), shape = 8)

## -----------------------------------------------------------------------------
points <- predict(mod_1, x = 55, type = "point", se_interval = "prediction")
points |> kable()

## -----------------------------------------------------------------------------
predict(mod_1, x = c(0, 108), type = "auc", n_points = 500) |> kable()

## -----------------------------------------------------------------------------
predict(mod_1, formula = ~ k / L * 100) |> kable()

## -----------------------------------------------------------------------------
predict(mod_1, formula = ~ (k * L) / 4) |> kable()

## ----fig.width= 8, fig.height=4, fig.alt="plot 1 deriv"-----------------------
plot(mod_1, id = plots, type = 5, color = "blue", add_ci = FALSE)

## -----------------------------------------------------------------------------
interval <- seq(0, 100, by = 0.1)
points_fd <- mod_1 |>
  predict(x = interval, type = "fd") |>
  group_by(uid) |>
  summarise(
    max_fd = max(predicted.value),
    argmax_fd = x_new[which.max(predicted.value)]
  )
points_fd |> kable()

## ----fig.alt="plot deriv" , fig.width= 8--------------------------------------
mod_1 |>
  plot(id = plots, type = 3) +
  color_palette(palette = "jco") +
  geom_vline(data = points_fd, aes(xintercept = argmax_fd), linetype = 2) +
  geom_label(data = points_fd, aes(x = argmax_fd, y = 0, label = argmax_fd)) +
  facet_wrap(~uid) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
points_fd$y_hat <- sapply(
  X = plots,
  FUN = \(i) {
    mod_1 |>
      predict(x = points_fd[points_fd$uid == i, "argmax_fd"], id = i) |>
      pull(predicted.value)
  }
)
points_fd |> kable()

## ----fig.alt="plot points" , fig.width= 8-------------------------------------
mod_1 |>
  plot(id = plots, type = 3) +
  color_palette(palette = "jco") +
  geom_point(data = points_fd, aes(x = argmax_fd, y = y_hat), shape = 8)

## ----fig.width= 8, fig.height=4, fig.alt="plot 2 deriv"-----------------------
plot(mod_1, id = plots, type = 6, color = "blue", add_ci = FALSE)

## -----------------------------------------------------------------------------
points_sd <- mod_1 |>
  predict(x = interval, type = "sd") |>
  group_by(uid) |>
  summarise(
    max_sd = max(predicted.value),
    argmax_sd = x_new[which.max(predicted.value)],
    min_sd = min(predicted.value),
    argmin_sd = x_new[which.min(predicted.value)]
  )
points_sd |> kable()

## ----fig.alt="plot deriv 2" , fig.width= 8------------------------------------
mod_1 |>
  plot(id = plots, type = 3) +
  color_palette(palette = "jco") +
  geom_vline(data = points_sd, aes(xintercept = argmax_sd), linetype = 2) +
  geom_vline(data = points_sd, aes(xintercept = argmin_sd), linetype = 2) +
  facet_wrap(~uid) +
  theme(legend.position = "none")

