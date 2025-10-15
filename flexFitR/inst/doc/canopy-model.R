## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(flexFitR)
library(dplyr)
library(kableExtra)
library(ggpubr)
library(purrr)

## -----------------------------------------------------------------------------
data(dt_potato)
explorer <- explorer(dt_potato, x = DAP, y = Canopy, id = Plot)

## -----------------------------------------------------------------------------
names(explorer)

## ----fig.width= 8, fig.height=4, fig.alt="plot corr"--------------------------
p1 <- plot(explorer, type = "evolution", return_gg = TRUE, add_avg = TRUE)
p2 <- plot(explorer, type = "x_by_var", return_gg = TRUE)
ggarrange(p1, p2)

## ----echo=FALSE---------------------------------------------------------------
kable(mutate_if(explorer$summ_vars, is.numeric, round, 2))

## ----echo = FALSE, fig.width= 8, fig.alt="plot fn"----------------------------
plot_fn(
  fn = "fn_lin_plat",
  params = c(t1 = 40, t2 = 61.8, k = 100),
  interval = c(0, 108),
  color = "black",
  base_size = 15
)

## ----warning=FALSE, message=FALSE---------------------------------------------
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_lin_plat",
    parameters = c(t1 = 45, t2 = 80, k = 0.9),
    subset = c(166, 40)
  )
mod_1

## ----fig.width= 8, fig.height=4, fig.alt="plot fit"---------------------------
plot(mod_1, id = c(166, 40))
kable(mod_1$param)

## -----------------------------------------------------------------------------
coef(mod_1)

## -----------------------------------------------------------------------------
confint(mod_1)

## -----------------------------------------------------------------------------
vcov(mod_1)

## -----------------------------------------------------------------------------
initials <- data.frame(
  uid = c(166, 40),
  t1 = c(70, 60),
  t2 = c(40, 80),
  k = c(100, 100)
)

## -----------------------------------------------------------------------------
kable(initials)

## -----------------------------------------------------------------------------
mod_2 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_lin_plat",
    parameters = initials,
    subset = c(166, 40)
  )

## ----fig.width= 8, fig.height=4, fig.alt="plot fit 2"-------------------------
plot(mod_2, id = c(166, 40))
kable(mod_2$param)

## -----------------------------------------------------------------------------
fixed_params <- list(k = "max(y)")

## -----------------------------------------------------------------------------
mod_3 <- dt_potato |>
  modeler(
    x = DAP,
    y = Canopy,
    grp = Plot,
    fn = "fn_lin_plat",
    parameters = c(t1 = 45, t2 = 80, k = 0.9),
    fixed_params = fixed_params,
    subset = c(166, 40)
  )

## ----fig.width= 8, fig.height=4, fig.alt="plot fit 3"-------------------------
plot(mod_3, id = c(166, 40))
kable(mod_3$param)

## -----------------------------------------------------------------------------
rbind.data.frame(
  mutate(mod_1$param, model = "1", .before = uid),
  mutate(mod_2$param, model = "2", .before = uid),
  mutate(mod_3$param, model = "3", .before = uid)
) |>
  filter(uid %in% 166) |>
  kable()

## -----------------------------------------------------------------------------
comparison <- performance(mod_1, mod_2, mod_3)
comparison |>
  filter(uid %in% 166) |>
  kable()

## ----fig.alt="plot fit 4"-----------------------------------------------------
plot(comparison, id = 166)

## -----------------------------------------------------------------------------
# Point Prediction
predict(mod_1, x = 45, type = "point", id = 166) |> kable()
# AUC Prediction
predict(mod_1, x = c(0, 108), type = "auc", id = 166) |> kable()
# Function of the parameters
predict(mod_1, formula = ~ t2 - t1, id = 166) |> kable()

## ----eval= FALSE--------------------------------------------------------------
# mod <- dt_potato |>
#   modeler(
#     x = DAP,
#     y = Canopy,
#     grp = Plot,
#     fn = "fn_lin_plat",
#     parameters = c(t1 = 45, t2 = 80, k = 0.9),
#     fixed_params = list(k = "max(y)"),
#     options = list(progress = TRUE, parallel = TRUE, workers = 5)
#   )

