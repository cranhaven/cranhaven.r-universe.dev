## ----include = FALSE----------------------------------------------------------
use_saved_results <- TRUE

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  eval = !use_saved_results,
  message = FALSE,
  warning = FALSE
)

if (use_saved_results) {
  results <- readRDS("vignette_pbhp.rds")
  eval_frame <- results$eval_frame
}

## ----eval=TRUE----------------------------------------------------------------
library(dplyr); library(tidyr); library(purrr) # Data wrangling
library(ggplot2); library(stringr) # Plotting
library(tidyfit)   # Auto-ML modeling

## ----eval=TRUE----------------------------------------------------------------
data <- MASS::Boston

# For reproducibility
set.seed(123)
ix_tst <- sample(1:nrow(data), round(nrow(data)*0.5))

data_trn <- data[-ix_tst,]
data_tst <- data[ix_tst,]

as_tibble(data)

## ----eval=TRUE----------------------------------------------------------------
model_frame <- data_trn |>
  regress(medv ~ ., OLS = m("lm"))

## ----eval=TRUE----------------------------------------------------------------
assess <- function(model_frame, data_trn, data_tst) {
  
  oos <- model_frame |>
    predict(data_tst) |>
    group_by(model, .add = TRUE) |>
    yardstick::rsq_trad(truth, prediction) |>
    mutate(type = "Out-of-sample") |>
    arrange(desc(.estimate))
  
  is <- model_frame |>
    predict(data_trn) |>
    group_by(model, .add = TRUE) |>
    yardstick::rsq_trad(truth, prediction) |>
    mutate(type = "In-sample")
  
  return(bind_rows(oos, is))
  
}

plotter <- function(df) {
  df <- df |>
    mutate(lab = round(.estimate, 2)) |>
    mutate(model = str_wrap(model, 12))
  df |> 
    mutate(model = factor(model, levels = unique(df$model))) |>
    ggplot(aes(model, .estimate)) +
    geom_point(aes(color = type), size = 2.5, shape = 4) + 
    geom_label(aes(label = lab, color = type), size = 2, nudge_x = 0.35) +
    theme_bw() +
    scale_color_manual(values = c("firebrick", "darkblue")) +
    theme(legend.title = element_blank(), 
          axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0.65, 0.95))
}

## ----fig.width=3, fig.height=3, fig.align="center", eval=TRUE-----------------
model_frame |>
  assess(data_trn, data_tst) |>
  plotter()

## ----fig.width=3, fig.height=3, fig.align="center", eval=TRUE-----------------
model_frame <- data_trn |>
  regress(medv ~ .*., OLS = m("lm"))

model_frame |>
  assess(data_trn, data_tst) |>
  plotter()

## ----echo = F, eval=TRUE------------------------------------------------------
library(kableExtra)
tbl <- data.frame(
  `Best subset` = c("Exhaustive search (`leaps`)", "Forward selection (`leaps`)", "Backward selection (`leaps`)", "Sequential Replacement (`leaps`)", "Genetic Algorithm ('gaselect')"),
  `L1/L2 penalized` = c("Lasso (`glmnet`)", "Ridge (`glmnet`)", "ElasticNet (`glmnet`)", "Adaptive Lasso (`glmnet`)", ""),
  `Latent factors` = c("Principal components regression (`pls`)", "Partial least squares (`pls`)", "", "", ""),
  `Machine Learning` = c("Gradient Boosting (`mboost`)", "Hierarchical feature regression (`hfr`)", "Support vector regression (`e1071`)", "", ""),
  `Bayesian` = c("Bayesian regression (`arm`)", "Bayesian model averaging (`BMS`)", "Bayesian Lasso (`monomvn`)", "Bayesian Ridge (`monomvn`)", "Spike and Slab Regression ('BoomSpikeSlab')"),
  check.names = F
)
kbl(tbl, align = "ccccc") |>
  kable_styling("striped")


## -----------------------------------------------------------------------------
# model_frame_all <- data_trn |>
#   regress(medv ~ .*.,
#           OLS = m("lm"),
#           BAYES = m("bayes"),
#           BMA = m("bma", iter = 10000),
#           SEQREP = m("subset", method = "seqrep", IC = "AIC"),
#           `FORWARD SELECTION` = m("subset", method = "forward", IC = "AIC"),
#           `BACKWARD SELECTION` = m("subset", method = "backward", IC = "AIC"),
#           LASSO = m("lasso"),
#           BLASSO = m("blasso"),
#           SPIKESLAB = m("spikeslab", niter = 10000),
#           RIDGE = m("ridge"),
#           BRIDGE = m("bridge"),
#           ELASTICNET = m("enet"),
#           ADALASSO = m("adalasso", lambda_ridge = c(0.001, 0.01, 0.1)),
#           PCR = m("pcr"),
#           PLSR = m("plsr"),
#           HFR = m("hfr"),
#           `GRADIENT BOOSTING` = m("boost"),
#           SVR = m("svm"),
#           GENETIC = m("genetic", populationSize = 1000, numGenerations = 50, statistic = "AIC", maxVariables = 20),
#           .cv = "vfold_cv", .cv_args = list(v = 10))

## -----------------------------------------------------------------------------
# eval_frame <- model_frame_all |>
#   assess(data_trn, data_tst)

## ----fig.width=7.25, fig.height=4.25, fig.align="center", eval=TRUE-----------
eval_frame |>
  plotter() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 90))

