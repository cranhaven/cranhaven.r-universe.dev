## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.align = "center"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(waves)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

## ----install_cran, eval=FALSE-------------------------------------------------
#  install.packages("waves")
#  library(waves)

## ----install_dev, eval=FALSE--------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("GoreLab/waves")
#  library(waves)

## ----format-------------------------------------------------------------------
ikeogu.2017[1:7, 1:7]

ikeogu.2017.prepped <- ikeogu.2017 %>%
  dplyr::rename(unique.id = sample.id,
                reference = DMC.oven) %>%
  dplyr::select(unique.id, dplyr::everything(), -TCC) %>%
  na.omit()

ikeogu.2017.prepped[1:7, 1:7]

## ----plot_raw, fig.height=5, fig.width=7--------------------------------------
ikeogu.2017.prepped %>%
  plot_spectra(
    df = .,
    num.col.before.spectra = 5,
    detect.outliers = FALSE,
    alternate.title = "Example spectra"
  )

## ----filter-------------------------------------------------------------------
filtered.df <- ikeogu.2017.prepped %>%
  filter_spectra(
    df = .,
    filter = TRUE,
    return.distances = TRUE,
    num.col.before.spectra = 5,
    window.size = 15
    )

filtered.df[1:5, c(1:5, (ncol(filtered.df) - 3):ncol(filtered.df))]

## ----aggregate----------------------------------------------------------------
aggregated.test <- ikeogu.2017.prepped %>%
  aggregate_spectra(
    grouping.colnames = c("study.name"),
    reference.value.colname = "reference",
    agg.function = "mean"
    )
aggregated.test[, 1:5]

## ----run_test_spectra---------------------------------------------------------
results.list <- ikeogu.2017.prepped %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  test_spectra(
    train.data = .,
    tune.length = 3,
    num.iterations = 3,
    pretreatment = 1
    )

## ----plot_pretreatments, fig.height=6, fig.width=7----------------------------
ikeogu.2017.prepped[1:10, ] %>% # subset the first 10 scans for speed
  pretreat_spectra(pretreatment = 2:13) %>% # exclude pretreatment 1 (raw data)
  bind_rows(.id = "pretreatment") %>%
  gather(key = "wl",
         value = "s.value",
         tidyselect::starts_with("X")) %>%
  mutate(wl = as.numeric(readr::parse_number(.data$wl)),
         pretreatment = as.factor(pretreatment)) %>%
  drop_na(s.value) %>%
  ggplot(data = ., aes(x = wl, y = s.value, group = unique.id)) +
  geom_line(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Pretreated spectra",
       x = "Wavelength",
       y = "Spectral Value") +
  facet_wrap(~ pretreatment, scales = "free")

## ----view_model---------------------------------------------------------------
summary(results.list$model)

## ----view_summary-------------------------------------------------------------
results.list$summary.model.performance

## ----view_performance---------------------------------------------------------
results.list$model.performance

## ----view_predictions---------------------------------------------------------
head(results.list$predictions)

## ----view_importance----------------------------------------------------------
results.list$importance[, 1:7]

## ----run_save_model-----------------------------------------------------------
model.to.save <- ikeogu.2017.prepped %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  save_model(
    df = .,
    write.model = FALSE,
    pretreatment = 1:13,
    tune.length = 5,
    num.iterations = 3,
    verbose = FALSE
    )

## ----summarize_saved_model----------------------------------------------------
summary(model.to.save$best.model)

## ----format_saved_output------------------------------------------------------
model.to.save$best.model.stats %>%
  gather(key = "statistic", value = "value", RMSEp_mean:best.mtry_mode) %>%
  separate(statistic, into =  c("statistic", "summary_type"), sep = "_") %>%
  pivot_wider(id_cols = c(Pretreatment, summary_type),
              names_from = statistic, values_from = value)

## ----prep_for_predictions-----------------------------------------------------
pretreated.val <- ikeogu.2017.prepped %>%
  filter(study.name == "C16Mval") %>%
  pretreat_spectra(pretreatment = 8)

pretreated.val.mx <- pretreated.val %>%
  dplyr::select(starts_with("X")) %>%
  as.matrix()

best.ncomp <- model.to.save$best.model.stats$best.ncomp_mode

## ----predict------------------------------------------------------------------
predicted.values <- as.numeric(predict(model.to.save$best.model,
                                       newdata = pretreated.val.mx,
                                       ncomp = best.ncomp))

## ----calculate_statistics-----------------------------------------------------
spectacles::postResampleSpectro(pred = predicted.values,
                                obs = pretreated.val$reference)

## ----plot_predictions, fig.height=6-------------------------------------------
overall.range <- c(min(c(pretreated.val$reference, predicted.values)),
                   max(c(pretreated.val$reference, predicted.values)))
cbind(unique.id = pretreated.val$unique.id,
      observed = pretreated.val$reference,
      predicted = predicted.values) %>%
  as_tibble() %>%
  mutate(observed = as.numeric(observed),
         predicted = as.numeric(predicted)) %>%
  ggplot(aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "gray80") +
  geom_point() +
  coord_fixed(xlim = overall.range,
              ylim = overall.range) +
  labs(title = "Example dry matter content predictions",
       x = "Observed",
       y = "Predicted") +
  theme_bw()

