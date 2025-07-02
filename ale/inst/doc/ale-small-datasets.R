## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----load libraries-----------------------------------------------------------
library(ale)

## ----attitude_str-------------------------------------------------------------
str(attitude)

## ----attitude_summary---------------------------------------------------------
summary(attitude)

## ----lm_summary---------------------------------------------------------------
lm_attitude <- lm(rating ~ ., data = attitude)

summary(lm_attitude)

## ----lm_simple, fig.width=7, fig.height=6-------------------------------------
ale_lm_attitude_simple <- ALE(lm_attitude)

# Print all plots
plot(ale_lm_attitude_simple) |> 
  print(ncol = 2)

## ----lm_full_call-------------------------------------------------------------
mb_lm <- ModelBoot(
  lm_attitude,
  boot_it = 10  # 100 by default but reduced here for a faster demonstration
)

## ----lm_full_stats------------------------------------------------------------
mb_lm@model_stats

## ----lm_full_coefs------------------------------------------------------------
mb_lm@model_coefs

## ----lm_full_ale, fig.width=7, fig.height=6-----------------------------------
plot(mb_lm) |> 
  print(ncol = 2)

## ----gam_summary--------------------------------------------------------------
gam_attitude <- mgcv::gam(
  rating ~ complaints + privileges + s(learning) +
    raises + s(critical) + advance,
  data = attitude)
summary(gam_attitude)

## ----gam_simple, fig.width=7, fig.height=6------------------------------------
ale_gam_attitude_simple <- ALE(gam_attitude)

plot(ale_gam_attitude_simple) |> 
  print(ncol = 2)

## ----gam_full_stats-----------------------------------------------------------
mb_gam <- ModelBoot(
  gam_attitude, 
  boot_it = 10,  # 100 by default but reduced here for a faster demonstration
  pred_type = 'response'
  )
mb_gam@model_stats

## ----gam_full_coefs-----------------------------------------------------------
mb_gam@model_coefs

## ----gam_full_ale, fig.width=7, fig.height=6----------------------------------
plot(mb_gam) |> 
  print(ncol = 2)

## ----gam_summary_repeat-------------------------------------------------------
gam_attitude_again <- mgcv::gam(
  rating ~ complaints + privileges + s(learning) +
    raises + s(critical) + advance,
  data = attitude)
summary(gam_attitude_again)

## ----model_call_string--------------------------------------------------------
mb_gam_non_standard <- ModelBoot(
  gam_attitude_again,
  model_call_string = 'mgcv::gam(
    rating ~ complaints + privileges + s(learning) +
      raises + s(critical) + advance,
    data = boot_data)', 
  boot_it = 10  # 100 by default but reduced here for a faster demonstration
)
mb_gam_non_standard@model_stats

