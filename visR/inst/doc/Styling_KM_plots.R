## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7,
  fig.height = 4,
  fig.align = "center",
  comment = "#>"
)

## ----libraries, include = TRUE------------------------------------------------
library(visR)

## ----generate-survival-data---------------------------------------------------
lung_cohort <- survival::lung

lung_cohort <- lung_cohort %>%  
  dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female")))  %>%  
  dplyr::mutate(status = status - 1) %>%
  dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") 

lung_suvival_object <- lung_cohort %>%
  visR::estimate_KM(strata = "Sex", CNSR = "Status", AVAL = "Days")

## ----default-ggplot2-plot-----------------------------------------------------
p <- lung_suvival_object %>%
  visR::visr()
p

## ----styling-with-ggplot2-----------------------------------------------------
p +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::scale_color_manual(values = c("red", "blue"))

## ----visr-parameter-styling---------------------------------------------------
lung_suvival_object %>%
  visR::visr(x_label = "Time",
             y_label = NULL, # NULL (default) causes the label to be deducted from the used function
             x_ticks = seq(0, 1200, 200),
             y_ticks = seq(0, 100, 20),
             fun = "pct",
             legend_position = "top") 

## ----visr-define_theme-empty--------------------------------------------------
visR::define_theme() 

## ----visr-define_theme-nonempty-----------------------------------------------
theme <-
  visR::define_theme(
    strata = list(
      "Sex" = list("Female" = "red",
                   "Male" = "blue"),
      "ph.ecog" = list("0" = "cyan",
                       "1" = "purple",
                       "2" = "brown")
    ),
    fontsizes = list(
      "axis" = 12,
      "ticks" = 10,
      "legend_title" = 10,
      "legend_text" = 8
    ),
    fontfamily = "Helvetica",
    grid = list("major" = FALSE,
                "minor" = FALSE),
    #grid = TRUE/FALSE # <- can also be used instead of the named list above
    bg = "transparent",
    legend_position = "top"
  )

## ----visr-apply_theme-empty, warning=FALSE------------------------------------
lung_suvival_object %>%
  visR::visr() %>%
  visR::apply_theme()

## ----visr-apply_theme-nonempty, warning = FALSE, message=FALSE----------------
lung_suvival_object %>%
  visR::visr() %>%
  visR::apply_theme(theme)

