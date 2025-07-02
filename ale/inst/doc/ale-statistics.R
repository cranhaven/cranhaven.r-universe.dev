## ----knitr, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load libraries-----------------------------------------------------------
library(mgcv)   # for datasets and the gam function
library(dplyr)  # for data manipulation
library(ale)

## ----data setup---------------------------------------------------------------
# Create and prepare the data

# Specific seed chosen to illustrate the spuriousness of the random variable
set.seed(6)  

math <- 
  # Start with math achievement scores per student
  MathAchieve |> 
  as_tibble() |> 
  mutate(
    school = School |> as.character() |>  as.integer(),
    minority = Minority == 'Yes',
    female = Sex == 'Female'
  ) |> 
  # summarize the scores to give per-school values
  summarize(
    .by = school,
    minority_ratio = mean(minority),
    female_ratio = mean(female),
    math_avg = mean(MathAch),
  ) |> 
  # merge the summarized student data with the school data
  inner_join(
    MathAchSchool |> 
      mutate(school = School |> as.character() |>  as.integer()),
    by = c('school' = 'school')
  ) |> 
  mutate(
    public = Sector == 'Public',
    high_minority = HIMINTY == 1,
  ) |> 
  select(-School, -Sector, -HIMINTY) |> 
  rename(
    size = Size,
    academic_ratio = PRACAD,
    discrim = DISCLIM,
    mean_ses = MEANSES,
  ) |> 
  # Remove ID column for analysis
  select(-school) |> 
  select(
    math_avg, size, public, academic_ratio,
    female_ratio, mean_ses, minority_ratio, high_minority, discrim,
    everything()
  ) |> 
  mutate(
    rand_norm = rnorm(nrow(MathAchSchool)) 
  )

glimpse(math)

## ----y summary----------------------------------------------------------------
summary(math$math_avg)

## ----train gam model----------------------------------------------------------
gam_math <- gam(
     math_avg ~ public + high_minority +
     s(size) + s(academic_ratio) + s(female_ratio) + s(mean_ses) + 
     s(minority_ratio) + s(discrim) + s(rand_norm),
     data = math
   )

gam_math

## ----ALEpDist-----------------------------------------------------------------
# # To generate the code, uncomment the following lines.
# # But it is slow because it retrains the model 1000 times, so this vignette loads a pre-created ALEpDist object.
# # For standard models like mgcv::gam that store their data,
# # there is no need to specify the data argument.
# gam_math_p_dist <- ALEpDist(gam_math)
# saveRDS(gam_math_p_dist, file.choose())
gam_math_p_dist <- url('https://github.com/tripartio/ale/raw/main/download/gam_math_p_dist.0.5.0.rds') |> 
  readRDS()

## ----model bootstrap----------------------------------------------------------
# # To generate the code, uncomment the following lines.
# # But bootstrapping is slow because it retrains the model, so this vignette loads a pre-created ale_boot object.
# mb_gam_math <- ModelBoot(
#   gam_math,
#   # Pass the ALEpDist object so that p-values will be generated
#   ale_p = gam_math_p_dist,
#   # For the GAM model coefficients, show details of all variables, parametric or not
#   tidy_options = list(parametric = TRUE),
#   # tidy_options = list(parametric = NULL),
#   boot_it = 100  # default
# )
# saveRDS(mb_gam_math, file.choose())
mb_gam_math <- url('https://github.com/tripartio/ale/raw/main/download/mb_gam_math_stats_vignette.0.5.0.rds') |> 
  readRDS()

## ----model_stats--------------------------------------------------------------
mb_gam_math@model_stats

## ----model_coefs--------------------------------------------------------------
mb_gam_math@model_coefs

## ----model_coefs stat sig variables-------------------------------------------
mb_gam_math@model_coefs |> 
  # filter is TRUE if conf.low and conf.high are both positive or both negative because
  # multiplying two numbers of the same sign results in a positive number.
  filter((conf.low * conf.high) > 0)

## ----all-ALE-plots, fig.width=7, fig.height=10--------------------------------
mb_gam_plots <- plot(mb_gam_math)
print(mb_gam_plots, ncol = 2)

## ----model-bootstrap-without-p, fig.width=7, fig.height=10--------------------
# # To generate the code, uncomment the following lines.
# # But bootstrapping is slow because it retrains the model, so this vignette loads a pre-created ale_boot object.
# mb_gam_no_p <- ModelBoot(
#   gam_math,
#   ale_p = NULL,  # disable ALE p-values
#   # For the GAM model coefficients, show details of all variables, parametric or not
#   tidy_options = list(parametric = TRUE),
#   # tidy_options = list(parametric = NULL),
#   boot_it = 40  # 100 by default but reduced here for a faster demonstration
# )
# saveRDS(mb_gam_no_p, file.choose())
mb_gam_no_p <- url('https://github.com/tripartio/ale/raw/main/download/mb_gam_no_p_stats_vignette.0.5.0.rds') |> 
  readRDS()

plot(mb_gam_no_p) |> 
  print(ncol = 2)

## ----ALE-effects-plot, fig.width=8, fig.height=6------------------------------
get(mb_gam_plots, type = 'effect')

## ----ALE-plot-for-public-1----------------------------------------------------
get(mb_gam_plots, 'public')

## ----ALE stats for public-----------------------------------------------------
get(mb_gam_math, 'public', stats = 'all')

## ----ALE-plot-forl-academic_ratio-1-------------------------------------------
get(mb_gam_plots, 'academic_ratio')

## ----ALE stats for academic_ratio---------------------------------------------
get(mb_gam_math, 'academic_ratio', stats = 'all')

## ----math-rand_norm-ALE-plot, fig.width=3.5, fig.height=3---------------------
get(mb_gam_plots, 'rand_norm')

## ----ALE stats for rand_norm--------------------------------------------------
get(mb_gam_math, 'rand_norm', stats = 'all')

## ----ale data for public------------------------------------------------------
get(mb_gam_math, 'public')

## ----ale data for academic_ratio----------------------------------------------
get(mb_gam_math, 'academic_ratio')

## ----ALE-plot-for-mean_ses----------------------------------------------------
get(mb_gam_plots, 'mean_ses')

## ----conf_regions for mean_ses------------------------------------------------
get(mb_gam_math, 'mean_ses', stats = 'conf_regions')

## ----text conf_regions for mean_ses-------------------------------------------
get(mb_gam_math, 'mean_ses', stats = 'conf_regions') |> 
  ale:::summarize_conf_regions_1D_in_words()

## ----ALE-plot-for-public-2----------------------------------------------------
get(mb_gam_plots, 'public')

## ----conf_regions for public--------------------------------------------------
get(mb_gam_math, 'public', stats = 'conf_regions')

## ----ALE-plot-for-rand_norm---------------------------------------------------
get(mb_gam_plots, 'rand_norm')

## ----conf_regions for rand_norm-----------------------------------------------
get(mb_gam_math, 'rand_norm', stats = 'conf_regions')

## ----significant conf_regions-------------------------------------------------
get(mb_gam_math, stats = 'conf_sig')

## ----variables with significant conf_regions----------------------------------
get(mb_gam_math, stats = 'conf_sig')$term |> 
  unique()

