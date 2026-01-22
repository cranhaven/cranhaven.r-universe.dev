library(tidyverse)
#######################################################################################
# Human Development Index
# the parameter used here is referenced in the table below at
# https://hdr.undp.org/sites/default/files/2021-22_HDR/hdr2021-22_technical_notes.pdf
######################################################
# Dimension             Indicator                             Minimum     Maximum
# Health                Life expectancy at birth (years)           20          85
# Education             Expected years of schooling (years)         0          18
# Education             Mean years of schooling (years)             0          15
# Standard of living    GNI per capita (2017 PPP$)                100       75000

# These parameters could be hand-coded, or derived from the data
# i.e. only a few countries has maximum GNI > 75,000 hence the cap.

# Other indices to compute in the notes (for example MPI - they currently use STATA,
# but is willing to adopt more R in the future as stated on the website)


scaling_params <- tibble::tribble(
  ~dimension, ~name, ~var,  ~min, ~max,
  "Health",              "Life expectancy at birth (years)",   "life_exp",    "20",          "85",
  "Education",           "Expected years of schooling (years)",  "exp_sch",    "0",          "18",
  "Education",           "Mean years of schooling (years)",   "avg_sch",      "0",          "15",
  "Standard of living",  "GNI per capita (2017 PPP$)",     "gni_pc",       "100",      "75000"
) |>
  mutate(across(c(min, max), as.numeric),
         across(c(min, max), ~ifelse(var == "gni_pc", log10(.x), .x)))

hdi_params <- scaling_params[,2:5] |>
  bind_rows(tibble_row(name = NA, var = "sch", min = NA, max = NA)) |>
  mutate(weight = c(1/3, 0, 0, 1/3, 1/3),
         weight2 = c(0.1, 0, 0, 0.8, 0.1),
         weight3 = c(0.8, 0, 0, 0.1, 0.1),
         weight4 = c(0.1, 0, 0, 0.1, 0.8))

raw <- readxl::read_xlsx(
  here::here("demo/HDR21-22_Statistical_Annex_HDI_Table.xlsx"), skip = 4)
raw_dt <- raw |>
  janitor::clean_names() |>
  dplyr::select(-paste0("x", seq(4, 14, 2))) |>
  dplyr::rename(id = x1, countries = x2) |>
  dplyr::mutate(across(c(id, human_development_index_hdi:hdi_rank), as.numeric)) |>
  dplyr::filter(!is.na(id)) |>
  dplyr::mutate(across(4:7, ~round(.x, digits = 3)))
colnames(raw_dt) <- c("id", "country", "hdi", "life_exp", "exp_sch", "avg_sch", "gni_pc", "diff", "rank")
#######################################################################################

dt2 <- hdi |>
  mutate(gni_pc = log10(gni_pc)) |>
  init(id = country, indicators = life_exp:gni_pc) |>
  add_meta(new_meta = hdi_params, var_col = var)

dt2 |> rescale(life_exp = rescale_minmax(~life_exp, min = 20, max = 85))

dt |> rescaling(rescale_minmax(~life_exp:gni_pc, min = min, max = max))



#######################################################################################
dt <- raw_dt |>
  mutate(gni_pc = log10(gni_pc)) |>
  mutate(life_exp = rescale_minmax(life_exp, min = 20, max = 85),
         exp_sch = rescale_minmax(exp_sch, min = 0, max = 18),
         avg_sch = rescale_minmax(avg_sch, min = 0, max = 15),
         gni_pc = rescale_minmax(gni_pc, min = 2, max = 4.88)) |>
  init(id = country, indicators = life_exp:gni_pc) |>
  add_meta(new_meta = hdi_params, var_col = var)

dt2 <- dt |>
  dimension_reduction(sch = aggregate_manual(~(exp_sch + avg_sch)/2)) |>
  dimension_reduction(index = aggregate_geometrical(~c(life_exp, sch, gni_pc)))

dt3 <- dt2 |>
  swap_exprs(
    .var = index,
    .exprs = list(
      aggregate_linear(~c(life_exp, sch, gni_pc), weight = weight)
    ),
    .raw_data = dt
  )

########################################################################
dt2 <- dt |>
  dimension_reduction(sch = aggregate_manual(~(exp_sch + avg_sch)/2)) |>
  dimension_reduction(index = aggregate_linear(~c(life_exp, sch, gni_pc), weight = weight))

dt4 <- dt2 |>
  swap_values(.id = 2, .param = weight,
              .value = list(weight2, weight3, weight4),
              .raw_data = dt)




#######################################################################################
# some TODOs
# - [big] maybe implement var_trans as a mutate subclass, since its is basically adding new columns
# - [big] maybe currently dimension reduction in expression should be feature engineering or variable transformation?
#   they don't look like  DR
# - [big] whether it is necessary to have switch_exprs/ switch_values since they can also be achieved by functional programming?
# - [test] different variations of inputting with method/ vars/ new_name should be tested
# - [test] test the class is preserved after each operation
# - [test] new_name should be able to take multiple names, validate the length is correct
# - [quick] variable naming: dim_red -> dimension_reduction
# - go through the ITSL tidymodel book to have a better understanding on why it is necessary to have a pipeline/ rather than just data manipulation
#   this relates to how you can implement dimension reduction
#######################################################################################

# basic
res <- dt |>
  var_trans(gni_pc = log10(gni_pc)) |>
  rescaling(.method = rescale_minmax, .vars = life_exp:gni_pc,
            min = scaling_params$min, max = scaling_params$max) |>
  dim_red(sch = (exp_sch + avg_sch) / 2) |>
  dim_red(index = (life_exp * sch * gni_pc)^(1/3))

dt |> var_trans(new = rescale_minmax(old, ...), )
dt |> var_trans(method = myfun, vars = xxx:bbb, new_name = my_new_name)
dt |> var_trans(new = avg_sch * 2)

# also can refer to computed variables
dt |>
  var_trans(gni_pc = log10(gni_pc)) |>
  rescaling(method = rescale_minmax, vars = gni_pc,
            min = log(100), max = quantile(gni_pc, 0.99))
#######################################################################################
# testing various experessionon the final dimension reduction
res2 <- res |>
  swap_exprs(
    .var = index,
    .exprs = list(
      index1 = (life_exp + sch + gni_pc)/3,
      index2 = 0.4 * life_exp + 0.2 * sch + 0.4 * gni_pc,
      index3 = 0.8 * life_exp + 0.1 * sch + 0.1 * gni_pc,
      index4 = 0.1 * life_exp + 0.8 * sch + 0.1 * gni_pc,
      index5 = 0.1 * life_exp + 0.1 * sch + 0.8 * gni_pc,
      index6 = 0.569 * life_exp + 0.576 * sch + 0.586 * gni_pc),
    .raw_data = dt)

# mutate_weighted_var <- function(w1, w2, w3) {
#   data |>
#     mutate(index = w1 * life_exp + w2 * sch + w3 * gni_pc)
# }


library(ggplot2)
res2$data |>
  ggplot(aes(x = index1, y = index6)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  theme(aspect.ratio = 1)

library(GGally)
# https://stackoverflow.com/questions/42654928/how-to-show-only-the-lower-triangle-in-ggpairs
gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1

  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1

  g
}

p1 <- ggpairs(res2$data, columns = 11:17, upper = NULL, diag = NULL) +
  #geom_abline(slope = 1, intercept = 0, color = "blue") +
  theme(aspect.ratio = 1)

gpairs_lower(p1)
#######################################################################################
# what about we change the upper limit of the gni_pc for rescaling
res2 <- res |>
  swap_values(
    .module = var_trans, .step = rescale_minmax, .res = avg_sch,
    .var = max, .values = 16,
    .raw_data = dt)

new_rank <- res2$data |>
  mutate(rank0 = rank(-index0), rank1 = rank(-index1), rank2 = rank(-index2)) |>
  arrange(rank0)

# need more analysis here to see how using different min value to scale changes the ranking


library("GGally")
g <- ggpairs(new_rank, columns =paste0("rank", 0:2), upper = NULL, diag = NULL) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  scale_y_continuous(breaks = seq(0, 200, 10)) +
  theme(aspect.ratio = 1)
gpairs_lower(g)
#######################################################################################
res2 <- dt |>
  dplyr::mutate(
    s_exp_sch = exp_sch/18, # rescaling
    s_avg_sch = avg_sch/15, # rescaling
    sch = (s_exp_sch + s_avg_sch) / 2, # dimension reduction
    life = (life_exp - 20)/(85-20), # rescaling
    gni = (log10(gni_pc) - log10(100))/(log10(75000) - log10(100)), # rescaling
    calc = (life * sch * gni)^(1/3) # dimension reduction
  )





raw <- res$data |> dplyr::select(life_exp, sch, gni_pc) |> as.matrix()
a <- prcomp(raw, center = TRUE, scale. = TRUE)


# MPI
mpi_raw <-  readxl::read_xlsx(file.choose(), skip = 2)

mpi_raw |>
  janitor::clean_names()

