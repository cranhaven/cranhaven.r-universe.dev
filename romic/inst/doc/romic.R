## ----env_setup, message = FALSE-----------------------------------------------
library(dplyr)
library(romic)

## ----create_tidy_omic---------------------------------------------------------
tidy_brauer <- create_tidy_omic(
  df = brauer_2008,
  feature_pk = "name",
  feature_vars = c("systematic_name", "BP", "MF"),
  sample_pk = "sample",
  sample_vars = c("nutrient", "DR")
)

## ----create_triple_omic-------------------------------------------------------
triple_brauer <- create_triple_omic(
  measurement_df = brauer_2008 %>% select(name, sample, expression),
  feature_df = brauer_2008 %>% select(name:systematic_name) %>% distinct(),
  sample_df = brauer_2008 %>% select(sample:DR) %>% distinct(),
  feature_pk = "name",
  sample_pk = "sample"
)

## ----triple_tidy_conversion---------------------------------------------------
# convert back and forth between tidy and triple representations
triple_brauer <- tidy_to_triple(tidy_brauer)
tidy_brauer <- triple_to_tidy(triple_brauer)

## ----filtering----------------------------------------------------------------
filtered_brauer <- brauer_2008_triple %>%
  filter_tomic(
    filter_type = "category",
    filter_table = "features",
    filter_variable = "BP",
    filter_value = c("protein biosynthesis", "rRNA processing", "response to stress")
  ) %>%
  filter_tomic(
    filter_type = "range",
    filter_table = "samples",
    filter_variable = "DR",
    filter_value = c(0.05, 0.2)
  )

## ----mutate-------------------------------------------------------------------
updated_features <- brauer_2008_triple$features %>%
  dplyr::filter(BP == "biological process unknown") %>%
  dplyr::mutate(chromosome = purrr::map_int(systematic_name, function(x) {
    which(LETTERS == stringr::str_match(x, "Y([A-Z])")[2])
  }))

updated_tomic <- update_tomic(
  brauer_2008_triple,
  updated_features
)

## ----static_heatmap, fig.height = 6, fig.width = 6----------------------------
plot_heatmap(
  filtered_brauer,
  value_var = "expression",
  change_threshold = 5,
  cluster_dim = "rows",
  plot_type = "grob"
)

## ----univariate_plot, warning=FALSE, fig.height = 6, fig.width = 6------------
centered_tidy <- tidy_brauer %>%
  center_tomic()

plot_univariate(
  centered_tidy$data,
  x_var = "expression"
)

