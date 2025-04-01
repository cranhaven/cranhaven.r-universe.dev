## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(tabnet)
#  library(dplyr)
#  library(data.tree)
#  library(ggplot2)
#  library(rsample)
#  library(tibble)
#  set.seed(202307)

## -----------------------------------------------------------------------------
#  data(acme, package = "data.tree")
#  acme$attributesAll
#  print(acme, "cost", "p" , limit = 8)

## -----------------------------------------------------------------------------
#  data(starwars, package = "dplyr")
#  head(starwars, 4)
#  
#  # erroneous Node construction
#  starwars_tree <- starwars %>%
#    mutate(pathString = paste("StarWars_characters", species, sex, `name`, sep = "/")) %>%
#    as.Node()
#  print(starwars_tree, "name","height", "mass", "eye_color", limit = 8)
#  

## -----------------------------------------------------------------------------
#  # demonstration of reserved column modification in Node construction
#  starwars_tree <- starwars %>%
#    rename(`_name` = "name", `_height` = "height") %>%
#    mutate(pathString = paste("StarWars_characters", species, sex, `_name`, sep = "/")) %>%
#    as.Node()
#  print(starwars_tree, "name", "_name","_height", "mass", "eye_color", limit = 8)

## -----------------------------------------------------------------------------
#  starw_split <- starwars %>%
#    tidyr::unnest_longer(films) %>%
#    tidyr::unnest_longer(vehicles, keep_empty = TRUE) %>%
#    tidyr::unnest_longer(starships, keep_empty = TRUE) %>%
#    initial_split( prop = .8, strata = "species")

## -----------------------------------------------------------------------------
#  # correct Node construction for hierarchical modeling
#  starwars_train_tree <- starw_split %>%
#    training() %>%
#    # avoid reserved column names
#    rename(`_name` = "name", `_height` = "height") %>%
#    rowid_to_column() %>%
#    mutate(pathString = paste("StarWars_characters", species, sex, rowid, sep = "/")) %>%
#    # remove outcomes labels from predictors
#    select(-species, -sex, -`_name`, -rowid) %>%
#    # turn it as hierarchical Node
#    as.Node()
#  
#  starwars_test_tree <- starw_split %>%
#    testing() %>%
#    rename(`_name` = "name", `_height` = "height") %>%
#    rowid_to_column() %>%
#    mutate(pathString = paste("StarWars_characters", species, sex, rowid, sep = "/")) %>%
#    select(-species, -sex, -`_name`, -rowid) %>%
#    as.Node()
#  
#  starwars_train_tree$attributesAll

## ----model training-----------------------------------------------------------
#  config <- tabnet_config(decision_width = 8, attention_width = 8, num_steps = 3, penalty = .003, cat_emb_dim = 2, valid_split = 0.2, learn_rate = 1e-3, lr_scheduler = "reduce_on_plateau", early_stopping_monitor = "valid_loss", early_stopping_patience = 4, verbose = FALSE)
#  
#  starw_model <- tabnet_fit(starwars_train_tree, config = config, epoch = 170, checkpoint_epochs = 25)
#  

## -----------------------------------------------------------------------------
#  autoplot(starw_model)

## -----------------------------------------------------------------------------
#  vip::vip(starw_model)

## -----------------------------------------------------------------------------
#  starwars_hat <- bind_cols(
#      predict(starw_model, starwars_test_tree),
#      node_to_df(starwars_test_tree)$y
#    )
#  tail(starwars_hat, n = 5)
#  

## -----------------------------------------------------------------------------
#  starwars_explain <- tabnet_explain(starw_model, starwars_test_tree)
#  autoplot(starwars_explain)
#  autoplot(starwars_explain, type = "steps")

