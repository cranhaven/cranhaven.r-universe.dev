## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results = 'hide', message=FALSE-----------------------------------
library(rsetse)
library(igraph)
library(dplyr)
library(ggraph)

## ----message=FALSE------------------------------------------------------------
biconnected_network %>%
  ggraph() + 
  geom_edge_link() +
  geom_node_point(aes(colour = group), size = 3) 

## ----message=FALSE------------------------------------------------------------

embeddings_cont <- biconnected_network %>%
  prepare_edges(.) %>%
  prepare_continuous_force(., node_names = "name", force_var = "force") %>%
  setse_auto(., k = "weight")


out <- create_node_edge_df(embeddings_cont, function_names = c("mean", "mode", "sum"))


## ----message=FALSE------------------------------------------------------------

embeddings_cont_fixed <- biconnected_network %>%
  prepare_edges(., k = 500) %>%
  prepare_continuous_force(., node_names = "name", force_var = "force") %>%
  setse_auto(., k = "k")


## ----message=FALSE------------------------------------------------------------

continuous_results <- bind_rows(create_node_edge_df(embeddings_cont) %>% mutate(type = "variable k"),
          create_node_edge_df(embeddings_cont_fixed) %>% mutate(type = "fixed k")
 ) 

continuous_results %>% 
  ggplot(aes(x = tension_mean, y = elevation, colour = node)) + geom_jitter() +
  facet_wrap(~type)  +
  facet_wrap(~type) +
  labs(title = "Continuous embeddings",
       x = "mean tension")


## ----message=FALSE------------------------------------------------------------

embeddings_binary <- biconnected_network %>%
  prepare_edges(.) %>%
  prepare_categorical_force(., node_names = "name", force_var = "group") %>%
  setse_auto(., 
             force = "group_A",
             k = "weight")

embeddings_binary_fixed <- biconnected_network %>%
  prepare_edges(., k = 500) %>%
  prepare_categorical_force(., node_names = "name", force_var = "group") %>%
  setse_auto(., 
             force = "group_A",
             k = "k")

binary_results <- bind_rows(create_node_edge_df(embeddings_binary) %>% mutate(type = "variable k"),
          create_node_edge_df(embeddings_binary_fixed) %>% mutate(type = "fixed k")
 ) 

binary_results %>% 
  ggplot(aes(x = tension_mean, y = elevation, colour = node)) + geom_jitter() +
  facet_wrap(~type) +
  labs(title = "Binary embeddings",
       x = "mean tension")


## ----message=FALSE------------------------------------------------------------


two_dimensional_embeddings <- biconnected_network %>%
  prepare_edges(.) %>%
  #prepare the continuous features as normal
  prepare_continuous_force(., node_names = "name", force_var = "force") %>%
  #prepare the categorical features as normal
  prepare_categorical_force(., node_names = "name", force_var = "group") %>%
  #embed them using the high dimensional function
  setse_auto_hd(., force = c("group_A", "force"), k = "weight")

two_dimensional_embeddings_fixed <- biconnected_network %>%
  prepare_edges(., k = 500) %>%
  #prepare the continuous features as normal
  prepare_continuous_force(., node_names = "name", force_var = "force") %>%
  #prepare the categorical features as normal
  prepare_categorical_force(., node_names = "name", force_var = "group") %>%
  #embed them using the high dimensional function
  setse_auto_hd(., force = c("group_A", "force"), k = "k")

bind_rows(two_dimensional_embeddings$node_embeddings %>% mutate(type = "variable k"),
two_dimensional_embeddings_fixed$node_embeddings %>% mutate(type = "fixed k")) %>%
  #The elevation variables are renamed for simplicity
  rename(categorical = elevation_group_A,
         continuous = elevation_force) %>%
  ggplot(aes(x = categorical, y = continuous, colour = node)) + geom_jitter() +
  facet_wrap(~type) +
  labs(title = "Node elevation for two different features",
       x = "elevation with continuous embedding",
       y = "elevation with categorical embedding")



