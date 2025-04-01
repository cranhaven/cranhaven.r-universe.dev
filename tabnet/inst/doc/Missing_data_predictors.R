## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(tidymodels, quietly = TRUE)
#  library(tabnet)
#  data("ames", package = "modeldata")
#  qplot(ames$Mas_Vnr_Area)

## -----------------------------------------------------------------------------
#  col_with_zero_as_na <- ames %>%
#    select(where(is.numeric)) %>%
#    select(matches("_SF|Area|Misc_Val|[Pp]orch$")) %>%
#    summarise_each(min) %>%
#    select_if(~.x==0) %>%
#    names()
#  ames_missing <- ames %>% mutate_at(col_with_zero_as_na, na_if, 0) %>%
#    mutate_at("Alley", na_if, "No_Alley_Access") %>%
#    mutate_at("Fence", na_if, "No_Fence") %>%
#    mutate_at(c("Garage_Cond", "Garage_Finish"), na_if, "No_Garage") %>%
#    mutate_at(c("Bsmt_Exposure", "BsmtFin_Type_1", "BsmtFin_Type_2"), na_if, "No_Basement")
#  
#  visdat::vis_miss(ames_missing)

## -----------------------------------------------------------------------------
#  ames_rec <- recipe(Sale_Price ~ ., data=ames) %>%
#    step_normalize(all_numeric())
#  
#  cat_emb_dim <- map_dbl(ames %>% select_if(is.factor), ~log2(nlevels(.x)) %>% round)
#  
#  ames_pretrain <- tabnet_pretrain(ames_rec, data=ames,  epoch=50, cat_emb_dim = cat_emb_dim,
#                              valid_split = 0.2, verbose=TRUE, batch=2930,
#                              early_stopping_patience = 3L, early_stopping_tolerance = 1e-4)
#  autoplot(ames_pretrain)

## -----------------------------------------------------------------------------
#  col_with_missings <- ames_missing %>%
#    summarise_all(~sum(is.na(.))>0) %>%
#    t %>% enframe(name="Variable") %>%
#    rename(has_missing="value")
#  
#  vip_color <- function(object, col_has_missing) {
#    vip_data <- vip::vip(object)$data %>% arrange(Importance)
#    vis_miss_plus <- left_join(vip_data, col_has_missing , by="Variable") %>%
#      mutate(Variable=factor(Variable, levels = vip_data$Variable))
#    vis_miss_plus
#    ggplot(vis_miss_plus, aes(x=Variable, y=Importance, fill=has_missing)) +
#      geom_col() + coord_flip() + scale_fill_grey()
#  }
#  vip_color(ames_pretrain, col_with_missings)

## -----------------------------------------------------------------------------
#  ames_missing_rec <- recipe(Sale_Price ~ ., data=ames_missing) %>%
#    step_normalize(all_numeric())
#  ames_missing_pretrain <- tabnet_pretrain(ames_missing_rec, data=ames_missing, epoch=50,
#                                      cat_emb_dim = cat_emb_dim,
#                                      valid_split = 0.2, verbose=TRUE, batch=2930,
#                                      pretraining_ratio=0.37,
#                                      early_stopping_patience = 3L, early_stopping_tolerance = 1e-4)
#  autoplot(ames_missing_pretrain)
#  vip_color(ames_missing_pretrain, col_with_missings)

## -----------------------------------------------------------------------------
#  ames_fit <- tabnet_pretrain(ames_rec, data=ames,  tabnet_model = ames_pretrain,
#                              epoch=50, cat_emb_dim = cat_emb_dim,
#                              valid_split = 0.2, verbose=TRUE, batch=2930,
#                              early_stopping_patience = 5L, early_stopping_tolerance = 1e-4)
#  autoplot(ames_fit)
#  vip_color(ames_fit, col_with_missings)

## -----------------------------------------------------------------------------
#  ames_missing_fit <- tabnet_pretrain(ames_rec, data=ames_missing,  tabnet_model = ames_missing_pretrain,
#                              epoch=50, cat_emb_dim = cat_emb_dim,
#                              valid_split = 0.2, verbose=TRUE, batch=2930,
#                              early_stopping_patience = 5L, early_stopping_tolerance = 1e-4)
#  autoplot(ames_missing_fit)
#  vip_color(ames_missing_fit, col_with_missings)

