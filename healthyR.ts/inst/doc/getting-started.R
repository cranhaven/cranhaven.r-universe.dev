## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE,
    fig.width = 8, 
    fig.height = 4.5,
    fig.align = 'center',
    out.width='95%', 
    dpi = 100,
    collapse = TRUE,
    comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(healthyR.ts)
library(ggplot2)
library(dplyr)

## ----ts_random_walk-----------------------------------------------------------
df <- ts_random_walk()
head(df)
glimpse(df)

## ----ts_random_walk_ggplot_layers---------------------------------------------
df %>%
   ggplot(
       mapping = aes(
           x = x
           , y = cum_y
           , color = factor(run)
           , group = factor(run)
        )
    ) +
    geom_line(alpha = 0.8) +
    ts_random_walk_ggplot_layers(df)

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

df %>%
    group_by(x) %>%
    summarise(
        min_y = min(cum_y),
        max_y = max(cum_y)
    ) %>%
    ggplot(
        aes(x = x)
    ) +
    geom_line(aes(y = max_y), color = "steelblue") +
    geom_line(aes(y = min_y), color = "firebrick") +
    geom_ribbon(aes(ymin = min_y, ymax = max_y), alpha = 0.2) +
    ts_random_walk_ggplot_layers(df)

## -----------------------------------------------------------------------------

# Random Walk for volatility range 1-15%
df1 <- ts_random_walk(.sd = 0.01)
df2 <- ts_random_walk(.sd = 0.05)
df3 <- ts_random_walk(.sd = 0.10)
df4 <- ts_random_walk(.sd = 0.15)

# Merge data frames into one
df_merged <- dplyr::bind_rows(
    df1 %>% mutate(ver = "A) Vol 1%"),
    df2 %>% mutate(ver = "B) Vol 5%"),
    df3 %>% mutate(ver = "C) Vol 10%"),
    df4 %>% mutate(ver = "D) Vol 15%")
)

# Plot range between minimum and maximum values
df_merged %>%
    ggplot(aes(
        x = x, y = cum_y,
        color = factor(run), group = factor(run)
    )) +
    geom_line(alpha = 0.8) +
    labs(title = "", x = "", y = "") +
    facet_wrap(~ver, scales = "free") +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none")

