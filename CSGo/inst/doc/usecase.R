## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  library(CSGo)
#  
#  # to get the statistics of the user 76561198263364899
#  rodrigo_stats <- get_stats_user(api_key = 'your_key', user_id = '76561198263364899')
#  

## ----eval = FALSE-------------------------------------------------------------
#  library(dplyr)
#  library(stringr)
#  
#  rodrigo_weapon_kill <- rodrigo_stats %>%
#    filter(
#      str_detect(name, 'kill'),
#      type == ' weapon info'
#    ) %>%
#    arrange(desc(value))
#  

## ----eval = FALSE-------------------------------------------------------------
#  library(ggplot2)
#  library(showtext)
#  
#  ## Loading Google fonts (https://fonts.google.com/)
#  font_add_google("Quantico", "quantico")
#  
#  rodrigo_weapon_kill %>%
#    top_n(n = 10, wt = value) %>%
#    ggplot(aes(x = name_match, y = value, fill = name_match)) +
#    geom_col() +
#    ggtitle("KILLS BY WEAPON") +
#    ylab("Number of Kills") +
#    xlab("") +
#    labs(fill = "Weapon Name") +
#    theme_csgo(text = element_text(family = "quantico")) +
#    scale_fill_csgo()
#  

## ----eval = TRUE, message = FALSE, echo = FALSE-------------------------------
library(knitr)
rodrigo_efficiency <- readRDS('data/rodrigo_efficiency.RDS')

## ----eval = FALSE, message = FALSE, results='asis'----------------------------
#  
#  rodrigo_efficiency <- rodrigo_stats %>%
#    filter(
#      name_match %in% c("ak47", "aug", "awp", "fiveseven",
#                        "hkp2000", "m4a1", "mp7", "p90",
#                        "sg556", "xm1014")
#    ) %>%
#    mutate(
#      stat_type = case_when(
#        str_detect(name, "shots") ~ "shots",
#        str_detect(name, "hits") ~ "hits",
#        str_detect(name, "kills") ~ "kills"
#      )
#    ) %>%
#    pivot_wider(
#      names_from = stat_type,
#      id_cols = name_match,
#      values_from = value
#    ) %>%
#    mutate(
#      kills_efficiency = kills/shots*100,
#      hits_efficiency = hits/shots*100,
#      hits_to_kill = kills/hits*100
#    )
#  
#  kbl(rodrigo_efficiency) %>%
#    kable_styling()

## ----eval = TRUE, message = FALSE, echo = FALSE-------------------------------
knitr::kable(rodrigo_efficiency)

## ----eval = FALSE-------------------------------------------------------------
#  rodrigo_efficiency %>%
#    top_n(n = 10, wt = kills) %>%
#    ggplot(aes(x = name_match, size = shots)) +
#    geom_point(aes(y = kills_efficiency, color = "Kills Efficiency")) +
#    geom_point(aes(y = hits_efficiency, color = "Hits Efficiency")) +
#    geom_point(aes(y = hits_to_kill, color = "Hits to Kill")) +
#    ggtitle("WEAPON EFFICIENCY") +
#    ylab("Efficiency (%)") +
#    xlab("") +
#    labs(color = "Efficiency Type", size = "Shots") +
#    theme_csgo(
#      text = element_text(family = "quantico"),
#      panel.grid.major.x = element_line(size = .1, color = "black",linetype = 2)
#    ) +
#    scale_color_csgo()
#  

