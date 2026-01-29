## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "passport-"
)

## ----intro, message=FALSE-----------------------------------------------------
library(passport)
library(gapminder)
library(dplyr)    # Works equally well in any grammar.
library(tidyr)
set.seed(47)

grep("Korea", unique(gapminder$country), value = TRUE)
grep("Yemen", unique(gapminder$country), value = TRUE)

## ----standardize-1------------------------------------------------------------
gap <- gapminder %>% 
    # standardize to ISO 3166 Alpha-2 code
    mutate(country_code = parse_country(country))

gap %>%
    select(country, country_code, year, lifeExp) %>%
    sample_n(10)

## ----standardize-2, eval=FALSE------------------------------------------------
#  parse_country(c("somewhere in Japan", "日本", "Japon", "जापान"), how = "google")
#  #> [1] "JP" "JP" "JP" "JP"
#  
#  parse_country(c("1600 Pennsylvania Ave, DC", "Eiffel Tower"), how = "google")
#  #> [1] "US" "FR"

## ----convert-1, message = FALSE-----------------------------------------------
# NATO member defense expenditure data; see `?nato`
data("nato", package = "passport")

nato %>% 
    select(country_stanag) %>% 
    distinct() %>%
    mutate(
        country_iso = as_country_code(country_stanag, from = "stanag"),
        country_name = as_country_name(country_stanag, from = "stanag", short = FALSE),
        country_name_thai = as_country_name(country_stanag, from = "stanag", to = "ta-my")
    )

## ----format, fig.width=5------------------------------------------------------
library(ggplot2)

living_longer <- gap %>% 
    group_by(country_code) %>% 
    summarise(start_life_exp = lifeExp[which.min(year)], 
              stop_life_exp = lifeExp[which.max(year)], 
              diff_life_exp = stop_life_exp - start_life_exp) %>% 
    top_n(10, diff_life_exp) 

# Plot country codes...
ggplot(living_longer, aes(x = country_code, y = stop_life_exp - 4.5,
                          ymin = start_life_exp, 
                          ymax = stop_life_exp - 4.5, 
                          colour = factor(diff_life_exp))) + 
    geom_point(pch = 17, size = 7) + 
    geom_linerange(size = 5) + 
                     # ...just pass `labels` a formatter function!
    scale_x_discrete(labels = country_format(),
                     # Easily change order
                     limits = order_countries(living_longer$country_code, 
                                              living_longer$diff_life_exp)) + 
    scale_y_continuous(limits = c(30, 80)) + 
    labs(title = "Life gets better",
         subtitle = "Largest increase in life expectancy",
         x = NULL, y = "Life expectancy") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          legend.position = "none")

