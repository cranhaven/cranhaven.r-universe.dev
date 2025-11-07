## ----include = FALSE----------------------------------------------------------
options(tidyverse.quiet = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
# load packages
library(tidyverse)
library(profiplots)
library(ggalluvial)
library(ggrepel)

# set the aesthetics (theme) of plots
profiplots::set_theme(pal_name = "blue-red", pal_name_discrete="discrete")

movie_series <- c(
  "The Phantom Menace",
  "Attack of the Clones",
  "Revenge of the Sith",
  "A New Hope",
  "The Empire Strikes Back",
  "Return of the Jedi",
  "The Force Awakens"
)

get_movie_order <- function(movie_names) {
  purrr::map_dbl(movie_names, function(mn) which(mn == movie_series))
}

# prepare dataset: Star Wars characters
sw <- 
  dplyr::starwars %>% 
  mutate(
    bmi = mass/(height/100)^2,
    is_droid = forcats::fct_explicit_na(if_else(sex == "none", "Droid", "Other"), "N/A"),
    first_film = purrr::map_chr(films, function(movies) {
      movie_ord = get_movie_order(movies)
      movies[which.min(movie_ord)]
    }),
    first_film = factor(first_film, labels = movie_series, ordered = TRUE),
    been_in_jedi = purrr::map_lgl(films, ~"Return of the Jedi" %in% .),
    n_films = purrr::map_dbl(films, length)
  )

## ----echo=TRUE----------------------------------------------------------------
plt <- 
  sw %>% 
  mutate(
    gender = forcats::fct_explicit_na(gender),  # Make the NA's be obvious (new level)
    gender = forcats::fct_infreq(gender),       # in case of `nominal` values, sort according to frequency
  ) %>% 
  ggplot(aes(x = gender)) + 
  stat_count(geom = "bar") + 
  labs(
    x = "Character gender",
    y = "Count",
    title = "Gender distribution among StarWars characters"
  )
plt

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  mutate(
    gender = forcats::fct_explicit_na(gender),  
    gender = forcats::fct_infreq(gender),       
  ) %>% 
  ggplot(aes(x = gender)) + 
  stat_count(geom = "bar", fill = profinit_cols("blue")) +    # Adding fill = 'color' makes the plot colored
  labs(
    x = "Character gender",
    y = "Count",
    title = "Gender distribution among StarWars characters"
  )

## -----------------------------------------------------------------------------
sw %>% 
  mutate(
    gender = forcats::fct_explicit_na(gender),  
    gender = forcats::fct_infreq(gender),       
    gender = forcats::fct_rev(gender)           # Reverse the order to have the most prominent cat on top
  ) %>%    
  ggplot(aes(x = gender)) + 
  stat_count(geom = "bar", fill = profinit_cols("blue")) + 
  coord_flip() +                                # This way you can make the barplot horizontal
  labs(
    x = NULL,                                   # You may get rid of the axis (if the Title is self explanatory)
    y = "Count",
    title = "Gender distribution among StarWars characters"
  )

## -----------------------------------------------------------------------------
# prepare a highlighting scale to be reused elsewhere -- be consistent within your report
higlighting_cols <- 
  profinit_cols("grey", "red") %>% 
  purrr::set_names(c("FALSE", "TRUE"))

sw %>% 
  mutate(
    gender = forcats::fct_explicit_na(gender),   
    gender = forcats::fct_infreq(gender),        
  ) %>% 
  ggplot(aes(x = gender, fill = gender == "feminine")) +   # now we highlight category `feminine` via Boolean indicator
  stat_count(geom = "bar") + 
  scale_fill_manual(values = higlighting_cols) + # mapping of highlighting colors 
  guides(fill = "none") +                        # no need for legend, the `x` axis says it all
  labs(
    x = "Character gender",
    y = "Count",
    title = "Gender distribution among StarWars characters"
  )

## -----------------------------------------------------------------------------
sw %>% 
  mutate(
    # in case of `ordinal` values, sort according to their order 
    # (in this case, we treat numbers as category labels). 
    # Plus have it factor/character for better `x` axis
    n_films = forcats::fct_inseq(as.character(n_films)),
  ) %>% 
  ggplot(aes(x = n_films)) +   
  stat_count(geom = "bar") + 
  labs(
    x = "In how many films is the character present?",
    y = "Character count",
    title = "What is the character durability in StarWars films?"
  )

## -----------------------------------------------------------------------------
set.seed(123)

data.frame(
  x = sample(LETTERS[1:7], prob = 1/(1 + 1/(1:7)), size = 1e5, replace = TRUE)
) %>% 
  ggplot(aes(x = x)) + 
  stat_count(geom = "point") +           # THIS is the way to change geom (bar -> point)
  scale_y_continuous(
    limits = c(7000, 17000),             # To truncate the y-axis 
    labels = scales::number              # To get better looking numbers on y-axis
  ) + 
  labs(
    x = "Category",
    y = "Number of observations",
    title = "Frequency of artificial categories"
  )

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=TRUE----------------------------------------------------------------
plt <- 
  sw %>% 
  ggplot(aes(x = height)) + 
  stat_bin(geom = "bar", bins = 20) +  
  labs(
    x = "Height [cm]",
    y = "Count",
    title = "Height distribution of StarWars characters"
  )
plt

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  ggplot(aes(x = height)) + 
  stat_bin(geom = "bar", bins = 20, fill = profinit_cols("blue")) +    # This way you make the plot be colored
  labs(
    x = "Height [cm]",
    y = "Count",
    title = "Height distribution of StarWars characters"
  )

## -----------------------------------------------------------------------------
sw %>% 
  ggplot(aes(x = height)) + 
  stat_bin(geom = "bar", binwidth = 25) +  
  labs(
    x = "Height [cm]",
    y = "Count",
    title = "Height distribution of StarWars characters",
    subtitle = "Binwidth set to 25"
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = height, fill = is_droid)) + 
  scale_fill_profinit(palette = "discrete-full", exact = TRUE) + 
  stat_bin(geom = "bar", bins = 20, position = "identity", alpha = .7) + 
  labs(
    x = "Height [cm]",
    y = "Count",
    fill = "Character type",
    title = "Height distribution of StarWars characters",
  ) + 
  theme(legend.position = "bottom")             # You can move the legend to use the full width of the plot for distribution

## -----------------------------------------------------------------------------
is_droid_color_mapping <- 
  profinit_pal("discrete-full")(3) %>% 
  set_names("Droid", "Other", "N/A")

sw %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = height, fill = is_droid)) + 
  scale_fill_manual(values = is_droid_color_mapping) + 
  stat_bin(geom = "bar", bins = 20, position = "identity", alpha = .7) + 
  labs(
    x = "Height [cm]",
    y = "Count",
    fill = "Character type",
    title = "Height distribution of StarWars characters",
  ) + 
  theme(legend.position = "bottom")  

## ----echo=TRUE----------------------------------------------------------------
hist(
  x = sw$height,
  breaks = 20,                 # (optional) tweak default setting of bins number
  border = NA,                 # bins border color, NA to turn it off
  col = profinit_cols("blue"), # bins fill color, use either of `profinit_cols()`, either `blue`, `red` or `grey` are preferable
  main = "Distribution of heights of StarWars characters",
  xlab = "Height [cm]",        # do not forget to mention units
  ylab = "Count",
  # TODO: change axes style
  # TODO: add grid
)

## ----echo=TRUE----------------------------------------------------------------
plt <- 
  sw %>% 
  filter(mass < 1000) %>% 
  ggplot(aes(x = bmi)) + 
  stat_density() +  
  labs(
    x = "Body mass index",
    y = "Density",
    title = "BMI distribution of StarWars characters",
    caption = "Characters under 1000kg  only."
  )
plt

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  filter(mass < 1000) %>% 
  ggplot(aes(x = bmi)) + 
  stat_density(fill = profinit_cols("blue")) +  
  labs(
    x = "Body mass index",
    y = "Density",
    title = "BMI distribution of StarWars characters",
    caption = "Characters under 1000kg  only."
  )

## ----echo=TRUE----------------------------------------------------------------
sw %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = height, fill = is_droid)) + 
  stat_density(
    alpha = .8,              # you shall use transparency in case of multiple overlapping groups
    position = "identity"    # do not position="stack" (default)!
  ) +  
  scale_fill_manual(values = is_droid_color_mapping) +  # Now, we're using fixed mapping to be consistent with other plots!
  labs(
    fill = "Character type",
    x = "Height [cm]",
    y = "Density",
    title = "Height distribution of StarWars characters"
  ) + 
  theme(legend.position = "bottom")

## ----echo=TRUE----------------------------------------------------------------
sw %>%
  ggplot(aes(x = height, fill = first_film)) + 
  stat_density(position = "identity", alpha = .3) +                # THIS way you can have fill very transparent
  scale_fill_profinit_d("blue-red") +
  guides(color = "none", fill = guide_legend(override.aes = list(alpha = .8))) +                                        # THIS way you won't have duplicated legend
  labs(
    fill = "First film of the character",
    x = "Height [cm]",
    y = "Density",
    title = "Height distribution of StarWars characters",
    subtitle = "Given the first films the character played in"
  )

## -----------------------------------------------------------------------------
higlight_fill_mapping <- c("TRUE" = profinit_cols("red"), "FALSE" = profinit_cols("gray"))
higlight_alpha_mapping <- c("TRUE" = .75, "FALSE" = .25)

higlight_cat <-  "Revenge of the Sith"

sw %>% 
  mutate(
    higlight_group = as.character(first_film == higlight_cat)
  ) %>% 
  ggplot(
    aes(
      x = height, 
      group = first_film, 
      fill = higlight_group, 
      alpha = higlight_group,
    )
  ) + 
  stat_density(
    position = "identity"
  ) + 
  scale_fill_manual(values = higlight_fill_mapping) + 
  scale_alpha_manual(values = higlight_alpha_mapping) + 
  annotate(x = 135, y = 0.014, geom = "text", label = "Revange\nof the Sith", color = profinit_cols("red")) + 
  annotate(x = 240, y = 0.014, geom = "text", label = "A New Hope", color = profinit_cols("grey"), alpha = .4) + 
  annotate(x = 210, y = 0.045, geom = "text", label = "The Phantom\nManace", color = profinit_cols("grey"), alpha = .4) + 
  guides(alpha =  "none", fill = "none") +                                        # THIS way you won't have duplicated
  labs(
    x = "Height [cm]",
    y = "Density",
    title = paste0("Characters in SW:", higlight_cat, " tends to be smaller"),
    subtitle = "Characters grouped by the first SW films they played in"
  )

## ----echo=TRUE----------------------------------------------------------------
# TODO: provide more straightforward approach

height_density = density(sw$height,na.rm = TRUE)
plot(
  height_density, 
  col = NA, 
  main = "Height distribution of StarWars characters",
  xlab = "Height [cm]",
  ylab = "Density"
)
polygon(
  x = height_density,
  col = profinit_cols("blue"), 
  border = NA,
)

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
plt <- 
  sw %>% 
  filter(is_droid != "N/A") %>% 
  ggplot() +
  aes(fill = is_droid, x =  1, y = ..count..) + 
  stat_count(position = "stack") + 
  guides(x = "none") + 
  scale_fill_manual(values = is_droid_color_mapping) + 
  scale_y_continuous(breaks = seq(0, 100, 10)) +              # customize Y axis ticks position
  labs(
    x = NULL,
    y = "Character count",
    fill = "Character type",
    title = "Proportion of droids among SW characters",
    subtitle = "Based on dplyr::starwars dataset",
    caption = "Characters with known status only"
  ) + 
  theme(
    legend.position = "bottom"
  )

## -----------------------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  filter(is_droid != "N/A") %>% 
  ggplot() +
  aes(fill = is_droid, x =  1, y = ..count.., label = ..count..) +      
  stat_count(position = "stack") + 
  stat_count(position = position_stack(vjust = 0.5), geom = "text") + 
  guides(x = "none") + 
  scale_fill_manual(values = is_droid_color_mapping) + 
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  labs(
    x = NULL,
    y = "Character count",
    fill = "Character type",
    title = "Proportion of droids among SW characters",
    subtitle = "Based on dplyr::starwars dataset",
    caption = "Characters with known status only"
  ) + 
  theme(
    legend.position = "bottom"
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(is_droid != "N/A") %>% 
  ggplot() +
  aes(fill = is_droid, x =  1, y = ..count..) +         # no need to change the y = ..count.., position_fill will do that for you
  stat_count(position = "fill") + 
  guides(x = "none") + 
  scale_fill_manual(values = is_droid_color_mapping) +  # to fix the color mapping
  scale_y_continuous(
    breaks = seq(0, 1, .1),                             # customize Y axis ticks position
    labels = scales::percent_format(suffix = " %")) +   # customize Y axis ticks labels, use ` %` (CZ) or `%` (EN)
  labs(
    x = NULL,
    y = "Proportion of characters",
    fill = "Character type",
    title = "Proportion of droids among SW characters",
    subtitle = "Based on dplyr::starwars dataset",
    caption = "Characters with known status only"
  ) + 
  theme(
    legend.position = "bottom"                          # to have the legend below the chart
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(is_droid != "N/A") %>% 
  ggplot() +
  aes(fill = is_droid, x =  1, y = ..count..) +               # You can change the X and y mapping (not shown)
  coord_flip() +                                              # ... or just flip the axes
  stat_count(position = "stack") + 
  guides(y = "none") +                                        # Turn of the 'primary' axis, y in this case
  scale_fill_manual(values = is_droid_color_mapping) +        # Set the color mapping to be consistent
  scale_x_continuous(breaks = seq(0, 100, 10)) +              # customize x axis ticks position
  labs(
    x = NULL,
    y = "Character count",
    fill = "Character type",
    title = "Proportion of droids among SW characters",
    subtitle = "Based on dplyr::starwars dataset",
    caption = "Characters with known status only"
  ) + 
  theme(
    legend.position = "bottom"                                # Set the legend position
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(!is.na(gender)) %>% 
  mutate(is_droid = forcats::fct_rev(is_droid)) %>% 
  ggplot() + 
  aes(x = gender, fill = is_droid) + 
  stat_count(position = position_fill()) + 
  scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent) + 
  scale_fill_manual(values = is_droid_color_mapping) + 
  labs(
    title = "Droid proportion is the same accross Gender",
    x = "Gender",
    y = "Proportion of droids",
    fill = "Character type",
    caption = "Characters with known Gender only"
  )

## -----------------------------------------------------------------------------
droid_prop_overall <- mean(sw$is_droid == "Droid", na.rm = TRUE)
droid_prop_overall_label <- paste0("Overall mean: ", scales::percent(droid_prop_overall, accuracy = .01))

sw %>% 
  filter(!is.na(gender)) %>% 
  mutate(is_droid = forcats::fct_rev(is_droid)) %>% 
  ggplot() + 
  aes(x = gender, fill = is_droid) + 
  stat_count(position = position_fill()) + 
  stat_identity(geom = "hline", yintercept = droid_prop_overall, linetype = "dashed", color = profinit_cols("grey")) +
  annotate(x = 2.1, y = droid_prop_overall - .01, geom = "text", label = droid_prop_overall_label, size = 2.5) + 
  scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent) + 
  scale_fill_manual(values = is_droid_color_mapping) + 
  labs(
    title = "Droid proportion is the same accross Gender",
    x = "Gender",
    y = "Proportion of droids",
    fill = "Character type",
    caption = "Characters with known Gender only"
  )

## ----echo=FALSE---------------------------------------------------------------
plt <- sw %>% 
  filter(!is.na(gender)) %>% 
  ggplot() + 
  aes(
    x = gender,
    fill = first_film
  ) + 
  stat_count(position = position_dodge(preserve = "single")) + 
  scale_fill_profinit() + 
  labs(
    x = "Gender of the character",
    y = "Count",
    fill = "First SW film of the character",
    title = "At what film first star the most characters of given gender"
  )
plt

## -----------------------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  filter(!is.na(gender)) %>% 
  mutate(is_droid = forcats::fct_rev(is_droid)) %>%                             # More important level comes first
  ggplot() + 
  aes(x = height, fill = is_droid) + 
  stat_density(geom = "area", position = position_fill()) +                     # Here we specify stacking(fill)
  scale_fill_manual(values = is_droid_color_mapping) +                          # To be consistent in the report
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .1)) +        # Pleasant y-axis labels
  labs(
    title = "Proportion of DROIDS among SW characters of a given height",
    x = "Height [cm]",
    y = "Proportion of droids",
    fill = "Character type",
    caption = "Characters with known gender only"                               # Describe the population in use
  ) + 
  theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
is_movie_present <- function(films) {
  purrr::map_dbl(movie_series, function(movie_name) movie_name %in% films) %>% 
    purrr::set_names(movie_series)
}



sw_wide_agg <- 
  sw %>% 
  mutate(
    x = purrr::map(films, is_movie_present)
  ) %>% 
  unnest_wider(x) %>% 
  filter(`A New Hope` == 1 | `Return of the Jedi` == 1 | `The Force Awakens` == 1) %>% 
  group_by_at(vars(`A New Hope`, `Return of the Jedi`, `The Force Awakens`)) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate_at(vars(`A New Hope`, `Return of the Jedi`, `The Force Awakens`), ~ifelse(. == 1, "Present", "Skipped")) 

sw_wide_agg %>% 
  ggplot() + 
  aes(y = n/sum(n), axis1 = `A New Hope`, axis2 = `Return of the Jedi`, axis3 = `The Force Awakens`) + 
  geom_alluvium(aes()) + 
  geom_stratum(aes(fill = after_stat(stratum)), color = "#00000000") + 
  scale_x_discrete(limits = c("A New Hope", "Return of the Jedi", "The Force Awakens")) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Characters being recycled in last 3 movies",
    x = NULL, 
    y = "Proportion of characters",
    fill = "Character in the movie",
    caption = "Characters present in at least one of the movies"
  ) + 
  theme(
    legend.position = "bottom"
  )

## ----echo=TRUE----------------------------------------------------------------
plt <-
  sw %>% 
  filter(mass < 1e3) %>% 
  ggplot(aes(x = height, y = mass)) + 
  geom_point() + 
  labs(
    x = "Height [cm]",
    y = "Weight [kg]",
    title = "Height ~ weight relation of StarWars characters",
    note = "Characters weighting less hten 1t"            # Indicate population filters!
  )
plt

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
obesity_color_mapping <- c("Overweight" = profinit_cols("red"), "Slim" = profinit_cols("grey"))
sw %>% 
  filter(mass < 1e3) %>% 
  mutate(
    higlight = if_else(bmi > 33, "Overweight", "Slim")
  ) %>% 
  ggplot(aes(x = height, y = mass, color = higlight)) + 
  scale_color_manual(values = obesity_color_mapping) + 
  geom_point() + 
  labs(
    x = "Height [cm]",
    y = "Weight [kg]",
    title = "StarWars characters with obesity problem",
    subtitle = "Height ~ weight relation of StarWars characters",
    caption = "Characters weighting less then 1000kg only\nBMI = weight[kg]/(height[m])^2"            # Indicate population filters!
  ) + 
  theme(
    legend.position = "bottom"
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(mass < 1e3) %>% 
  mutate(
    higlight = if_else(bmi > 33, "Overweight", "Slim")
  ) %>% 
  ggplot(aes(x = height, y = mass, color = higlight)) + 
  geom_text_repel(                                                         # geom_text does not set the rectangle
    data = function(d) filter(d, mass/(height/100)^2 > 33),           # You can use `data` to provide filtering
    aes(label = name),                                                # Specify the label (text) mapping
    size = 2.2
  ) + 
  geom_point() + 
  scale_color_manual(values = obesity_color_mapping) + 
  labs(
    x = "Height [cm]",
    y = "Weight [kg]",
    color = "BMI status", 
    title = "Overweighted characters in StarWars",
    subtitle = "Characters with BMI > 33",
    note = "Characters weighting less hten 1t"            # Indicate population filters!
  ) + 
  theme(
    legend.position = "bottom"
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(mass < 1e3, is_droid != "N/A") %>% 
  ggplot(aes(x = height, y = mass, color = is_droid)) + 
  geom_point(size = 1) + 
  geom_smooth(method = "lm", se = FALSE, formula = "y~x") + # THIS way you introduce best LM fit y~x without error bound
  labs(
    x = "Height [cm]",
    y = "Weight [kg]",
    title = "Height ~ weight relation of StarWars characters",
    note = "Characters weighting less hten 1t\nTrend line of `y ~ x`"            
  )

## -----------------------------------------------------------------------------
r2d2 <-
  sw %>% 
  filter(name == "R2-D2")

sw %>% 
  filter(mass < 1e3) %>% 
  ggplot(aes(x = height, y = mass, color = name == "R2-D2")) + 
  geom_point(size = 1) + 
  geom_abline(intercept = 150, slope = -.05, color = profinit_cols("blue")) +   # THIS is the way to add arbitrary line
  annotate(x = 220, y = 135, label = "Arbitrary line", color = profinit_cols("blue"), geom = "text", size = 2.7) + 
  geom_vline(xintercept = r2d2$height, linetype = "dashed", color = profinit_cols("red")) +                       # THIS way you introduce vertical lines & customize their line style
  annotate(x = r2d2$height, y = 135, label = "R2-D2", geom = "text", color = profinit_cols("red"), hjust=1.2, size = 2.7) + 
  scale_color_manual(values = c("TRUE" = profinit_cols("red"), "FALSE" = profinit_cols("grey"))) + 
  labs(
    x = "Height [cm]",
    y = "Weight [kg]",
    title = "Height ~ weight relation of StarWars characters",
    subtitle = "In comparision with R2-D2",
    note = "Characters weighting less hten 1t"           
  )

## -----------------------------------------------------------------------------
sw %>% 
  ggplot(aes(x = height, y = mass)) + 
  geom_point() + 
  scale_y_log10() +                                                # this is as easy as setting y-scale 
  labs(
    x = "Height [cm]",
    y = "Weight [kg], log10 scaled",                               # be sure you inform your reader
    title = "Height ~ weight relation of StarWars characters",
  )

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=TRUE----------------------------------------------------------------
plt <- 
  sw %>% 
  filter(mass < 1000) %>% 
  ggplot(aes(x = height, y = mass)) +
  stat_density2d_filled() +  
  scale_fill_profinit("blues", reverse = TRUE) + 
  labs(
    x = "Height [cm]",
    y = "Mass [kg]",
    caption = "Characters below 1000kg only",
    title = "Height ~ Mass relationship among SW Characters"
  )

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  filter(mass < 1000) %>% 
  ggplot(aes(x = height, y = mass)) +
  stat_density2d(geom = "path", aes(color = after_stat(level))) +  
  scale_color_profinit_c("reds-dark", reverse = TRUE, labels = scales::number) + 
  labs(
    x = "Height [cm]",
    y = "Mass [kg]",
    color = "Density",
    caption = "Characters below 1000kg only",
    title = "Height ~ Mass relationship among SW Characters"
  )

## -----------------------------------------------------------------------------
sw %>% 
  filter(mass < 1000) %>% 
  ggplot(aes(x = height, y = mass)) +
  stat_bin_2d() +  
  scale_fill_profinit_c() + 
  labs(
    x = "Height [cm]",
    y = "Mass [kg]",
    caption = "Characters below 1000kg only",
    title = "Height ~ Mass relationship among SW Characters"
  ) + 
  coord_equal()

## -----------------------------------------------------------------------------
sw %>% 
  filter(mass < 1000) %>% 
  ggplot(aes(x = height, y = mass)) +
  stat_bin_hex() + 
  scale_fill_profinit_c() + 
  labs(
    x = "Height [cm]",
    y = "Mass [kg]",
    caption = "Characters below 1000kg only",
    title = "Height ~ Mass relationship among SW Characters"
  ) + 
  coord_equal()

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=FALSE---------------------------------------------------------------
# TODO

## ----echo=TRUE----------------------------------------------------------------
plt <- sw %>% 
  group_by(first_film, gender) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = gender, y = first_film, fill=n)) + 
  stat_identity(geom = "tile") + 
  scale_fill_profinit_c("blues", reverse = TRUE) + 
  labs(
    x = "Character gender",
    y = "First film of the character",
    fill = "Count", 
    title = "Where do the characters of given gender mostly starts?"
  )

## ----echo=FALSE---------------------------------------------------------------
plt

## -----------------------------------------------------------------------------
sw %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender, is_droid) %>% 
  summarise(
    n_total = n(),
    n_overweight = sum(bmi > 30, na.rm = TRUE),
    odds_overweight = n_overweight/(n_total - n_overweight),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = gender, y = is_droid, fill = odds_overweight)) + 
  stat_identity(geom = "tile") + 
  scale_fill_gradient2(low = profinit_cols("red"), mid = "white", high = profinit_cols("blue"), midpoint = 1) + 
  labs(
    x = "Character gender",
    y = "Character type",
    fill = "Overweight\nOdds", 
    title = "What is the odds to be overweighted?",
    subtitle = "Based on Gender and being droid in SW",
    caption = "Characters with known gender only"
  )

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=FALSE---------------------------------------------------------------
# TODO

## ----echo=TRUE----------------------------------------------------------------
plt <- ggplot()

## ----echo=FALSE---------------------------------------------------------------
plt

## ----echo=TRUE----------------------------------------------------------------
# TODO

## ----echo=FALSE---------------------------------------------------------------
# TODO

