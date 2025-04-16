## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE, 
  warning = FALSE,
  fig.height = 7, 
  fig.width = 7,
  dpi = 75
)

## ----eval = FALSE-------------------------------------------------------------
# # install from CRAN
# install.packages("geofi")
# 
# # Install development version from GitHub
# remotes::install_github("ropengov/geofi")

## ----include = FALSE, eval = TRUE---------------------------------------------
# Let's first create a function that checks if the suggested 
# packages are available
check_namespaces <- function(pkgs){
  return(all(unlist(sapply(pkgs, requireNamespace,quietly = TRUE))))
}
apiacc <- geofi::check_api_access()
pkginst <- check_namespaces(c("geofacet","ggplot2","dplyr"))
apiacc_pkginst <- all(apiacc,pkginst)

## ----municipality_keys, eval = pkginst----------------------------------------
library(geofi)
library(dplyr)
d <- data(package = "geofi")
as_tibble(d$results) %>% 
  select(Item,Title) %>% 
    filter(grepl("municipality_key", Item))

## ----municipality_key_names, eval = TRUE--------------------------------------
names(geofi::municipality_key_2023)

## ----municipality_key_maakunta, eval = pkginst--------------------------------
geofi::municipality_key_2023 %>% 
  count(maakunta_code,maakunta_name_fi,maakunta_name_sv,maakunta_name_en)

## ----municipality_map, fig.height = 7, fig.width = 4, eval = apiacc_pkginst----
municipalities <- get_municipalities(year = 2023, scale = 4500)
plot(municipalities["municipality_name_fi"], border = NA)

## ----muni_pop_map1, fig.height = 7, fig.width = 4, eval = apiacc_pkginst------
get_municipality_pop(year = 2022) %>%  
  subset(select = miehet_p) %>% 
  plot()

## ----muni_pop_map2, fig.height = 7, fig.width = 4, eval = apiacc_pkginst------
get_municipality_pop(year = 2022) %>%  
  group_by(hyvinvointialue_name_fi) %>%  
  summarise(vaesto = sum(vaesto)) %>%  
  select(vaesto) %>% 
  plot()

## ----muni_pop_map3, fig.height = 7, fig.width = 4, eval = apiacc_pkginst------
get_municipality_pop(year = 2022) %>%  
  dplyr::group_by(hyvinvointialue_name_fi) %>% 
  summarise(vaesto = sum(vaesto),
            miehet = sum(miehet)) %>% 
  mutate(share = miehet/vaesto*100) %>% 
  select(share) %>% 
  plot()

## ----zipcode_map, fig.height = 7, fig.width = 4, eval = apiacc_pkginst--------
zipcodes <- get_zipcodes(year = 2023) 
plot(zipcodes["nimi"], border = NA)

## ----statisticsl_grid_data, fig.height = 7, fig.width = 4, eval = apiacc_pkginst----
stat_grid <- get_statistical_grid(resolution = 5, auxiliary_data = TRUE)
plot(stat_grid["euref_x"], border = NA)

## ----population_grid_data, fig.height = 7, fig.width = 4, eval = apiacc_pkginst----
pop_grid <- get_population_grid(year = 2018, resolution = 5)
plot(pop_grid["kunta"], border = NA)

## ----central_localities, fig.height = 7, fig.width = 4, eval = apiacc_pkginst----
plot(municipality_central_localities["teksti"])

## ----geofacets, eval = apiacc_pkginst-----------------------------------------
d <- data(package = "geofi")
as_tibble(d$results) %>% 
  select(Item,Title) %>% 
    filter(grepl("grid", Item)) %>% 
  print(n = 100)

## ----geofacet, fig.height = 8, fig.width = 10, eval = apiacc_pkginst----------
# Let pull population data from THL
sotkadata <- geofi::sotkadata_population

# lets aggregate population data
dat <- left_join(geofi::municipality_key_2023 %>% select(-year),
                 sotkadata) %>% 
  group_by(maakunta_code, maakunta_name_fi,year) %>% 
  summarise(population = sum(primary.value, na.rm = TRUE)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  rename(code = maakunta_code, name = maakunta_name_fi)

library(geofacet)
library(ggplot2)

ggplot(dat, aes(x = year, y = population/1000, group = name)) + 
  geom_line() + 
  facet_geo(facets = ~name, grid = grid_maakunta, scales = "free_y") +
  theme(axis.text.x = element_text(size = 6)) +
  scale_x_discrete(breaks = seq.int(from = 2000, to = 2023, by = 5)) +
  labs(title = unique(sotkadata$indicator.title.fi), y = "%")


