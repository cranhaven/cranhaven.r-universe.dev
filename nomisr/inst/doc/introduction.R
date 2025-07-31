## ----specific-dataset, echo=TRUE----------------------------------------------
library(nomisr)
y <- nomis_data_info("NM_893_1")

tibble::glimpse(y)

## ----specific-dataset-exam, eval=FALSE----------------------------------------
#  library(dplyr, warn.conflicts = F)
#  
#  y$annotations.annotation %>% class()
#  
#  y$annotations.annotation %>% length()
#  
#  y$annotations.annotation[[1]] %>% class()
#  
#  y %>% pull(annotations.annotation) %>% class()
#  
#  y %>% pull(annotations.annotation) %>% .[[1]] %>% class()
#  
#  y %>% pull(annotations.annotation) %>% purrr::pluck() %>% class()
#  
#  ## Unnesting list columns
#  y %>% tidyr::unnest(annotations.annotation) %>% glimpse()

## ----data-searching, eval=FALSE-----------------------------------------------
#  a <- nomis_search(name = '*jobseekers*', keywords = 'Claimants')
#  
#  tibble::glimpse(a)
#  
#  a %>% tidyr::unnest(components.attribute) %>% glimpse()
#  
#  b <- nomis_search(keywords = c('Claimants', '*Year*'))
#  
#  tibble::glimpse(b)
#  
#  b %>% tidyr::unnest(components.attribute) %>% glimpse()
#  

## ----overview, eval=FALSE-----------------------------------------------------
#  q <- nomis_overview("NM_1650_1")
#  
#  q %>% tidyr::unnest(name) %>% glimpse()
#  

## ----overview-select, eval=FALSE----------------------------------------------
#  s <- nomis_overview("NM_1650_1", select = c("units", "keywords"))
#  
#  s %>% tidyr::unnest(name) %>% glimpse()

## ----get-metadata, eval=FALSE-------------------------------------------------
#  a <- nomis_get_metadata(id = "NM_893_1")

## ----concepts, eval=FALSE-----------------------------------------------------
#  b <- nomis_get_metadata(id = "NM_893_1", concept = "GEOGRAPHY")

## ----geographies, eval=FALSE--------------------------------------------------
#  c <- nomis_get_metadata(id = "NM_893_1", concept = "geography", type = "type")

## ----constituencies, eval=FALSE-----------------------------------------------
#  d <- nomis_get_metadata(id = "NM_893_1",
#                          concept = "geography", type = "TYPE460")
#  

## ----ccg, eval=FALSE----------------------------------------------------------
#  z <- nomis_get_data(id = "NM_893_1", time = "latest", geography = "TYPE266")

## ----NM_893_1-gorton-withington, eval=FALSE-----------------------------------
#  x <- nomis_get_data(id = "NM_893_1", time = "latest",
#                      geography = c("1929380119", "1929380120"))

## ----jsa-claimaints, eval=FALSE-----------------------------------------------
#  library(ggplot2)
#  library(dplyr)
#  library(nomisr)
#  
#  jsa <- nomis_get_data(id = "NM_1_1", time = "2018-01-2021-10",
#                        geography = "TYPE480", measures=20201,
#                        sex=c(5,6), item = 1, tidy = TRUE)
#  
#  jsa <- jsa %>%
#    mutate(date = as.Date(paste0(date, "-01")),
#           obs_value = obs_value/100)
#  
#  theme_set(theme_bw())
#  
#  p_jsa <- ggplot(jsa, aes(x = date, y = obs_value, colour = sex_name)) +
#    geom_line(size = 1.15) +
#    scale_colour_viridis_d(end = 0.75, begin = 0.1, name = "Gender") +
#    scale_x_date(breaks = "6 months", date_labels = "%b %Y") +
#    scale_y_continuous(labels = scales::percent) +
#    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
#          legend.position = "bottom") +
#    labs(x = "Date", y= "JSA Claimants (Percentage of Workforce)") +
#    facet_wrap(~geography_name, scales = "free_y")
#  
#  p_jsa

