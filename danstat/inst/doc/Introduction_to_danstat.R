## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 10,
  fig.height = 5,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(danstat)
library(purrr)
library(dplyr)
library(ggplot2)
library(kableExtra)

## -----------------------------------------------------------------------------
get_subjects()

## -----------------------------------------------------------------------------
subj <- get_subjects(subjects = c("6","7"))
subsubjects <- subj$subjects %>% bind_rows()
subsubjects

## -----------------------------------------------------------------------------
tables <- get_tables(subjects = c("3465", "3413")) 
tables %>% 
  select(id, text, variables) %>% 
  kable()

## -----------------------------------------------------------------------------
vars_acc <- get_table_metadata(table_id = "uheld4", variables_only = TRUE)
vars_alco <- get_table_metadata(table_id = "alko3", variables_only = TRUE)

vars_acc %>% 
  select(id, text)

vars_alco %>% 
  select(id, text)

## -----------------------------------------------------------------------------
vars_acc$values[1] %>% 
  kable()

vars_alco$values[1] %>% 
  kable()

## -----------------------------------------------------------------------------
variable_codes <- vars_acc$id[c(1, 3, 6)] # UHELDA, KLOK and Tid
variable_values <- list(c(1000, 2000), NA, NA) # all values for KLOK and Tid

# Construct the variable_input as a list of code-values pairs
variable_input <- purrr::map2(.x = variable_codes, .y = variable_values, .f = ~list(code = .x, values = .y))

# Get data 
accidents <- get_data("uheld4", variables = variable_input)
head(accidents) %>% kable()

## -----------------------------------------------------------------------------
variable_codes <- vars_alco$id 
variable_values <- list(c("055", "09"), NA) # All values for Tid

# Construct the variable_input as a list of code-values pairs
variable_input <- purrr::map2(.x = variable_codes, .y = variable_values, .f = ~list(code = .x, values = .y))

# Get data 
alcohol <- get_data("alko3", variables = variable_input)
alcohol %>% 
  filter(INDHOLD != "..") %>% # the API returns ".." as missing values
  head() %>% 
  kable()

## ----out.width = '100%'-------------------------------------------------------
accidents_by_hour <- accidents %>%
	filter(KLOK != "Not stated") %>%
	group_by(UHELDA, KLOK) %>%
	summarise(mean_accidents = mean(INDHOLD, na.rm = TRUE))

accidents_by_hour %>%
	ggplot(aes(x = KLOK, y = mean_accidents, color = UHELDA, group = UHELDA)) +
	geom_line() +
	geom_point() +
	theme_bw() + 
  theme(legend.position="top") +
  labs(x = "Time of day", y = "Average annual accidents")

## -----------------------------------------------------------------------------
accidents_by_year <- accidents %>% 
  group_by(UHELDA, TID) %>% 
  summarize(INDHOLD = sum(INDHOLD)) %>% 
  ungroup()

accidents_by_year %>%
  group_by(UHELDA) %>% 
  summarise(min(TID), max(TID))

alcohol_by_year <- alcohol %>% 
  filter(INDHOLD != "..") %>% # the API returns ".." as missing values
  mutate(INDHOLD = as.numeric(INDHOLD))

alcohol_by_year %>%
  group_by(TYPE) %>% 
  summarise(min(TID), max(TID))

## ----out.width = '100%'-------------------------------------------------------

alcohol_data <- alcohol_by_year %>% 
  filter(between(TID, 1997, 2021),
         grepl("sales", TYPE, ignore.case = TRUE)) %>% 
  select(year = TID,
         alcohol_sales = INDHOLD)

accidents_data <- accidents_by_year %>% 
  filter(grepl("alcohol", UHELDA, ignore.case = TRUE)) %>% 
  select(year = TID,
         alcohol_accidents = INDHOLD)

accidents_2000 <- accidents_data %>% 
  filter(year == 2000) %>% 
  pull(alcohol_accidents)
  
accidents_data$alcohol_accidents <- round((accidents_data$alcohol_accidents/accidents_2000)*100)

data <- inner_join(alcohol_data, accidents_data, by = "year") %>% 
  mutate(risk_index = round(alcohol_accidents/alcohol_sales*100))

data %>% 
  ggplot(aes(x=year, y=risk_index)) +
  geom_col()

