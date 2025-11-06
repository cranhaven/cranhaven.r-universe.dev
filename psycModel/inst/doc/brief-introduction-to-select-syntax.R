## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(message=FALSE,warning = FALSE, comment = NA)
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)

## ----setup--------------------------------------------------------------------
library(psycModel)
library(dplyr)

## -----------------------------------------------------------------------------
iris %>% head() # head() show the first 5 rows of the data frame 

## -----------------------------------------------------------------------------
iris %>% select(1:3) %>% head(1)
iris %>% select(Sepal.Length:Petal.Length) %>% head(1)

## -----------------------------------------------------------------------------
iris %>% select(c(1, 3:4)) %>% head(1)
iris %>% select(Sepal.Length, Petal.Length:Petal.Width) %>% head(1)

## -----------------------------------------------------------------------------
iris %>% select(1:5, -3) %>% head(1)
iris %>% select(Sepal.Length:Species, -Petal.Length) %>% head(1)

## -----------------------------------------------------------------------------
# select all columns
iris %>% select(everything()) %>% head(1)
# select everything except Sepal.Width
iris %>% select(c(everything(),-Sepal.Width)) %>% head(1)

## -----------------------------------------------------------------------------
iris %>% select(starts_with('Sepal')) %>% head(1)

## -----------------------------------------------------------------------------
iris %>% select(ends_with('Width')) %>% head(1)

## -----------------------------------------------------------------------------
iris %>% select(contains('Sepal')) %>% head(1) # same as starts_with
iris %>% select(contains('Width')) %>% head(1) # same as ends_with
iris %>% select(contains('.')) %>% head(1) # contains "." will be selected

## -----------------------------------------------------------------------------
iris %>% select(where(is.numeric)) %>% head(1) 

## -----------------------------------------------------------------------------
set.seed(1)
test_data = data.frame(y = rnorm(n = 100,mean = 2,sd = 3), 
           x1 = rnorm(n = 100,mean = 1.5, sd = 4),
           x2 = rnorm(n = 100,mean = 1.7, sd = 4), 
           x3 = rnorm(n = 100,mean = 1.5, sd = 4),
           x4 = rnorm(n = 100,mean = 2, sd = 4),
           x5 = rnorm(n = 100,mean = 1.5, sd = 4))

## -----------------------------------------------------------------------------
# Without this package: 
model1 = lm(data = test_data, formula = y ~ x1 + x2 + x3 + x4 + x5)

# With this package: 
model2 = lm_model(data = test_data,
         response_variable = y, 
         predictor_variable = c(everything(),-y))

## -----------------------------------------------------------------------------
model3 = lm_model(data = test_data,
         response_variable = y, 
         predictor_variable = everything())

