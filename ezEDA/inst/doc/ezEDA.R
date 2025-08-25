## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("ezEDA")

## ----setup--------------------------------------------------------------------
library(ezEDA)

## ----measure_change_over_time_wide--------------------------------------------
## For ggplot2::economics, plot the trend of population and number unemployed. 
## In this dataset, the different measures are in different columns
measure_change_over_time_wide(ggplot2::economics, date, pop, unemploy)

## ----measure_change_over_time_long--------------------------------------------
## For ggplot2::economics_long, plot the trend of population and number unemployed.
## In this dataset, all measures are in the column named value and  
## the names of the measures are in the column named variable
measure_change_over_time_long(ggplot2::economics_long, date, variable, value, pop, unemploy)

## ----measure_distribution_1---------------------------------------------------
## For ggplot2::mpg, plot the distribution of highway mileage
measure_distribution(ggplot2::mpg, hwy)

## ----measure_distribution_2---------------------------------------------------
## For ggplot2::mpg, plot the distribution of highway mileage 
measure_distribution(ggplot2::mpg, hwy, bwidth = 2)

## ----measure_distribution_3---------------------------------------------------
## For ggplot2::mpg, plot the distribution of highway mileage as a boxplot
measure_distribution(ggplot2::mpg, hwy, type = "box")

## ----measure_distribution_by_category_1---------------------------------------
## For ggplot2::diamonds, plot the distribution of price while highlighting 
## the counts of diamonds of different cuts 
measure_distribution_by_category(ggplot2::diamonds, price, cut)

## ----measure_distribution_by_category_2---------------------------------------
## For ggplot2::diamonds, plot the distribution of price showing 
## the distribution for each kind of cut in a different facet
measure_distribution_by_category(ggplot2::diamonds, price, cut, separate = TRUE)

## ----measure_distribution_by_two_categories-----------------------------------
## For ggplot2::diamonds, plot the distribution of price separately 
## for each unique combination of cut and clarity
measure_distribution_by_two_categories(ggplot2::diamonds, carat, cut, clarity)

## ----measure_distribution_over_time-------------------------------------------
## 50 random values of three measures for each of 1999, 2000 and 2001
h1 <- round(rnorm(50, 60, 8), 0)
h2 <- round(rnorm(50, 65, 8), 0)
h3 <- round(rnorm(50, 70, 8), 0)
h <- c(h1, h2, h3)
y <- c(rep(1999, 50), rep(2000, 50), rep(2001, 50))
df <- data.frame(height = h, year = y)
measure_distribution_over_time(df, h, year)

## ----two_measures_relationship_1----------------------------------------------
## For ggplot2::mpg, plot the highway mileage against the displacement 
two_measures_relationship(ggplot2::mpg, displ, hwy)

## ----two_measures_relationship_2----------------------------------------------
## For ggplot2::diamonds, plot the price against carat   
## while showing the relationship separately for diamionds of
## each kind of cut
two_measures_relationship(ggplot2::diamonds, carat, price, cut)

## ----multi_measures_relationship----------------------------------------------
## For ggplot2::mpg, plot the relationship between city mileage, 
## highway mileage and displacement
multi_measures_relationship(ggplot2::mpg, cty, hwy, displ)

## ----category_tally-----------------------------------------------------------
## For ggplot2::mpg, plot the tallies for the different vehicle classes
category_tally(ggplot2::mpg, class)

## ----two_category_tally_1-----------------------------------------------------
## For ggplot2::diamonds plot the tallies for different types of 
## cut and clarity
two_category_tally(ggplot2::diamonds, cut, clarity)

## ----two_category_tally_2-----------------------------------------------------
## For ggplot2::diamonds, plot the tallies for different types of 
## cut and clarity and show the plots for each value of the 
## second category in a separate facet
two_category_tally(ggplot2::diamonds, cut, clarity, separate = TRUE)

## ----category_contribution----------------------------------------------------
## For ggplot2::diamonds, plot the total price for each kind of cut
category_contribution(ggplot2::diamonds, cut, price)

## ----two_category_contribution_1----------------------------------------------
## For ggplot2::diamonds, plot the total price for each kind of cut  
## while also showing the contribution of each kind of clarity 
## within each kind of cut
two_category_contribution(ggplot2::diamonds, cut, clarity, price)

## ----two_category_contribution_2----------------------------------------------
## For ggplot2::diamonds, same as above, but show each color 
## on a different facet
two_category_contribution(ggplot2::diamonds, cut, clarity, price, separate = TRUE)

