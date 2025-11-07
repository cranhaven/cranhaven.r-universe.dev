## ---- include = FALSE, warning=FALSE, message=FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(qacBase)

## ----include=TRUE-------------------------------------------------------------
# simple summary statistics 
qstats(cardata, highway_mpg)

# summary statistics by vehicle_size
qstats(cardata, highway_mpg, vehicle_size)

# summary statistics by vehicle_size and drive type
qstats(cardata, highway_mpg, vehicle_size, driven_wheels)

## ----include=TRUE-------------------------------------------------------------
# single statistic
qstats(cardata, highway_mpg, vehicle_size, stats = "median")

# multiple statistics
qstats(cardata, highway_mpg, vehicle_size, 
       stats = c("median", "min", "max"))

## ----include=TRUE-------------------------------------------------------------
#custom statistics
p25 <- function(x) quantile(x, probs=.25)
p75 <- function(x) quantile(x, probs=.75)

#calling the built in and custom statistics
qstats(cardata, highway_mpg, vehicle_size, 
       stats = c("min", "p25", "p75", "max"))

## ----include=TRUE-------------------------------------------------------------
qstats(cardata, highway_mpg, vehicle_size,  
       stats=c("n", "mean","median","sd"),  
       na.rm=FALSE, digits=2)

