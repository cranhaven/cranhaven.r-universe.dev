# lactcurves

<!-- badges: start -->
<!-- badges: end -->

The goal of lactcurves is to provide parameter estimates and selection criteria for
lactation curve models, cubic splines, and legendre polynomials. Start parameters
for lactation curve models were optimized using milk yield test-day data
across the first three lactations of ~1.7 million Holstein Friesian cows. Other data
might require adjusting of the start parameters, but the lactcurve package gives
a comprehensive source of models over the last 100 years.

## Installation

You can install the released version of lactcurves from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lactcurves")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lactcurves)
## basic example code
```
## create data set for 3 individuals with milk yield records on 24 days

ID=c(rep("ID123",24),rep("ID456",24),rep("ID789",24))

dim=as.integer(rep(seq(from=5, to=340, by=14),3))

mkg=as.numeric(c(23.4,28.3,30.5,31.3,31.5,31.3,30.9,30.5,30.1,29.6,29.1,28.7,28.2,27.7,27.2,26.7,
26.2,25.7,25.2,24.7,24.2,23.7,23.2,22.8,
21.3,25.7,26.9,27.2,26.9,26.5,26.1,25.6,25.1,24.6,24.1,23.6,23.1,22.6,22.1,21.6,21.1,20.6,20.1,
19.6,19.1,18.6,18.1,17.6,
22.0,26.5,28.1,28.4,28.2,27.9,27.4,26.9,26.4,25.9,25.4,24.9,24.4,23.9,23.4,22.9,22.4,21.9,21.4,
20.9,20.4,19.9,19.4,18.9))

data=cbind.data.frame(ID,dim,mkg)

## run example

output=AllCurves(data,mkg,dim)

output$critall
output$modeldescrip
output$critbest
output$bestmodel
output$Error
output$ModelParam
output$summary17b

## plot curve
# set the number of days to consider
dim=c(1:340)

# look up the model and its estimated parameters
output$summary17b

# use model and parameters to plot curve
plot(19.293701+(31.358471-19.293701)*(1-exp(1)^(-0.059874*dim))-0.035495*dim)
