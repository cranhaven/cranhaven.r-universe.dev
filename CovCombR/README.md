# CovCombR
Combine partial covariance or relationship matrices from independent experiments.

Combine partial covariance matrices using a Wishart-EM algorithm. 
Methods are described in the November 2019 article by Akdemir et al. (<https://www.biorxiv.org/content/10.1101/857425v1>).
It can be used to combine partially overlapping covariance matrices from independent trials, partially overlapping multi-view relationship data from genomic experiments, partially overlapping Gaussian graphs described by their covariance structures. This is useful for high dimensional covariance estimation, multi-view data integration, high dimensional covariance graph estimation.


## Installation
```
devtools::install_github("denizakdemir/CovCombR")
```


## Basic Usage

```
library(CovCombR)



data("mtcars")
my_data <- mtcars[, c(1,3,4,5)]
dim(my_data)
# print the first 6 rows
head(my_data, 6)
#Artificially making 3 partial covariance matrices! 
#These are the partial covariances obtained from 
#independent  multi-view experiments.
set.seed(123)
cov12<-cov(my_data[sample(nrow(my_data),20),1:2])
cov23<-cov(my_data[sample(nrow(my_data),20),2:3])
cov34<-cov(my_data[sample(nrow(my_data),20),3:4])

# Combine covariances using the package
Combined<-CovComb(Klist=list(cov12,cov23,cov34))

# Actual covariance using all data
Actual<-cov(my_data)

#Compare results
round(Combined,2)
#Result
#         mpg     disp      hp   drat
#mpg    42.97  -656.51 -183.02   1.09
#disp -656.51 13724.62 5175.68 -31.00
#hp   -183.02  5175.68 3855.31 -23.41
#drat    1.09   -31.00  -23.41   0.28
round(Actual,2)
#Result
#         mpg     disp      hp   drat
#mpg    36.32  -633.10 -320.73   2.20
#disp -633.10 15360.80 6721.16 -47.06
#hp   -320.73  6721.16 4700.87 -16.45
#drat    2.20   -47.06  -16.45   0.29
```
## Another example

```

cov12<-cov(my_data[sample(nrow(my_data),20),c(1,2)])
cov13<-cov(my_data[sample(nrow(my_data),20),c(1,3)])
cov14<-cov(my_data[sample(nrow(my_data),20),c(1,4)])


Combined<-CovComb(Klist=list(cov12,cov13,cov14))
Actual<-cov(my_data)
round(Combined,2)
#         mpg     disp      hp   drat
#mpg    41.11  -770.23 -347.78   2.66
#disp -770.23 18253.89 5140.44 -48.71
#hp   -347.78  5140.44 5233.73 -21.01
#drat    2.66   -48.71  -21.01   0.30
round(Actual,2)
#         mpg     disp      hp   drat
#mpg    36.32  -633.10 -320.73   2.20
#disp -633.10 15360.80 6721.16 -47.06
#hp   -320.73  6721.16 4700.87 -16.45
#drat    2.20   -47.06  -16.45   0.29
```



## Vignette


```
vignette("CovCombR")
```
