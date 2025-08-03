# hmeasure

## Measuring Classification Performance: the hmeasure package for R
The `hmeasure` package implements a large number of classification performance metrics, such as the AUC, Error Rate, sensitivity and specificity, and additionally extends standard libraries by incorporating recent advances in this area, notably including the H-measure which was proposed by David Hand as coherent alternative to the AUC, and further developed by Hand and Anagnostopoulos. For more info, please visit [hmeasure.net](hmeasure.net).

## Why use it? 
This package aspires to become a one-stop-shop for classification performance metrics, and to support implementations in all popular data science languages. At the moment it is already covering more metrics than any other package in R, and is the only one that covers the H-measure, Specificity at fixed Sensitivity and Minimum Error Rate.

## How do I use it? 

The `hmeasure` packages relies on the scoring interface that most classifiers support, whereby each example is assigned a score, so that higher values indicate that the example is more likely to be labelled positive. For many classifiers, this scores is intended to be a probability and lies between 0 and 1, but that is not a requirement for the package. The main function, `HMeasure`, takes as input the true labels, and a data frame where each column contains the scores of a classifier applied on a certain dataset. You may run the following example where we directly specify the scores to be a noisy version of the label, with more noise for classifier A than classifier B (this is known as a the 'binormal' case in the literature):

```
library(hmeasure)
n = 50
set.seed(1)
y = c(rep(1, n), rep(0, n))
scores = data.frame(
  A=c(rnorm(n,0,1), rnorm(n,0.5,1)),
  B=c(rnorm(n,0,1), rnorm(n,2,1))
)
out = HMeasure(true.class = y, scores = scores)
summary(out)
```
The package also supports a plotting routine that visualizes the ROC curve, as well as some additional interesting plots that concern specifically the H-measure:

```{R}
plotROC(out)
```

## Where can I get it? 

The package is available on CRAN and can be downloaded and installed as per usual:

```{R}
install.packages('hmeasure')
library(hmeasure)
```

For the most recent version, you can install the github version instead: 

```{R}
devtools::install_github(canagnos/hmeasure)
```

