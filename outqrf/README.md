# {outqrf}
## Overview
**outqrf** is an R package used for outlier detection. Each numeric variable is regressed onto all other variables using a quantile random forest (QRF).
We use [ranger](https://github.com/imbs-hl/ranger) to perform the fitting and prediction of quantile regression forests (QRF).
Next, we will compute the rank of the observed values in the predicted results' quantiles. If the rank of the observed value exceeds the threshold, 
the observed value is considered an outlier.

Since the same predicted value might be distributed across multiple quantiles in the predicted quantile results, 
this affects our location finding for the observed value. Therefore, we also used a method similar to the [outForest](https://github.com/mayer79/outForest) package to compare the observed value 
with the 50% quantile value again to determine the final quantile result.

## Installation
```r
# Development version
devtools::install_github("flystar233/outqrf")
```

## Usage
We first generate a data set with about 5% outliers values in each numeric column.
```
#Generate data with outliers in numeric columns
irisWithOutliers <- generateOutliers(iris, p = 0.05,seed =2024)
# Find outliers by quantile random forest regressions
out <- outqrf(irisWithOutliers,quantiles_type=400)
out$outliers
#    row          col    observed predicted  rank
# 1   32 Sepal.Length  14.9308229       5.4 0.999
# 2   35 Sepal.Length  -1.8135664       4.6 0.001
# 3   84 Sepal.Length  11.4849203       6.3 0.999
# 4  129 Sepal.Length  -5.6021049       6.2 0.001
# 5   49  Sepal.Width  10.7927619       3.7 0.999
# 6   90  Sepal.Width  -0.7648333       2.4 0.001
# 7  131  Sepal.Width  -2.1389311       2.7 0.001
# 8  137  Sepal.Width  11.4992802       3.2 0.999
# 9   36 Petal.Length  12.8033669       1.6 0.999
# 10  73 Petal.Length -17.1905846       4.4 0.001
# 11 107 Petal.Length  13.6672827       5.6 0.999
# 12 123 Petal.Length  -8.9717894       5.1 0.001
# 13 140 Petal.Length  13.5214560       5.7 0.999
# 14  10  Petal.Width -11.8406790       0.2 0.001
# 15  14  Petal.Width  -6.3030372       0.2 0.003
# 16  34  Petal.Width   7.5843853       0.4 0.999
# 17  66  Petal.Width   6.9828746       2.0 0.993
# 18 113  Petal.Width  -6.0696862       1.5 0.001

```

## Evaluation on iris (Small Dataset)
First, let's simply detect outliers in the data using a box plot.
```
# find outliers use boxplot
# 32
irisWithOutliers <- outqrf::generateOutliers(iris, p = 0.05,seed =2024)
boxplot_num = 0
for (i in names(irisWithOutliers)[sapply(irisWithOutliers,is.numeric)]){
  q1 <- quantile(irisWithOutliers[,i], 0.25)
  q3 <- quantile(irisWithOutliers[,i], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  num <- sum(irisWithOutliers[,i]<lower_bound|irisWithOutliers[,i]>upper_bound)
  boxplot_num<-boxplot_num+num
}
boxplot_num
# 28
```
![](https://github.com/user-attachments/assets/0a453eb9-3901-4c46-a4f4-ee86c386a701)

Then, use outqtf and outForest respectively to detect outliers.
```
qrf <- outqrf(irisWithOutliers,quantiles_type=400)
rf <- outForest(irisWithOutliers)

evaluateOutliers(iris,irisWithOutliers,qrf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency 
# 32.00      17.00      17.00       0.53       1.00 
evaluateOutliers(iris,irisWithOutliers,rf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency
# 32.00      19.00      19.00       0.59       1.00 
```
We can even display the original values and outliers of the data using a paired box plot.
```
plot(qrf)
```
![](https://github.com/user-attachments/assets/073f4e4d-3c80-459a-af50-40988d769899)

## Evaluation on diamonds (Big Dataset)
```
data <- diamonds|>select(price,carat,cut,color,clarity)
data2 <- outqrf::generateOutliers(data, p = 0.001,seed =2024)
qrf <- outqrf(data2,num.threads=8,quantiles_type=400)
# The process can be slow because it needs to predict the value at 400|1000 quantiles for each observation. 
rf <- outForest(data2)
evaluateOutliers(data,data2,qrf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency 
#108.00     369.00     103.00       0.95       0.28 
evaluateOutliers(data,data2,rf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency 
#108.00     687.00     104.00       0.96       0.15
```

