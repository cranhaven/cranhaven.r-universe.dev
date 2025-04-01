# Clusterability R Package
The **clusterability** package tests for cluster tendancy of a dataset. Results of these tests can inform whether clustering algorithms are appropriate for the data.

## Installation
You can install the released version of **clusterability** from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("clusterability")
```
If you would prefer to use a newer version of **clusterability** not yet available on CRAN, it can be downloaded as a binary package from this repository and installed locally. [Documentation](https://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages) on this process can be found on the R project website.

## Example

This demonstrates the use of the `clusterabilitytest` function to determine if the four numeric variables of the [*iris*](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html) dataset have a natural cluster tendency.

#### Input
``` r
library(clusterability)
data(iris)
iris_numeric <- iris[,c(1:4)]
iris_result <- clusterabilitytest(iris_numeric, "dip")
print(iris_result)
```

#### Output
```
----------------------
Clusterability Test
----------------------

Data set name: iris_numeric
Your data set has 150 observation(s) and 4 variable(s).
There were no missing values. Your data set is complete.

Data Reduced Using: PCA

-----------------------------------------
Results: Dip Test of Unimodality
-----------------------------------------

Null Hypothesis: number of modes = 1
Alternative Hypothesis: number of modes > 1
p-value: 0 
Dip statistic: 0.107841006841301 

---------------------
Test Options Used
---------------------

Default values for the optional parameters were used. To learn more about customizing the behavior of the clusterabilitytest, please see the R documentation.

```

## Required Parameters
The **data** and **test** parameters are required when calling the `clusterabilitytest()` function.

##### data
The dataset to be used in the test. Internally, the `as.matrix` R function is used to coerce the **data** argument, so the **data** argument should be a dataframe, matrix, or other object that can be coerced to a matrix. The dataset should consist only of numeric values.

##### test
The test to be performed. Valid values are `"dip"`, which will perform the Dip Test of Unimodality, or `"silverman"`, which will perform Silverman's Critical Bandwidth test.

## Additional Parameters
The following parameters are optional and can be used to further customize the behavior of the `clusterabilitytest()` function.

##### reduction
The dimension reduction technique to be used to reduce the **data** to a unidimensional dataset. 
-    Principal Component Analysis can be used by specifying the value `"pca"`. This is the default behavior. 
-    Pairwise Distances can be used by specifying the value `"distance"`. 
-    If the **data** argument is a one-dimensional data set, the `"none"` option can be used.

##### distance_metric
If using pairwise distances as the dimension reduction technique, this is the metric to be used in computing the distances. The default is `"euclidean"`. See the documentation for the `clusterabilitytest()` function for a list of the available metrics.
##### distance_standardize
If using pairwise distances for dimension reduction, this is how the variables should be standardized before computing the distances. The default is `"std"`, which standardizes each variable to have mean 0 and standard deviation 1. See the documentation for a list of the available standardization methods.
##### pca_center
If using PCA as the dimension reduction technique, this is a logical determines if the variables are shifted to be zero centered. The default is `TRUE`. 
##### pca_scale
If using PCA for dimension reduction, this is a logical value that determines if the variables are scaled to have unit variance. The default is `TRUE`.
##### is_dist_matrix
This is a logical value indicating if the **data** argument is a distance matrix. This is `FALSE` by default. If it is `TRUE`, then the lower triangular portion of **data** will be extracted and used.
##### completecase
This is a logical value indicating if a complete case analysis should be performed. This is `FALSE` by default. Missing data must be removed before a test can be performed, which can be done either manually by the user or by specifying `TRUE` for the **completecase** argument. 
## Additional Parameters and Details
Parameters to customize the Dip Test are prefixed with *d_* and the Silverman Test with *s_*. Documentation for these parameters, along with additional details for the parameters described above, is provided in the documentation for `clusterabilitytest()`, which can be found by executing the following command:

``` r
?clusterabilitytest
```

Documentation is also available in the accompanying paper.

## Supplemental Files
##### clusterability_timings.R
This contains code to test the relative computational performance of each test and dimension reduction combination.
##### examples.R
This contains code to replicate the examples in the accompanying paper.
##### Rplots.R
This contains code to replicate the plots provided in the accompanying paper.