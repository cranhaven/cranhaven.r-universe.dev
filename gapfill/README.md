The R package gapfill 
=====================

The package provides tools to fill missing values in 
satellite data. It can be used to gap-fill, e.g., MODIS NDVI 
data, and is helpful for the development of new gap-fill 
algorithms. The predictions are based on a subset-predict 
procedure, i.e., each missing value is predicted separately by
(1) subsetting the data to a neighborhood around it and 
(2) predict the values based on that subset.

### Features of the package
  * Gap-filling can be executed in parallel.
  * Users may define `Subset` and `Predict` functions and 
    run alternative prediction algorithms with little effort. 
    See `?Extend` for more information and examples.
  * The visualization of space-time data is simplified 
    through the ggplot2 based function Image.



### Get started 

The package can be installed with 

    R> install.packages("gapfill")

To get started see the example in 

    R> ?Gapfill


