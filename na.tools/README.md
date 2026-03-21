
## na.tools: Comprehensive Library for Working with Missing (NA) Values in Vectors

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/na.tools)](https://cran.r-project.org/package=na.tools)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-2.0.html) 
[![Downloads](https://cranlogs.r-pkg.org/badges/na.tools?color=brightgreen)](https://www.r-pkg.org/pkg/na.tools)
[![](https://cranlogs.r-pkg.org/badges/grand-total/na.tools)](https://CRAN.R-project.org/package=na.tools)

<!--
[![software impact](http://depsy.org/api/package/r/na.tools/badge.svg)](http://depsy.org/package/r/na.tools)
-->

**na.tools** is a comprehensive library for handling missing (NA) values.
It has several goals:

1. extend existing `stats::na.*()` functions, 
2. provide a collection for all functions for working with missing data together, and 
3. provide a consistent and intuitive interface. 

In this package, there are methods for the detection, removal, replacement, 
--imputation--, *recollection*, etc. of missing values (`NAs`). This libraries focus 
is on vectors (atomics). For **tidy**/**dplyr** compliant methods operating on
tables and lists, please use the 
[tidyimpute](https://github.com/decisionpatterns/tidyimpute) package which 
depends on this package.


## Installation

### Github 

    devtools::install_github( "decisionpatterns/na.tools")
    
### CRAN 

    install.packages("na.tools")


## Feature List
 
 * Over **70** functions for working with missing values (See [#Function List] below.) 
 * Standardizes and extends `na.*` functions found in the *stats* package.
 * Extensible S3 methods
 * Calculate statistics on missing values: `n_na`, `pct_na`
 * Remove missing values: `na.rm` 
 * Replacement/Imputation:
   * Type/class and length-safe replacement. (**na.tools** will never change the length or class of its argument.)
     produce an object with a different length/nrow or type/class of its target.)
   * Three types of imputations: 
     * constant
     * univariate commutative (order-independent)
     * univariate mom-commutative (order-dependent), e.g time series data
   * Replace using scalar, vector or function(s)
  * Easy mnemonics: 
      * functions beginning with `na.` return a transformed version of the 
        input vector with missing values imputes/
  
  
### Upcoming features

 * recall/track which values have been replaced and how.
   
   
## Examples

    x <- 1:3
    x[2] <- NA_real_
     
    any_na(x)
    all_na(x)
    which_na(x)
     
    n_na(x)
    pct_na(x)
     
    na.rm(x)
      
    na.replace(x, 2) 
    na.replace(x, mean)     # error
    na.replace(x, na.mean)  # Works
     
    na.zero(x)  
    na.mean(x)
     
    na.cumsum(x)

## Function List 

### Calculation 

 * `na.n` - Count mising values 
 * `na.pct` - Calculate pct of missing values

### Identification and Tests

 * `which.na` - Return logical or character indicating which elements are missing 
 * `all.na` (`na.all`)  - test if all elements are missing
 * `any.na` (`na.any`)  - test if any elements are missing
   
### Removal / Ommision / Exclusion 

 * `na.rm` - remove `NA`s  (with tables is equivalent to `drop_cols_all_na` )
 * `na.trim` - remove `NA`s from beginning or end (non-commutative/order matters)
 
 
### Replacement / Imputation ###

There are two types of imputation methods for plain vectors. They are 
distinguished by their replacement values. 

#### Imputation by constant

In "constant" imputation methods, missing values are replaced by an 
*a priori* selected constant value. No calculation are performed to derive 
replacement values and all missing value assume the same transformied value.

 * `na.zero` : Replace `NA`s with  0
 * `na.true` | `na.false` : ... `TRUE`
 * `na.inf` / `na.neginf` : ... `Inf` / `-Inf`
 * `na.constant` : constant value `.na`


#### Imputation by function 

In functional imputation, the value is calculated from the vector
containing the missing value(s) -- and only that vector. 
Missing values may impute to different values. Replacement values may (or may not) 
be affected by the ording of the vector. 


**Cummatative functions** 

Commutative functions provide the same result irregarless of the ordering of 
the input vectors. Therefore, these functions do not depend on the ordering
of elements of the input vector. 

(When imputing in a table, imputation by function is also called 
*column-based imputation* since replacement values derive from the single 
column. Table-based imputation is found in the **tidyimpute** package.)

 * `na.max` - maximum  
 * `na.min` - minumum 
 * `na.mean` - mean 
 * `na.median` - median value
 * `na.quantile` - quantile value
 * `na.sample`/`na.random` - randomly sampled value


** Non-commulative functions  **s
 
 * `na.cummax` - cumulative max
 * `na.cummin` - cumulative min
 * `na.cumsum` - cumulative sum
 * `na.cumprod` - cumulative prod
 

**General Imputation**

 * `na.replace`/`na.explicit` - atomic vectors only. General replacement function
 * `na.unreplace`/`na.implicit` - turn explicit values back into NAs


## See Also 

A number of other packages have methods for working with missing values and/or 
imputation. Here is a short, incomplete and growing list:

* `randomForest::na.roughfix()` - imputes with `median`
* `zoo::na.*` - collection of *non-commutative* imputation techniques for time series data.
* [CRAN Task View: Multivariate Statistics](https://CRAN.R-project.org/view=Multivariate):

> **mitools** provides tools for multiple imputation, mice provides multivariate imputation by chained equations **mvnmle** provides ML estimation for multivariate normal data with missing values, **mix** provides multiple imputation for mixed categorical and continuous data. **pan** provides multiple imputation for missing panel data. **VIM** provides methods for the visualisation as well as imputation of missing data. aregImpute() and transcan() from **Hmisc** provide further imputation methods. **monomvn** deals with estimation models where the missing data pattern is monotone.




