
# revengc: An R package to reverse engineer decoupled and censored data

Decoupled (e.g. separate averages) and censored (e.g. > 100 species) variables are continually reported by many well-established organizations (e.g. World Health Organization (WHO), Centers for Disease Control and Prevention (CDC), World Bank, and various national censuses).  The challenge therefore is to infer what the original data could have been given summarized information.  We present an R package that reverse engineers decoupled and/or censored data with two main functions.  The cnbinom.pars function estimates the average and dispersion parameter of a censored univariate frequency table.  The rec function reverse engineers summarized data into an uncensored bivariate table of probabilities.

**It is highly recommended for a user to read the vignettes for more information about the methodology of both functions.**     
        
## Getting Started 
 
You can install the latest development version from github with

```
devtools::install_github("GIST-ORNL/revengc")
library(revengc)
```

or the the latest release version from CRAN with
```
install.packages("revengc")
library(revengc)
```

## Usage
`cnbinom.pars()` has the following format 
```
cnbinom.pars(censoredtable)
``` 

where a description of the argument directly below 

* **censoredtable** - A frequency table (censored and/or uncensored).  A data.frame and matrix are acceptable classes.  See Table format section below.


`rec()` has the following format where a description of each argument is found below 
 
```
rec(X, Y, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, 
    seed.matrix, seed.estimation.method)
``` 

* **X** - Argument can be an average, a univariate frequency table, or a censored contingency table.  The average value should be a numeric class while a data.frame or matrix are acceptable table classes.  Y defaults to NULL if X argument is a censored contingency table.  See Table format section below.

* **Y** - Same description as X but this argument is for the Y variable.  X defaults to NULL if Y argument is a censored contingency table.

* **Xlowerbound** - A numeric class value to represent the left bound for X (row in contingency table).  The value must strictly be a non-negative integer and cannot be greater than the lowest category/average value provided for X (e.g. the lower bound cannot be 6 if a table has '< 5' as a X or row category). 

* **Xupperbound** - A numeric class value to represent the right bound for X (row in contingency table).  The value must strictly be a non-negative integer and cannot be less than the highest category/average value provided for X (e.g. the upper bound cannot be 90 if a table has '> 100' as a X or row category).

* **Ylowerbound** - Same description as Xlowerbound but this argument is for Y (column in contingency table). 

* **Yupperbound** - Same description as Xupperbound but this argument is for Y (column in contingency table). 

* **seed.matrix** - An initial probability matrix to be updated.  If decoupled variables is provided the default is a Xlowerbound:Xupperbound by Ylowerbound:Yupperbound matrix with interior cells of 1, which are then converted to probabilities.  If a censored contingency table is provided the default is the seedmatrix()\$Probabilities output.

* **seed.estimation.method** - A character string indicating which method is used for updating the seed.matrix. The choices are: "ipfp", "ml", "chi2", or "lsq". Default is "ipfp".  


## Table format
The univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with the frequencies or probabilities in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G).  Also, calculations use closed intervals.    


* left censoring: <, L, <=, LE
* interval censoring: - or I (symbol has to be placed in the middle of the two category values)
* right censoring: >, >=, +, G, GE
* uncensored: no symbol (only provide category value)

The formatted example below is made with the following code.

```
univariatetable<-cbind(as.character(c("<=6", "7-12", "13-19", "20+")), c(11800,57100,14800,3900))
```

<=6  | 11800
---- | ----
7-12 | 57100
13-19| 14800
 20+ |3900


The contingency table has restrictions.  The censored symbols should follow the requirements listed above.  The table's class can be a data.frame or a matrix.  The column names should be the Y category values. Row names should never be placed in this table, the default should always be 1:n.  The first column should be the X category values. The inside of the table are X * Y cross tabulation, which are either nonnegative frequencies or probabilities if seed.estimation.method is "ipfp" or strictly positive when method is "ml", "lsq" or "chi2".  The row and column marginal totals corresponding to their X and Y category values need to be placed in this table. The top left, top right, and bottom left corners of the table should be NA or blank.  The bottom right corner can be a total cross tabulation sum value, NA, or blank. The formatted example below is made with the following code.

```
contingencytable<-matrix(c(18, 13, 7, 19, 8, 5, 8, 12, 10), nrow = 3, ncol = 3)
  rowmarginal<-apply(contingencytable,1,sum)
  contingencytable<-cbind(contingencytable, rowmarginal)
  colmarginal<-apply(contingencytable,2,sum)
  contingencytable<-rbind(contingencytable, colmarginal)
  row.names(contingencytable)[row.names(contingencytable)=="colmarginal"]<-""
  contingencytable<-data.frame(c("<5", "5I9", "G9", NA), contingencytable)
  colnames(contingencytable)<-c(NA,"<=19","20-30",">=31", NA)
```

  NA | <=19 | 20-30 | >=31 | NA 
 -----|------|-------|------|-----
  <5 | 18 | 19 | 8 | 45
  5I9 | 13 | 8 | 12 | 33
  G9 | 7 | 5 | 10 | 22
  NA | 38 | 32 | 30 | 100 



## Examples of Applying functions to Census Data

### Nepal
A Nepal Living Standards Survey [1] provides a censored table and average for urban household size. Using the censored table, the cnbinom.pars function calculates a close approximation to the provided average household size (4.4 people).

```
# revengc has the Nepal houshold table preloaded as univariatetable.csv   
cnbinom.pars(censoredtable = univariatetable.csv)
```

### Indonesia
In 2010, the Population Census Data - Statistics Indonesia provided over 60 censored contingency tables containing Floor Area of Dwelling Unit (square meter) by Household Member Size. The tables are separated by province, urban, and rural.  Here we use the household size by area contingency table for Indonesia's rural Aceh Province to show the multiple coding steps and functions implemented inside rec.  This allows the user to see a methodology workflow in code form.  The final uncensored household size by area estimated probability table, which implemented the "ipfp" method and default seed matrix, has rows ranging from 1 (Xlowerbound) to 15 (Xupperbound) people and columns ranging from 10 (Ylowerbound) to 310 (Yupperbound) square meters. 



```
# data = Indonesia 's rural Aceh Province censored contingency table
# preloaded as 'contingencytable.csv'
contingencytable.csv 

# provided upper and lower bound values for table
# X=row and Y=column
Xlowerbound=1
Xupperbound=15
Ylowerbound=10
Yupperbound=310

# table of row marginals provides average and dispersion for x
row.marginal.table<-row.marginal(contingencytable.csv)
x<-cnbinom.pars(row.marginal.table)
# table of column marginals provides average and dispersion for y
column.marginal.table<-column.marginal(contingencytable.csv)
y<-cnbinom.pars(column.marginal.table)

# create uncensored row and column ranges   
rowrange<-Xlowerbound:Xupperbound
colrange<-Ylowerbound:Yupperbound

# new uncensored row marginal table = truncated negative binomial distribution
uncensored.row.margin<-dtrunc(rowrange, mu=x$Average, size = x$Dispersion, 
  a = Xlowerbound-1, b = Xupperbound, spec = "nbinom")
# new uncensored column margin table = truncated negative binomial distribution
uncensored.column.margin<-dtrunc(colrange, mu=y$Average, size = y$Dispersion,
  a = Ylowerbound-1, b = Yupperbound, spec = "nbinom")

# sum of truncated distributions equal 1
# margins need to be equal for mipfp 
sum(uncensored.row.margin)
sum(uncensored.column.margin)

# create seed of probabilities (rec default)
seed.output<-seedmatrix(contingencytable.csv, Xlowerbound, 
  Xupperbound, Ylowerbound, Yupperbound)$Probabilities

# run mipfp
# store the new margins in a list
tgt.data<-list(uncensored.row.margin, uncensored.column.margin)
# list of dimensions of each marginal constrain
tgt.list<-list(1,2)
# calling the estimated function
## seed has to be in array format for mipfp package
## ipfp is the selected seed.estimation.method
final1<-Estimate(array(seed.output,dim=c(length(Xlowerbound:Xupperbound), 
  length(Ylowerbound:Yupperbound))), tgt.list, tgt.data, method="ipfp")$x.hat

# filling in names of updated seed  
final1<-data.frame(final1)
row.names(final1)<-Xlowerbound:Xupperbound
names(final1)<-Ylowerbound:Yupperbound

# reweight estimates to known censored interior cells 
final1<-reweight.contingencytable(observed.table = contingencytable.csv, 
  estimated.table = final1)

# final result is probabilities
sum(final1)

# rec function outputs the same table
# default of rec seed.estimation.method is ipfp
# default of rec seed.matrix is the output of the seedmatrix() function
final2<-rec(X= contingencytable.csv,
  Xlowerbound = 1,
  Xupperbound = 15,
  Ylowerbound = 10,
  Yupperbound = 310)

# check that both data.frame results have same values
all(final1 == final2$Probabilities)
```


## Legal
[1] National Planning Commissions Secretariat, Government of Nepal. (2011). *Nepal Living Standards Survey*. Retrieved from: <http://siteresources.worldbank.org/INTLSMS/Resources/3358986-1181743055198/3877319-1329489437402/Statistical_Report_Vol1.pdf>

[2] Population Census Data - Statistics Indonesia. (2010). *Household by Floor Area of Dwelling Unit and Households Member Size*. Retrieved from: <http://sp2010.bps.go.id/index.php/site/tabel?wid=1100000000&tid=334&fi1=586&fi2=>

