[![CRAN Version](https://www.r-pkg.org/badges/version/dfmeta)](https://cran.r-project.org/package=dfmeta)
![](https://cranlogs.r-pkg.org/badges/grand-total/dfmeta)
  
# dfmeta

### Description

The **dfmeta** package includes meta-analysis approaches for Phase I dose finding early phases clinical trials in order to better suit requirements in terms of the maximum tolerated dose (MTD) and the maximal dose regimem (MDR).

#### Installation 

### Establish Version  

 The 1st version of the package **dfmeta** will be available on CRAN very soon and it will be loaded via 

```{r} 
install.packages("dfmeta")
library(dfmeta) 
```  

### Development Version 
To install the **dfmeta** package from GitHub, first make sure that you can install the necessary depended packages such as **stats4**, **lme4**, **plyr** etc. Once the depended packages are successfully installed, you can install **dfmeta** from GitHub using the **devtools** package by executing the following in R:

```{r}
if (!require(devtools)){
  install.packages("devtools") 
  library(devtools) 
}

install_github("artemis-toumazi/dfmeta")
```

If installation fails, please let us know by [filing an issue](https://github.com/artemis-toumazi/dfmeta/issues). 

Details on formula syntax, families and link functions, as well as prior distributions can be found on the help page of the dfmeta function:
```{r help.dfmeta, eval=FALSE}
help(dfmeta) 
```

#### FAQ

### What is the best way to ask a question or propose a new feature? 

You can either open an issue on [github](https://github.com/artemis-toumazi/dfmeta) or write me an email to (artemis.toumazi@gmail.com). 

