 [![CRAN Version](https://www.r-pkg.org/badges/version/dfped)](https://cran.r-project.org/package=dfped)
 ![](https://cranlogs.r-pkg.org/badges/grand-total/dfped)

# dfped

The **dfped** R package provides an unified method for designing and analysing dose-finding trials in paediatrics, while bridging information from adults. 

### Description

dfped package includes three extrapolation methods in order to calculate the dose range: linear, allometry and maturation adjustment, using pharmacokinetic (PK) data. To do this, it is assumed that target exposures are the same in both populations. The working model and prior distribution parameters of the dose-toxicity and dose-efficacy relationships can be obtained using early phase adult toxicity and efficacy data at several dose levels through dfped package. Priors are used into the dose finding process through a Bayesian model selection or adaptive priors, to facilitate adjusting the amount of prior information to differences between adults and children. This calibrates the model to adjust for misspecification if the adult and paediatric data are very different. User can use his/her own Bayesian model written in Stan code through the dfped package. A template of this model is proposed in the examples of the corresponding R functions in the package. Finally, in this package you can find a simulation function for one trial or for more than one trial. 

#### Installation

### Establish Version

A latest version of the package **dfped** is available on CRAN and can be loaded via

```{r}
install.packages("dfped")
library(dfped)
```  

### Development Version 
To install the **dfped** package from GitHub, first make sure that you can install the **rstan** package and C++ toolchain by following these [instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). The program Rtools (available on https://cran.r-project.org/bin/windows/Rtools/) comes with a C++ compiler for Windows. On OS-X, you should install Xcode. Once **rstan** is successfully installed, you can install **dfped** from GitHub using the **devtools** package by executing the following in R:

```{r}
if (!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

install_github("artemis-toumazi/dfped")
```

If installation fails, please let us know by [filing an issue](https://github.com/artemis-toumazi/dfped/issues).

Details on formula syntax, families and link functions, as well as prior distributions can be found on the help page of the dfped function:
```{r help.dfped, eval=FALSE}
help(dfped)
```

#### FAQ

### Can I avoid compiling Stan models?

Unfortunately, fitting your Stan model with **dfped**, there is currently no way to avoid the compilation. 

### What is the best way to ask a question or propose a new feature? 

You can either open an issue on [github](https://github.com/artemis-toumazi/dfped) or write me an email to (artemis.toumazi@gmail.com).
