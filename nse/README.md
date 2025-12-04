# nse

Computation of numerical standard errors in R

`nse` ([Ardia and Bluteau, 2017](https://doi.org/10.21105/joss.00172)) is an R package for computing the numerical standard error (NSE), an estimate 
of the standard deviation of a simulation result, if the simulation experiment were to be repeated 
many times. The package provides a set of wrappers around several R packages, which give access to 
more than thirty NSE estimators, including batch means estimators, initial sequence 
estimators, spectrum at zero estimators, heteroskedasticity and autocorrelation 
consistent (HAC) kernel estimators and bootstrap estimators. See [Ardia and Bluteau (2017)](https://doi.org/10.21105/joss.00172) for details. The full set of methods available in `nse` is summarized in [Ardia et al. (2018)](https://doi.org/10.1515/jtse-2017-0011) together with several examples of applications in econometrics and finance.

The latest stable version of `nse` is available at [https://cran.r-project.org/package=nse](https://cran.r-project.org/package=nse).

The latest development version of `nse` is available at [https://github.com/keblu/nse](https://github.com/keblu/nse).

## Please cite the package in publications!

By using `nse` you agree to the following rules: 

1) You must cite [Ardia et al. (2018)](https://doi.org/10.1515/jtse-2017-0011) in working papers and published papers that use `nse`.
2) You must place the following URL in a footnote to help others find `nse`: [https://CRAN.R-project.org/package=nse](https://CRAN.R-project.org/package=nse). 
3) You assume all risk for the use of `nse`.

Ardia, D., Bluteau, K., Hoogerheide, L.F. (2018).      
Methods for computing numerical standard errors: Review and application to Value-at-Risk estimation.        
_Journal of Time Series Econometrics_ **10**(2) pp 1-9.          
[https://doi.org/10.1515/jtse-2017-0011](https://doi.org/10.1515/jtse-2017-0011)
[https://doi.org/10.2139/ssrn.2741587](https://doi.org/10.2139/ssrn.2741587) 

Ardia, D., Bluteau, K. (2017).      
nse: Computation of numerical standard errors in R.       
_Journal of Open Source Software_ **10**(2).      
[https://doi.org/10.21105/joss.00172](https://doi.org/10.21105/joss.00172)  