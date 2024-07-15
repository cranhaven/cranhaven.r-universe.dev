**Graphical and numerical checks for mode-finding routines**


*Martin Lysy*

*March 14, 2018*

---

*Description:*

Tools for checking that the output of an optimization algorithm is indeed at a local mode of the objective function.  This is accomplished graphically by calculating all one-dimensional "projection plots" of the objective function, i.e., varying each input variable one at a time with all other elements of the potential solution being fixed.  The numerical values in these plots can be readily extracted for the purpose of automated and systematic unit-testing of optimization routines.

*Installation:*

Install the **R** [**`devtools`**](https://CRAN.R-project.org/package=devtools) package and run
```{r}
devtools::install_github("mlysy/optimCheck")
```

*Usage:*

A quick tutorial is provided by the package [vignette](http://htmlpreview.github.com/?https://github.com/mlysy/optimCheck/master/inst/doc/optimCheck-quicktut.html).
