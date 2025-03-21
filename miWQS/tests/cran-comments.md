##Resubmissions (Current Version)
#miWQS 0.0.9
*  Wrote package names, software names and API names in single quotes (e.g. 'miWQS') in Description, as requested.
 * Removed default of filename.  Changed where plots are saved in example from the working directory to temp.dir()   so that  functions do not write by default or in your examples/vignettes/tests in the user's home filespace, as requested. 
 * Uncommented  example code, as requested.
 * Successfully passed windows check. 

#miWQS 0.0.8
* Reworked plot.wqs() function by using ggplot2 instead of base plotting in R. 
* Successfully passed windows check. 

#miWQS 0.0.7 
* Fixed bug of installing packages in examples as requested. 
* Successfully passed windows check. 

#miWQS 0.0.5 
 *Shortened example in estimate.wqs() as requested. 
 *Successfully passed windows check.
  
# miWQS 0.0.4  
  *Fixed bad URLS as requested in the NEWS file. 
  *Successfully passed windows check.
  
# miWQS 0.0.3
*Made comments in `impute.Lubin()` easier to read.
*Removed "\dontrun" for all examples as requested. 
*Merged `quantile.fn()` and `quantile.bdl.fn()` to `make.quantile.matrix()` so that all examples can be run. Had to adjust `estimate.wqs()`to reflect the change.  
*Placed references in form #(Author(Year) <doi:10.prefix/suffix>) #  in the description field of DESCRIPTION file, as requested. 

# miWQS 0.0.2
*Removed non-standard files. 
*Rewrote the comments in `quantile.fn()` example 
*Modified printed messages to screen to be consistent in impute.univariate.bayesian, `estimate.wqs()`,
and pool.mi.

# miWQS 0.0.1
*Minor updates; made sure examples and help files were correct. 

## Test environments
* local OS X install, R 3.5.1
* ubuntu 12.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

Status: OK
R CMD check results
0 errors | 0 warnings | 0 notes
 

* This is a new release. 

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

 
* FAILURE SUMMARY
There are no errors or warnings to report.

