# approxOT

## Changes in Version 1.1.1

### Minor Improvements and Bug fixes
* Minor bug fix to update a few Eigen C++ functions (Thank you Andrew Johnson!) 
* Removing older functions that are no longer used to speed up build
* No changes to user interface

## Changes in Version 1.1

* Added a `NEWS.md` file to track changes to the package.
* Added a `as.matrix` function to convert transportation plans to matrices.
* Added a `as.transport.plan` to convert from transportation matrix to transportation plan.
* Added classes to support these two functions.
* Added support for log domain calculations for the sinkhorn method. This can be accessed with "sinkhorn_log" as the method.
* Fixed typo in C files incorrectly referencing Rcpp::stop
* updated DLL registrations in Namespace
