
# plot3logit 3.1.4


## Minor changes

* Included paper published on JSS as a pdf vignette.
* Revised version of package dependencies.
* Revised and integrated the help.





# plot3logit 3.1.3


## Minor changes

* Added references and citations to the JSS article about the package.





# plot3logit 3.1.2


## Typos and other marginal changes

* Revision of help and references.





# plot3logit 3.1.1


## Major changes

* Solved a bug on `add_confregions()` (class of returned objects).





# plot3logit 3.1.0


## Major changes

* Implemented the syntax based on named numeric for argument `delta` of
  `field3logit()`.
* Solved a bug on method dispatch of generic `autoplot` for class
  `multifield3logit`.



## Minor changes

* Revised and extended the vignette `plot3logit-overview`.
* Corrected a bug on argument `label` of `field3logit()` and added tests.
* Corrected a bug on method `label<-` and added tests.
* Provided statistical details on the computation of confidence regions in the
  help of `add_confregions()`.



## Typos and other marginal changes

* Corrected file `CITATION` and list of authors of the vignette
  `plot3logit-overview`.
* Syntactical revision of tests.





# plot3logit 3.0.0


## Major changes

* Changed dependencies of the package both in section "Imports" (added
  `stringr`) and "Suggests" (added `ordinal` and `testthat`).
* Implemented syntax for factor covariates based on delimiters `<<...>>`.
* Implemented the generic `export3logit()` and several S3 methods for reading
  output from fitted models.
* Revised the arguments of `field3logit()`: arguments `alpha` and `vcov` are now
  deprecated and does not work any more.
* Argument `model` of `field3logit()` cannot accept matrices any more. Direct
  model estimates can be passed to `field3logit()` by means of a list.
* Now `field3logit()` can read also trinomial models fitted through function
  `vglm` of package `VGAM`, and functions `clm` and `clm2` of package `ordinal`
  (included amongst suggested packages).



## Minor changes

* Introduced class `model3logit` as output of `export3logit()`.
* Renamed component `lab` of `field3logit` and `multifield3logit` objects to
  `levels`.
* Redefined classes and inheritance of `field3logit` and `multifield3logit`.
  objects.
* Revised `coef` method for `field3logit` objects of ordinal models.
* Redefined the output of `plot.multifield3logit` and remove the legend.



## Typos and other marginal changes

* Revised the help and the examples.





# plot3logit 2.2.0


## Major changes

* Now `plot3logit` depends on R v. 3.5 or later.
* Now `field3logit` can read categorical trinomial models fitted through
  function `vgam` of package `VGAM` (included amongst suggested packages).
* `list` objects can be passed to argument `delta` of `field3logit` for
  computing multiple fields (a `multifield3logit` object is returned).
* Argument `ncurves` of function `field3logit` has been renamed to `nstreams`.
  No backward compatibility has been implemented.
* Point estimates are passed to argument `model` of `field3logit`, matrix
  should have an attribute with the values the dependent variable may take.
  The attribute has been renamed from `labs` to `levels`. No backward
  compatibility has been implemented.
* Added package `generics` amongst dependencies in section `Imports`.



## Minor changes

* Added dataset `USvote2016`.
* Corrected bug on reading ordinal models from matrix.
* Added S3 methods of generics `tidy` for class `field3logit`.
* Added S3 methods of generics `as.data.frame`, `as_tibble` and `tidy` for
  class `multifield3logit`.
* Made `autoplot` the S3 method of generic `autoplot` for `field3logit` objects
* Added S3 method of generic `label` for class `field3logit`.
* Added S3 method of generic `label` for class `multifield3logit`.
* Added S3 method of generics `[` and `[<-` for `multifield3logit` class.
* Added the generic `labels<-` and the S3 methods for `field3logit` and
  `multifield3logit` class.



## Typos and other marginal changes

* Removed names from components of `multifield3logit` objects.





# plot3logit 2.1.0


## Major changes

* Major bug corrected from version 2.0.0.





# plot3logit 2.0.0


## Major changes

* Added functions for computing the confidence regions of the covariate effects
  of categorical trilogit models and function for representing confidence
  regions both in standard an gg graphics.
* Changed dependencies of the package both in section "Depends" (updated the
  version of ggtern) and in section "Imports" (added `dplyr`, `forcats`,
  `grDevices`, `lifecycle`, `purrr`, `Rdpack`, `tibble`, `tidyr`, `tidyselect`
  and removed `reshape2`).
* Added the `plot` function for `multifield3logit` in standard graphics.
* Updated the functions for reading the output of `mlogit` according to the
  upcoming new version of the package. The package `plot3logit` handles the
  output of both the current and the upcoming version of `mlogit`.
* Completely redefined the output of methods `as.data.frame` and `fortify` for
  objects of class `field3logit` and `multifield3logit`.
* Function `plot3logit` is now deprecated and not updated.
* Partially redefined function `stat_3logit` and added function
  `stat_field3logit` which works like the previous version of `stat_3logit`.
  


## Minor changes

* Added methods `coef` and `vcov` for objects of class `field3logit` and
  `multifield3logit`.



## Typos and other marginal changes

* Added zero element (NULL) in the sum of `field3logit` and `multifield3logit`.
* Added attribute `ordinal` to S3 class `field3logit` and updated the S3 method
 `print`.





# plot3logit 1.0.2


## Major changes

* Vignette `plot3logit-overview` has been added to the package.
* Updated dependencies by including package "ellipse" amongst imported packages.


## Minor changes

* The level `Long` of variable `irregulariry` of dataset `cross_1year` has
  been changed to `High`.


## Typos and other marginal changes

* Updated references to Santi, Dickson and Espa (2019).





# plot3logit 1.0.1


## Major changes

* Bug solved for `as.data.frame.field3logit` when a zero-coefficient model is
  passed to field3logit. The previous version was not able to return a
  data.frame correctly built. Functions `stat_3logit`, and `TernaryField` have
  been adapted so as to draw points instead of arrows.


## Minor changes

* Interface of `fortify` methods has been redefined by including argument
  `data`, so that now all `fortify` methods of the package are compliant with
  the interface of the generic.

* `field3logit` now can read categories labels of the dependent variable from
  attribute `labs` when argument `model` is a `matrix`. Example in the help of
  package presentation has been updated.


## Typos and other marginal changes

* `CITATION` file has been amended: the title of the package and other typos
  have been corrected.

* Revision of the file `README.md`.





# plot3logit 1.0.0

First release of the package `plot3logit` on CRAN!
