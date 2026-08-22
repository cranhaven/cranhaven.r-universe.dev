# GLMMcosinor 0.2.1

* Fixed issues #14 and #15 relating to handling model formulas without 
  `amp_acro()` components.

* Update import from glmmTMB which previously failed with released version.

* Perform the glmmTMB's `.onLoad()` when GLMMcosinor is loaded by including an
  `@importFrom` glmmTMB in pkg documentation.

* Plot labels now correspond to the name of the group(s), and they are more 
  concise.  
  
* Component labels have been removed from `polar_plot()` when there is only 
  one component in the model.

# GLMMcosinor 0.2.0

* Successful peer review from rOpenSci!

* Create a `sigma()` function which gets the dispersion parameter from a given
  `cglmm` model (by calling `glmmTMB::sigma()`).

* Improve introduction to cosinor modeling in the getting-started vignette.

* Fix small typos in vignettes/docs and make plot legend title consistent across
`autoplot()` and `polar_plot()`.

* Rename functions to be `snake_case` and all S3 class names to `camelCase`.

# GLMMcosinor 0.1.0

* First development version of `{GLMMcosinor}` for submission to rOpenSci.

* Includes functions for fitting a cosinor model, similarly to the {cosinor}
R package but using the `{glmmTMB}` modeling framework to allow more 
flexibility in terms of fitting **generalized** linear **mixed** cosinor models.
