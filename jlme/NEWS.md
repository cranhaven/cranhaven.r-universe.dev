# jlme (development version)

# jlme 0.4.1

* Bump `{JuliaConnectoR}` dependency to improve discovery of Julia executable.

# jlme 0.4.0

* New argument `jlmer(fit = TRUE)`. If `FALSE`, returns the unfit model object.

* New function `jl_dict()` to construct a Dictionary object using `list()` syntax.

* Experimental support for `MixedModels.profile()` via `profilelikelihood()` with a `tidy()` method

* Experimental implementation of an interface to setting OptSummary values via `jlmer(optsum = list(...))`.

* New model helper function `likelihoodratiotest()` for conducting likelihood-ratio tests on nested models

# jlme 0.3.0

* Better detection of Julia executable

* New function `jl()` to evaluate Julia expressions from string and return Julia objects, with options for interpolation of R objects via `...`.

* New functions `jl_get()` and `jl_put()`, wrappers around `JuliaConnectoR::juliaGet()` and `JuliaConnectoR::juliaPut()`.

* `parametricbootstrap()` gains a `optsum_overrides` argument which accepts a list of values to override in the model [OptSummary](https://juliastats.org/MixedModels.jl/stable/api/#MixedModels.OptSummary).

* `parametricbootstrap()` now prints progress to the console.

# jlme 0.2.0

* Experimental support for `MixedModels.parametricbootstrap()` via `parametricbootstrap()` with a `tidy()` method.

* `jlme_setup()` gains an `add` argument which accepts a character vector of additional Julia libraries to install and load on initializing the session.

* Some common workflows in MixedModels.jl have been re-exported as functions: `propertynames()`, `issingular()`

# jlme 0.1.0

* CRAN release
